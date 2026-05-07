---
title: "FEMA Data Doc"
output: html_notebook
---

## Question (Impact): Is there an emerging pattern of increasing delays for federal disaster response?

## Data Sets: Disaster Declarations Summaries v2: <https://drive.google.com/file/d/1YhwtLE-e428HYR-CLruthPc3VHoWb2cg/view?usp=sharing>

## Methodology

I used FEMA's Disaster Declarations Summaries v2, a publicly available dataset spanning the Eisenhower administration to the present, where each row represents a county-level disaster designation. The variable days_to_declaration measures the number of calendar days between incidentBeginDate and declarationDate using difftime() in R, restricted to Major Disaster Declarations only (declarationType == "DR") as these usually indicate more broad federal assistance.

Because each row is a county designation rather than a unique disaster, the data needed to be cleaned. A check of ~3,000 state-incident combinations found only 13 with variation in declaration dates within a single state which were nearly all attributable to tribal nations receiving separate, typically later, declarations. My analysis groups at the state level but keeps county-level rows to report minimum, median, and maximum lag in declaration per state rather than reducing to a single #. Median is used as the primary statistic over mean to try and avoid a single outlier county disproportionately skewing state-wide data.

Hurricane Helene is isolated by incidentId rather than name string, as a string search for "helene" returns rows from an older unrelated storm since FEMA reuses storm names. I chose Hurricanes Camille, Floyd, Frances, Ivan, Jeanne, Harvey, Ida, and Matthew as comparable storms because of their similar rapid intensification and extreme rainfall. I tried making both a median and a simple unweighted mean chart to compare.  A weighted average by population or damage estimates would be more rigorous but would probably require joining an additional dataset?

I also tried to filter for those reservations to analyze that more closely. One key limitation throughout is that the lag clock starts at incident begin, but FEMA cannot declare until a state governor formally requests it, meaning some delay may reflect state-level decisions rather than federal response time.

```{r setup}
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)
```

```{r load}
df_FEMA_OG <- read.csv('FEMA_disaster_declarations.csv')
```

```{r clean-dates}
df_FEMA_clean <- df_FEMA_OG %>%
  mutate(
    declarationDate   = as_date(declarationDate),
    incidentBeginDate = as_date(incidentBeginDate),
    incidentEndDate   = as_date(incidentEndDate)
  )
```

---

## Checking incidentId vs. string filtering (Professor's note)

Filtering by name string catches an older storm also named Helene. Used `incidentId`.

```{r helene-id-vs-string}
helene_by_id <- df_FEMA_OG %>%
  filter(incidentId == '2024092301')

helene_by_string <- df_FEMA_OG %>%
  filter(grepl("helene", declarationTitle, ignore.case = TRUE))

#since helene by string has more, let's see what it has that's not in the list filtered by ID number

helene_missing_in_id <- helene_by_string %>%
  filter(!id %in% helene_by_id$id)

## and vice versa just in case 

helene_missing_in_string <- helene_by_id %>%
  filter(!id %in% helene_by_string$id)


#filtering using the string includes rows from another storm named helene before!!

cat("Rows caught by string but not ID (older storm):", nrow(helene_missing_in_id), "\n")
cat("Rows caught by ID but not string:", nrow(helene_missing_in_string), "\n")
```

---

## Days to Declaration — All DR Declarations

```{r all-dr}
all_DR_data <- df_FEMA_clean %>%
  filter(declarationType == 'DR') %>%
  mutate(days_to_declaration = as.numeric(difftime(declarationDate, incidentBeginDate, units = "days")))
```

```{r comparable-storms-naming}

comparable_names <- c("camille", "floyd", "frances", "ivan", "jeanne",
                      "harvey", "ida", "matthew", "helene")

```

```{r all-dr-comparable}
DR_data_comparable <- all_DR_data %>%
  filter(incidentType %in% c("Hurricane", "Tropical Storm")) %>%
  filter(str_detect(str_to_lower(declarationTitle),
                    paste(comparable_names, collapse = "|"))) %>%
  distinct(declarationTitle, incidentId, incidentBeginDate) %>%
  arrange(incidentBeginDate)

DR_data_comparable

```

```{r dr-states-incidents}
# check whether declaration dates vary within a state for a given incident
# If count == 1 for all rows, every county in that state-incident got the same date
# only 13 out of ~3,000 state-incident combos have variation

DR_states_incidents <- all_DR_data %>%
  select(femaDeclarationString, state, incidentId, incidentBeginDate, declarationDate) %>%
  unique() %>%
  group_by(state, incidentId) %>%
  summarise(count = n(), .groups = "drop")

# View the 13 exceptions
DR_states_incidents %>%
  filter(count > 1) %>%
  arrange(desc(count))
```

```{r check-exceptions-tribal}
# nearly all exceptions appear to be tribal declarations with a
# later date.

exception_ids <- DR_states_incidents %>%
  filter(count > 1) %>%
  pull(incidentId)

all_DR_data %>%
  filter(incidentId %in% exception_ids) %>%
  select(femaDeclarationString, state, incidentId, designatedArea,
         incidentBeginDate, declarationDate, days_to_declaration, tribalRequest) %>%
  unique() %>%
  arrange(incidentId, declarationDate)
```

Since within-state date variation is minimal and largely driven by tribal declarations,
I grouped at the state level for the main analysis.

```{r dr-state-level}
# One row per state-incident: take the earliest declaration date within each group
# (tribal declarations tend to be later and are a separate phenomenon)

DR_state_level <- all_DR_data %>%
  select(femaDeclarationString, state, incidentId, incidentType,
         declarationTitle, incidentBeginDate, declarationDate,
         days_to_declaration, tribalRequest, fyDeclared) %>%
  unique() %>%
  group_by(state, incidentId) %>%
  slice_min(declarationDate, n = 1, with_ties = FALSE) %>%
  ungroup()
```

---

## Hurricane Helene

```{r helene-dr}
helene_data_DR <- df_FEMA_clean %>%
  filter(declarationType == 'DR') %>%
  filter(incidentId == '2024092301') %>%
  mutate(days_to_declaration = as.numeric(difftime(declarationDate, incidentBeginDate, units = "days")))
```

```{r helene-state-summary}
helene_state_summary <- helene_data_DR %>%
  group_by(state) %>%
  summarise(
    n_counties = n(),
    min_lag    = min(days_to_declaration),
    median_lag = median(days_to_declaration),
    max_lag    = max(days_to_declaration),
    .groups    = "drop"
  ) %>%
  arrange(median_lag)

helene_state_summary
```

```{r helene-plot}
# I heavily used the help of Claude here to help me chart what I found visually. I think for KY and WV being a lot later that might be because of delayed effects? Or maybe that's something to look further into.
# bar height shows median lag in response. States where min and max are far apart
# probably need closer inspection where one county may have had an unusually delayed declaration
helene_state_summary %>%  
ggplot(aes(x = reorder(state, -median_lag), y = median_lag, fill = median_lag)) +
  geom_col() +
  geom_text(aes(label = median_lag), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_fill_gradient(low = "steelblue", high = "firebrick") +
  labs(
    title    = "Hurricane Helene: Median Days to DR Declaration by State",
    subtitle = "Incident began 2024-09-23; bar shows median, see helene_state_summary for min/max",
    x        = NULL,
    y        = "Median Days to Declaration"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
```

---

## Comparable Storms (grouped by incidentId)

Rather than filtering by name string, I grouped all DR declarations by `incidentId`
and then label the storms of interest.

```{r comparable-storms}
#trying to include storms to compare any delays via helene to
comparable_ids <- c(
  "69034", "69035", "69043",       # Camille (1969)
  "1999091407",                     # Floyd (1999)
  "8091000001",                     # Frances/Georges (1998)
  "2004082901",                     # Frances (2004)
  "2004090502",                     # Ivan (2004)
  "2004091501",                     # Jeanne (2004)
  "2009110901",                     # Ida (2009)
  "2016092802",                     # Matthew (2016)
  "2017082301",                     # Harvey (2017)
  "2021082601",                     # Ida (2021)
  "2022092201",               # Ian - Seminole Tribe (2022)
  "2023082403",                     # Idalia (2023)
  "2024092301"                      # Helene (2024)
)

comparable_storms <- all_DR_data %>%
  filter(incidentId %in% comparable_ids) %>%
  filter(days_to_declaration >= 0) %>%
  mutate(
    storm_label = case_when(
      incidentId %in% c("69034", "69035", "69043") ~ "Camille (1969)",
      incidentId == "1999091407" ~ "Floyd (1999)",
      incidentId == "8091000001" ~ "Frances/Georges (1998)",
      incidentId == "2004082901" ~ "Frances (2004)",
      incidentId == "2004090502" ~ "Ivan (2004)",
      incidentId == "2004091501" ~ "Jeanne (2004)",
      incidentId == "2009110901" ~ "Ida (2009)",
      incidentId == "2016092802" ~ "Matthew (2016)",
      incidentId == "2017082301" ~ "Harvey (2017)",
      incidentId == "2021082601" ~ "Ida (2021)",
      incidentId == "2022092201" ~ "Ian - Seminole (2022)",
      incidentId == "2023082403" ~ "Idalia (2023)",
      incidentId == "2024092301" ~ "Helene (2024)"
    )
  ) %>%
  distinct(incidentId, designatedArea, state, .keep_all = TRUE)%>%
  group_by(storm_label) %>%
  summarise(
    n_counties = n(),
    #to count number of counties & filter for unique instances with distinct to prevent duplicates
    min_lag    = min(days_to_declaration),
    median_lag = median(days_to_declaration),
    max_lag    = max(days_to_declaration),
    .groups    = "drop"
  )

comparable_storms
```

```{r comparable-plot}
#plotting those comparable storms
comparable_storms %>%
  mutate(year = as.numeric(str_extract(storm_label, "\\d{4}"))) %>%
  ggplot(aes(x = reorder(storm_label, year), y = median_lag)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = round(median_lag, 0)), hjust = -0.2, size = 3.5) +
  coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Median Declaration Lag for Storms Comparable to Helene",
    subtitle = "DR declarations only; median across all designated counties",
    x        = NULL,
    y        = "Median Days from Incident Begin to Declaration"
  ) +
  theme_minimal()
```

```{r comparable-unweighted}
#i wanted to include something that didn't take into account the number of counties in a state just in case it was useful to compare
comparable_storms_unweighted <- all_DR_data %>%
  filter(incidentId %in% comparable_ids) %>%
  filter(days_to_declaration >= 0) %>%
  mutate(
    storm_label = case_when(
      incidentId %in% c("69034", "69035", "69043") ~ "Camille (1969)",
      incidentId == "1999091407" ~ "Floyd (1999)",
      incidentId == "8091000001" ~ "Frances/Georges (1998)",
      incidentId == "2004082901" ~ "Frances (2004)",
      incidentId == "2004090502" ~ "Ivan (2004)",
      incidentId == "2004091501" ~ "Jeanne (2004)",
      incidentId == "2009110901" ~ "Ida (2009)",
      incidentId == "2016092802" ~ "Matthew (2016)",
      incidentId == "2017082301" ~ "Harvey (2017)",
      incidentId == "2021082601" ~ "Ida (2021)",
      incidentId == "2022092201" ~ "Ian (2022)",
      incidentId == "2023082403" ~ "Idalia (2023)",
      incidentId == "2024092301" ~ "Helene (2024)"
    )
  ) %>%
  filter(!is.na(storm_label)) %>%
  group_by(storm_label) %>%
  summarise(
    n_counties = n(),
    mean_lag   = round(mean(days_to_declaration), 1),
    .groups    = "drop"
  )
```

```{r comparable-plot-unweighted}
comparable_storms_unweighted %>%
  mutate(year = as.numeric(str_extract(storm_label, "\\d{4}"))) %>%
  ggplot(aes(x = reorder(storm_label, year), y = mean_lag)) +
  geom_col(fill = "firebrick", alpha = 0.8) +
  geom_text(aes(label = mean_lag), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Unweighted Mean Declaration Lag for Storms Comparable to Helene",
    subtitle = "DR declarations only; simple mean across all designated counties — compare with median chart above",
    x        = NULL,
    y        = "Mean Days from Incident Begin to Declaration"
  ) +
  theme_minimal()
```
#Having trouble figuring out why one is showing up as NA. I used code below to try and filter for what was causing the problem and I guess Ian is Hurricane Ian + Hurricane Ian Seminole Tribe of Florida, both under the same incidentID.

```{r na}
df_findna <- all_DR_data %>%
  filter(incidentId %in% comparable_ids) %>%
  filter(days_to_declaration >= 0) %>%
  mutate(
    storm_label = case_when(
      incidentId %in% c("69034", "69035", "69043") ~ "Camille (1969)",
      incidentId == "1999091407" ~ "Floyd (1999)",
      incidentId == "8091000001" ~ "Frances/Georges (1998)",
      incidentId == "2004082901" ~ "Frances (2004)",
      incidentId == "2004090502" ~ "Ivan (2004)",
      incidentId == "2004091501" ~ "Jeanne (2004)",
      incidentId == "2009110901" ~ "Ida (2009)",
      incidentId == "2016092802" ~ "Matthew (2016)",
      incidentId == "2017082301" ~ "Harvey (2017)",
      incidentId == "2021082601" ~ "Ida (2021)",
      incidentId == "2023082403" ~ "Idalia (2023)",
      incidentId == "2024092301" ~ "Helene (2024)"
    )
  ) %>%
  filter(is.na(storm_label)) %>%
  distinct(incidentId, declarationTitle)

```

```{r tribal}
tribal_DR <- all_DR_data %>%
  filter(
  tribalRequest == 1 |
  str_detect(str_to_lower(designatedArea), "indian nation|indian reservation|indian tribe|band of cherokee|band of indian|pueblo|tribe of")
)%>%
  filter(
    str_detect(str_to_lower(declarationTitle),
               paste(comparable_names, collapse = "|")) | incidentId == "2024092301"
  ) %>%
  group_by(state, designatedArea, declarationTitle, fyDeclared) %>%
  summarise(
    n_declarations = n(),
    min_lag        = min(days_to_declaration),
    median_lag     = median(days_to_declaration),
    max_lag        = max(days_to_declaration),
    .groups        = "drop"
  ) %>%
  arrange(desc(median_lag))

tribal_DR
```


```{r tribal-plot}
tribal_DR %>%
  mutate(area_label = paste0(designatedArea, " (", declarationTitle, ")")) %>%
  ggplot(aes(x = reorder(area_label, median_lag), y = median_lag, fill = median_lag)) +
  geom_col() +
  geom_text(aes(label = median_lag), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_fill_gradient(low = "steelblue", high = "firebrick") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Declaration Lag for Tribal Areas",
    subtitle = "Helene + comparable storms",
    x        = NULL,
    y        = "Median Days to Declaration"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 7))
```

```{r}
# same as what you had, but adding in tribal to the group_by
DR_state_tribal_level <- all_DR_data %>%
  select(femaDeclarationString, state, incidentId, incidentType,
         declarationTitle, incidentBeginDate, declarationDate,
         days_to_declaration, tribalRequest, fyDeclared) %>%
  mutate(tribalRequest = ifelse(tribalRequest == 1, 'Days - Tribal', 'Days - Not Tribal')) %>%
  unique() %>%
  group_by(state, incidentId, tribalRequest) %>%
  slice_min(declarationDate, n = 1, with_ties = FALSE) %>%
  ungroup()

# finding when more than one declaration for a given state and incident 
same_state_tribal <- DR_state_tribal_level %>%
  select(femaDeclarationString, state, incidentId, tribalRequest) %>%
  group_by(incidentId, state) %>%
  mutate(number_in_state = n())

keep <- same_state_tribal %>%
  filter(number_in_state > 1)

# filtering for just those id'ed in "keep" 
# this one shows names of storms
DR_state_tribal_level_same_state_Weather <- DR_state_tribal_level %>%
  filter(femaDeclarationString %in% keep$femaDeclarationString) %>%
  # took covid related out - though that looks like a potential story too 
  filter(incidentType != "Biological") 

## remove columns that make pivot difficult 
## pivot to better compare data 
DR_state_tribal_level_same_state_Weather_difference <- DR_state_tribal_level_same_state_Weather %>%
  select(state, incidentId, incidentType, days_to_declaration, tribalRequest) %>%
  pivot_wider(names_from = tribalRequest, values_from = days_to_declaration)


View(DR_state_tribal_level_same_state_Weather_difference)

library(ggplot2)

# kind of a weird plot but.. 
ggplot(DR_state_tribal_level_same_state_Weather) + 
  geom_point(aes(x = incidentBeginDate, y = days_to_declaration, group = tribalRequest, col = tribalRequest), alpha = 0.7)
```

```{r}


```


```{r write-csv}
write.csv(helene_state_summary, "helene_state_summary.csv", row.names = FALSE)
write.csv(comparable_storms, "comparable_storms.csv", row.names = FALSE)
write.csv(comparable_storms_unweighted, "comparable_storms_unweighted.csv", row.names = FALSE)
write.csv(tribal_DR, "tribal_DR.csv", row.names = FALSE)
write.csv(DR_state_tribal_level_same_state_Weather, "DR_state_tribal_level_same_state_Weather.csv", row.names = FALSE)
```

```{r overall-trend}
#overall trend df + plot from analysis feedback

storms <- DR_state_level|>
  filter(incidentType %in% c("Tropical Storm", "Hurricane"))

ggplot(storms) +
  geom_point(aes(x = incidentBeginDate, y = days_to_declaration)) +
  geom_smooth(aes(x = incidentBeginDate, y = days_to_declaration))

```
```{r plot}
#plotting overall trends for trop. storms and hurricanes

storms <- DR_state_level %>%
  filter(incidentType %in% c("Tropical Storm", "Hurricane"))

ggplot(storms, aes(x = incidentBeginDate, y = days_to_declaration)) +
  geom_point(color = "steelblue", alpha = 0.4, size = 1.5) +
  geom_smooth(color = "black", se = TRUE) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(
    title    = "Hurricane & Tropical Storm Declaration Lag Over Time",
    subtitle = "All major disaster declarations (DR); one point per state-level declaration",
    x        = NULL,
    y        = "Days from Incident Begin to Declaration"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

write.csv(storms, "storms.csv", row.names = FALSE)

```

```{r storms-decades}
#Wanted to get a sense of what this looks like breaking down by decade and then by presidential administration

# Decade summary
decade_summary <- storms %>%
  mutate(
    decade = paste0(floor(year(incidentBeginDate) / 10) * 10, "s")
  ) %>%
  group_by(decade) %>%
  summarise(
    avg_days = round(mean(days_to_declaration, na.rm = TRUE), 1),
    median_days = round(median(days_to_declaration, na.rm = TRUE), 1),
    n_declarations = n()
  ) %>%
  arrange(decade)

# Administration summary
admin_order <- c(
  "Eisenhower (R)", "Kennedy (D)", "Johnson (D)", "Nixon (R)",
  "Ford (R)", "Carter (D)", "Reagan (R)", "H.W. Bush (R)",
  "Clinton (D)", "W. Bush (R)", "Obama (D)", "Trump I (R)",
  "Biden (D)", "Trump II (R)"
)

admin_summary <- storms %>%
  mutate(
    administration = case_when(
      year(incidentBeginDate) >= 1953 & year(incidentBeginDate) <= 1961 ~ "Eisenhower (R)",
      year(incidentBeginDate) >= 1961 & year(incidentBeginDate) <= 1963 ~ "Kennedy (D)",
      year(incidentBeginDate) >= 1963 & year(incidentBeginDate) <= 1969 ~ "Johnson (D)",
      year(incidentBeginDate) >= 1969 & year(incidentBeginDate) <= 1974 ~ "Nixon (R)",
      year(incidentBeginDate) >= 1974 & year(incidentBeginDate) <= 1977 ~ "Ford (R)",
      year(incidentBeginDate) >= 1977 & year(incidentBeginDate) <= 1981 ~ "Carter (D)",
      year(incidentBeginDate) >= 1981 & year(incidentBeginDate) <= 1989 ~ "Reagan (R)",
      year(incidentBeginDate) >= 1989 & year(incidentBeginDate) <= 1993 ~ "H.W. Bush (R)",
      year(incidentBeginDate) >= 1993 & year(incidentBeginDate) <= 2001 ~ "Clinton (D)",
      year(incidentBeginDate) >= 2001 & year(incidentBeginDate) <= 2009 ~ "W. Bush (R)",
      year(incidentBeginDate) >= 2009 & year(incidentBeginDate) <= 2017 ~ "Obama (D)",
      year(incidentBeginDate) >= 2017 & year(incidentBeginDate) <= 2021 ~ "Trump I (R)",
      year(incidentBeginDate) >= 2021 & year(incidentBeginDate) <= 2025 ~ "Biden (D)",
      year(incidentBeginDate) >= 2025 ~ "Trump II (R)",
      TRUE ~ "Unknown"
    ),
    administration = factor(administration, levels = admin_order)
  ) %>%
  group_by(administration) %>%
  summarise(
    avg_days = round(mean(days_to_declaration, na.rm = TRUE), 1),
    median_days = round(median(days_to_declaration, na.rm = TRUE), 1),
    n_declarations = n()
  )

# Decade chart
ggplot(decade_summary, aes(x = decade)) +
  geom_col(aes(y = avg_days), fill = "steelblue", alpha = 0.7, width = 0.4,
           position = position_nudge(x = -0.22)) +
  geom_col(aes(y = median_days), fill = "firebrick", alpha = 0.7, width = 0.4,
           position = position_nudge(x = 0.22)) +
  geom_text(aes(y = avg_days, label = avg_days),
            position = position_nudge(x = -0.22), vjust = -0.4, size = 3) +
  geom_text(aes(y = median_days, label = median_days),
            position = position_nudge(x = 0.22), vjust = -0.4, size = 3) +
  labs(
    title = "Hurricane & Tropical Storm Declaration Lag by Decade",
    subtitle = "Blue = mean, Red = median | Major Disaster Declarations only",
    x = NULL,
    y = "Days to Declaration"
  ) +
  theme_minimal()

# Administration chart
ggplot(admin_summary, aes(x = administration)) +
  geom_col(aes(y = avg_days), fill = "steelblue", alpha = 0.7, width = 0.4,
           position = position_nudge(x = -0.22)) +
  geom_col(aes(y = median_days), fill = "firebrick", alpha = 0.7, width = 0.4,
           position = position_nudge(x = 0.22)) +
  geom_text(aes(y = avg_days, label = avg_days),
            position = position_nudge(x = -0.22), vjust = -0.4, size = 3) +
  geom_text(aes(y = median_days, label = median_days),
            position = position_nudge(x = 0.22), vjust = -0.4, size = 3) +
  labs(
    title = "Hurricane & Tropical Storm Declaration Lag by Administration",
    subtitle = "Blue = mean, Red = median | Major Disaster Declarations only",
    x = NULL,
    y = "Days to Declaration"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

```
