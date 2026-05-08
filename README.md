## Question: Is there an emerging pattern of increasing delays for federal disaster response?

## Data Sets: Disaster Declarations Summaries v2: <https://drive.google.com/file/d/1YhwtLE-e428HYR-CLruthPc3VHoWb2cg/view?usp=sharing>



## Methodology (view the prettier HTML version [here](https://rxguerose.github.io/FEMA/))

FEMA's Disaster Declarations Summaries v2, a publicly available dataset spanning the Eisenhower administration to the present, was used for this analysis, where each row represents a county-level disaster designation. The variable days_to_declaration measures the number of calendar days between incidentBeginDate and declarationDate using difftime() in R, restricted to Major Disaster Declarations only (declarationType == "DR") as these usually indicate more broad federal assistance.

Because each row is a county designation rather than a unique disaster, the data needed to be cleaned. A check of ~3,000 state-incident combinations found only 13 with variation in declaration dates within a single state, which were nearly all attributable to tribal nations receiving separate, typically later, declarations. This analysis groups at the state level and takes the earliest declaration date within each state-incident group — tribal declarations tend to come later and are treated as a separate phenomenon. County-level rows are kept to report minimum, median, and maximum lag per state rather than reducing to a single number. Median is used as the primary statistic over mean to avoid a single outlier county disproportionately skewing state-wide data.

Hurricane Helene is isolated by incidentId rather than name string, as a string search for "helene" returns rows from an older unrelated storm since FEMA reuses storm names.

For the overall trend, all DR declarations were filtered to hurricanes and tropical storms only, then days_to_declaration was plotted against incidentBeginDate with a LOESS smoothed trend line. Response times were also broken down by decade, reporting both mean and median to show how the distribution has shifted over time.

For tribal declarations, the data was filtered using both the tribalRequest flag and a string search across designatedArea for keywords like "indian nation," "indian reservation," "pueblo," and "tribe of." Tribal and non-tribal declaration lag were then compared within the same state-incident to identify delays — Florida is highlighted specifically because it provides a direct within-state comparison across multiple storms.

Two key limitations throughout are that the lag clock starts at incident begin, but FEMA cannot declare until a state governor formally requests it, meaning some delay may reflect state-level decisions rather than federal response time. Additionally, data manually entered before the 1998 launch of the National Emergency Management Information System may be subject to a small percentage of human error.
