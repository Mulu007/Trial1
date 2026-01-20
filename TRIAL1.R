library("tidyverse")
library("dplyr")
library("ggplot2")
install.packages("scales")
library("scales")

enrollment <- read_csv("IPEDS Enrollment Data 2019-2024 (1).csv")
completers <- read_csv("IPEDS Completers Data 2019-2024.csv")

view(enrollment)
view(completers)

# Pivoting races in enrollment
glimpse(enrollment)

enrollment_long <- enrollment %>%
  pivot_longer(
    cols = c(
      "American Indian or Alaska Native total",
      "Asian total",
      "Black or African American total",
      "Hispanic or Latino total",
      "Native Hawaiian or Other Pacific Islander total",
      "White total",
      "Two or more races total",
      "Race/ethnicity unknown total",
      "U.S. Nonresident total"
    ),
    names_to = "Race",
    values_to = "Enrolled"
  )

view(enrollment_long)

# Pivoting races in Completers
glimpse(completers)

completers_long <- completers %>%
  pivot_longer(
    cols = c(
      "American Indian or Alaska Native total",
      "Asian total",
      "Black or African American total",
      "Hispanic or Latino total",
      "Native Hawaiian or Other Pacific Islander total",
      "White total",
      "Two or more races total",
      "Race/ethnicity unknown total",
      "U.S. Nonresident total"
    ),
    names_to = "Race",
    values_to = "Completed"
  )

view(completers_long)

# Removing the word "total" in each race value
enrollment_long <- enrollment_long %>%
  mutate(Race = str_remove(Race, " total"))

view(enrollment_long)

completers_long <- completers_long %>%
  mutate(Race = str_remove(Race, " total"))

view(completers_long)

# Completion rates visualisation
# ggplot(completers_long, aes("Year", "Grand total", color = Race)) +
#   geom_line(size = 1.2) +
#   geom_point() +
#   labs(
#     title = "Completion Rates by Race Over Time",
#     y = "Completion Rate",
#     x = "Year"
#   ) +
#   theme_minimal()

# Table joining
# demographic_gap <- enrollment_long %>%
#   left_join(
#     completers_long,
#     by = c("Year", "UNITID", "Race", "Institution (entity) name", "State abbreviation"),
#     suffix = c("_enroll", "_complete")
#   )
# 
# view(demographic_gap)

# enrollment_long %>%
#   group_by(Year, UNITID, Race, `Institution (entity) name`, `State abbreviation`) %>%
#   filter(n() > 1) %>%
#     glimpse()

# Duplicate & Error consolidation in enrollment
enrollment_long <- enrollment_long %>%
  group_by(Year, UNITID, Race, `Institution (entity) name`, `State abbreviation`) %>%
  summarise(Enrolled = sum(Enrolled, na.rm = TRUE), .groups = "drop")

view(enrollment_long)

# Duplicate & Error consolidation in  completers
completers_long <- completers_long %>%
  group_by(Year, UNITID, Race, `Institution (entity) name`, `State abbreviation`) %>%
  summarise(Completed = sum(Completed, na.rm = TRUE), .groups = "drop")

view(completers_long)

# Table joining
demographic_gap <- enrollment_long %>%
  left_join(
    completers_long,
    by = c("Year", "UNITID", "Race", "Institution (entity) name", "State abbreviation")
  )

view(demographic_gap)

# Demographic difference (gap) calculation
demographic_gap <- demographic_gap %>%
  mutate(
    Gap = Enrolled - Completed,
    CompletionRate = Completed / Enrolled * 100
  )

view(demographic_gap)

# First plot
demographic_gap %>%
  group_by(Race) %>%
  summarise(TotalGap = sum(Gap, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(Race, TotalGap), y = TotalGap, fill = Race)) +
  geom_col() +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-6, suffix = "M")
  ) +
  labs(
    title = "Demographic Differences Between Enrollment and Degree Completion",
    subtitle = "Population-weighted aggregate difference between enrolled and completed students across U.S. institutions (2019–2024)",
    caption = "Source: IPEDS Enrollment Data, 2019-2024",
    y = "Count of Students",
    x = "Race / Ethnicity"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray50"),
    plot.caption  = element_text(hjust = 1),
    axis.text.x   = element_blank()
  )

# Descriptive statistics
gap_summary <- demographic_gap %>%
  #exclusion of anomalies
  filter(gap_rate >=0 & gap_rate <= 1) %>%
  group_by(Race) %>%
  summarise(
    mean = mean(gap_rate, na.rm = TRUE),
    median = median(gap_rate, na.rm = TRUE),
    sd = sd(gap_rate, na.rm = TRUE),
    # iqr = IQR(Gap, na.rm = TRUE)
    Count = n()
  ) 
  #   %>%
  # arrange(desc(median))

view(gap_summary)

summary(demographic_gap)

# Are some demographic groups disproportionately affected, or are the gaps just large because the group is large?

demographic_gap <- demographic_gap %>%
  mutate(
    completion_rate = Completed / Enrolled,
    gap_rate = Gap / Enrolled 
  ) %>%
  # removes Inf, -Inf and NaN
  filter(is.finite(gap_rate))

view(demographic_gap)
summary(demographic_gap$gap_rate)

# demographic_gap %>%
#   ggplot(aes(gap_rate)) +
#   geom_histogram(bins = 1000) +
#   labs(title = "Distribution of Gap Rates")

# ggplot(demographic_gap, aes(x = Race, y = gap_rate)) +
#   geom_boxplot(outlier.shape = NA) +
# #  geom_jitter(width = 0.2, alpha = 0.3) +
#   coord_cartesian(ylim = c(-0.1, 1.1)) +
#   coord_flip()

# Explains and shows anamolies in my data
view(subset(demographic_gap, gap_rate < 0))

# Box-plot code
clean_gap_data <- demographic_gap %>%
  filter(gap_rate >= 0 & gap_rate <= 1)

ggplot(clean_gap_data, aes(x = Race, y = gap_rate)) +
  geom_boxplot(outlier.shape = NA, fill = "red" , alpha = 0.7) +
  # coord_cartesian(ylim = c(0, 1)) +
  # geom_point() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Gap Rate by Race",
    subtitle = "Excluding outliers where Completion > Enrollment",
    y = "Gap Rate"
  )

ggplot(demographic_gap, aes(x = Enrolled, y = Completed, color = Race)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  scale_x_log10(trans = "pseudo_log",labels = comma) +
  scale_y_log10(trans = "pseudo_log",labels = comma) +
  facet_wrap(~Race) + # Separates by race to see trends clearly
  theme_minimal() +
  labs(
    title = "Enrolled vs. Completed Counts (Log Scale)",
    subtitle = "The red dashed line represents a 100% completion rate.",
    x = "Total Enrolled (Log Scale)",
    y = "Total Completed (Log Scale)"
  )

# 1. Summarize the data by Race to get a single performance metric per group
race_summary <- demographic_gap %>%
  filter(Enrolled > 0 & Enrolled >= Completed) %>% # Filter out noise
  group_by(Race) %>%
  summarise(
    Total_Enrolled = sum(Enrolled, na.rm = TRUE),
    Total_Completed = sum(Completed, na.rm = TRUE),
    # Calculate the weighted Success Rate (opposite of Gap Rate for easier reading)
    Success_Rate = Total_Completed / Total_Enrolled
  ) %>%
  arrange(desc(Success_Rate))

# 2. Extract the Top 3 and Bottom 3 groups
extreme_groups <- bind_rows(
  head(race_summary, 3), # Top 3 highest completion
  tail(race_summary, 3)  # Bottom 3 lowest completion (highest gaps)
) %>%
  mutate(Category = ifelse(Success_Rate > median(Success_Rate), "Highest Success", "Highest Gap"))

# 3. Create a clean, simple Bar Chart
ggplot(extreme_groups, aes(x = reorder(Race, Success_Rate), y = Success_Rate, fill = Category)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  scale_fill_manual(values = c("Highest Success" = "#2ecc71", "Highest Gap" = "#e74c3c")) +
  labs(
    title = "The Demographic Extremes: Completion Success vs. Gaps",
    subtitle = "Comparing the 3 highest and 3 lowest performing racial demographics",
    x = "",
    y = "Overall Completion Rate (%)"
  )

# Top 10 states by volume
top_states <- demographic_gap %>%
  group_by(`State abbreviation`) %>%
  summarise(total = sum(Enrolled, na.rm = TRUE)) %>%
  slice_max(total, n = 10) %>%
  pull(`State abbreviation`)

# view(demographic_gap %>%
#      group_by(`State abbreviation`) %>%
#      summarise(total = sum(Enrolled, na.rm = TRUE)))

# Aggregate data for the heat map
heatmap_data <- demographic_gap %>%
  # Removal of anomalies
  filter(`State abbreviation` %in% top_states & Enrolled > 0 & Enrolled >= Completed) %>%
  group_by(Race, `State abbreviation`) %>%
  summarise(Avg_Success_Rate = sum(Completed) / sum(Enrolled)) %>%
  ungroup()

view(heatmap_data)

# Heat Map Plot
ggplot(heatmap_data, aes(x = `State abbreviation`, y = Race, fill = Avg_Success_Rate)) +
  geom_tile(color = "white") +
  scale_fill_distiller(palette = "RdYlGn", direction = 1, labels = scales::percent) +
  theme_minimal() +
  labs(
    title = "The 'Equity' Heat Map: Success Rates by Demographic & State",
   # subtitle = "Red cells indicate the largest gaps; Green cells indicate higher success.",
    x = "State",
    y = "",
    fill = "Success Rate"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
