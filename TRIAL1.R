library("tidyverse")

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
