library(tidyverse)
library(arrow)
# clean records data
records <- arrow::read_feather("Clean_Data/internment_camps.feather")

# basically stats by subregion 
stats_by_subregion <- records %>%
  filter(
    residence_state %in% c("California", "Washington", "Oregon"),
    ! residence_area %in% c("B13","California", "Washington")
    ) %>%
  group_by(residence_state, residence_area) %>%
  summarise(
    n = n()
  ) %>%
  rename(
    "state" = "residence_state",
    "subregion" = "residence_area"
    )

