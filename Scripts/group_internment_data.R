library(tidyverse)
library(arrow)
library(willbprocessed)
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
    # many more stats to add here!!
    n = n(),
    US_born = mean(I(birth_place == "United States"), na.rm = T),
    Japan_born = mean(I(birth_place == "Japan"), na.rm = T)
  ) %>%
  rename(
    "state" = "residence_state",
    "subregion" = "residence_area"
  ) %>%
  mutate(across(matches("_"), as_percent, 1))

write_feather(stats_by_subregion, "Clean_Data/internment_stats_by_subregion.feather")
