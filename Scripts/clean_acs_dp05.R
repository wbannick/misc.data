library(tidyverse)
library(willbprocessed)

# maybe we put this in a package if we see much of this in future ...
acs <- willbprocessed::read_data_directory("Data/ACS") %>%
  # drop explanations
  filter(GEO_ID != "id") %>%
  transmute(
    # yaaa not real generalizeable
    state = "CA",
    city = str_extract(NAME, ".+(?= city)"),
    # so these don't include Hispanic as a race but report data does.
    # the approach I am using is if Black, Black, elseif hispanic, elseif
    # only one race, that race
    Black = DP05_0033E, 
    Hispanic = as.integer(DP05_0066E) -
      (as.integer(DP05_0033E) - as.integer(DP05_0073E)),
    White = DP05_0072E,
    Asian = DP05_0075E,
    Native = DP05_0074E,
    `Pacific Islander` = DP05_0076E,
    Other = as.integer(DP05_0077E) + 
      as.integer(DP05_0078E) 
  ) %>%
  mutate(
    # all race categories are capital
    across(matches("^[A-Z]", ignore.case = F), as.integer)
  ) %>%
  #lets make it long
  pivot_longer(
    cols = -all_of(c("state", "city")),
    names_to = "race",
    values_to = "n"
    ) %>%
  mutate(
    #for opp data
    race5 = case_when(
      race %in% c("Black", "Hispanic", "White") ~ race,
      race %in% c("Asian", "Pacific Islander") ~ "AAPI",
      T ~ "Other"
    ) %>%
      factor(levels = c("AAPI", "Black", "Hispanic", "White", "Other"))
  ) %>%
  # I reckon these will be helpful for looking at data quickly
  group_by(city, race, race5) %>%
  #first population
  summarise(population = sum(n)) %>%
  group_by(city) %>%
  #then percent by race
  mutate(
    percent = (population/sum(population)) %>% as_percent(2), 
  )

  

arrow::write_feather(acs, "Clean_Data/acs_bay_cities14.feather")
