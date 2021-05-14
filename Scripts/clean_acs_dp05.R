library(tidyverse)
library(willbprocessed)

# -----------------------------------------
# I. Reshape Function
# -----------------------------------------
state_abb <- "CA"
# maybe we put this in a package if we see much of this in future ...
# ugh actually different names each year... time to change this
reshape_acs_dp05 <- function(acs){
 acs %>%
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
}

# -----------------------------------------
# II. Cleaning 2014 ACS DP05 
# -----------------------------------------

acs14 <- willbprocessed::read_data_directory(
  "Data/ACS",
  include_files_regex = "2014") %>%
  # drop explanations
  filter(GEO_ID != "id") %>%
  # so this changes each year so not in a fxn for now
  transmute(
    state = state_abb,
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
  reshape_acs_dp05()

arrow::write_feather(acs14, "Clean_Data/acs_bay_cities14.feather")


# -----------------------------------------
# II. Cleaning 2019 ACS DP05 
# -----------------------------------------

acs19 <- willbprocessed::read_data_directory(
  "Data/ACS",
  include_files_regex = "2019") %>%
  # drop explanations
  filter(GEO_ID != "id") %>%
  # so this changes each year so not in a fxn for now
  transmute(
    state = state_abb,
    city = str_extract(NAME, ".+(?= city)"),
    # doing same strategy for race but with new names
    Black = DP05_0038E, 
    Hispanic = as.integer(DP05_0071E) -
      (as.integer(DP05_0038E) - as.integer(DP05_0078E)),
    White = DP05_0077E,
    Asian = DP05_0080E,
    Native = DP05_0079E,
    `Pacific Islander` = DP05_0081E,
    Other = as.integer(DP05_0082E) + 
      as.integer(DP05_0083E) 
  ) %>%
  reshape_acs_dp05()

arrow::write_feather(acs19, "Clean_Data/acs_bay_cities19.feather")

