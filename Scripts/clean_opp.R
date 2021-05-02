library(tidyverse)
library(willbprocessed)
library(arrow)

# I. Clean Bay Area Stop Data
#######################################################
# reading in OPP's data for the three Bay Area Cities they have...
df <- willbprocessed::read_data_directory(
  "Data/OPP",
  read_fxn = readRDS,
  filename_as_var = TRUE
) %>%
  mutate(
    #grab city + year
    city = str_extract(filename, "(?<=ca_).+(?=_20)") %>%
      str_replace_all("_", " ") %>% str_to_title(),
    year = format(date, "%Y") %>% as.integer()
  ) %>%
  #years where we have a lot of data in each
  filter(year %in% 2013:2016) %>%
  #selecting only the cols we want + some basic recodes
  transmute(
    city, year, date,
    race = subject_race %>% str_to_title() %>%
      recode( "Asian/Pacific Islander"= "AAPI") %>%
      factor(levels = c("AAPI", "Black", "Hispanic", "White", "Other")), 
    age = subject_age,  
    gender = subject_sex %>% str_to_title(),
    type, reason_for_stop,
    arrest_made, citation_issued, warning_issued,
    search_conducted, search_basis, contraband_found,
    use_of_force_description
  )

write_feather(df, "Clean_Data/opp_bay_area.feather")

# II. Clean OPP's Bay Area ACS Data
#######################################################
# jk probs won't use
# acs <- read_csv("https://raw.githubusercontent.com/stanford-policylab/opp/master/resources/acs_2017_5_year_city_level_race_data.csv")
