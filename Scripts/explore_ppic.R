
library(tidyverse)
library(willbprocessed)
library(haven)

##############################################################
# I. Data Ingest
##############################################################

data_path <- "Data/PPIC/"

ppic20 <- willbprocessed::read_data_directory(data_path, purrr::map, haven::read_sav)
#variables I'm interested in
vars <- c("id", "version", "county", "gender","language", "likevote", "weight", 
          #top_issue, newsom_app, ca_track, votereg, pid3, presvote_intent20,
          "q1", "q2","q5", "q7", "q7a", "q9",
          #lost_job, reduced_pay, pay_rent, trump_app, covid_outlook, medicare
          "q18", "q19", "q21", "q25", "q32", "q34",
          #ideo5, polint, turn_intent20, age, employ, educ, hisp, race_a, race_b, race
          "q38", "q39", "q41", "d1", "d5", "d7", "d8", "qd8a_1", "qd8a_2", "d8com",
          #immig, faminc
          "d9", "d11",
          "lgdr", "d1a", "gender", 
          "language", "likevote", "weight","filename")

# sigh still different variable classes...
# df20 <- map_dfr(ppic20, function(df){
#   df %>% select(any_of(vars))
# })

#for now let's check out the most recent release
df_0620 <- ppic20[[4]] %>%
  #grabbing relevant vars
  select(all_of(vars)) %>%
  #making them factors cuz I prefer to labeled vectors
  mutate_if(
    haven::is.labelled,
    forcats::as_factor
  ) %>%
  #so I don't have to remember what number means what
  dplyr::rename(
    "top_issue" ="q1","newsom_app" = "q2", "ca_track" ="q5", "votereg" = "q7",
    "pid3"= "q7a", "presvote_intent20" = "q9", "lost_job"="q18", "reduced_pay" = "q19",
    "pay_rent" = "q21", "trump_app" = "q25","covid_outlook" = "q32","medicare" = "q34",
    "ideo5" = "q38", "polint" = "q39", "turn_intent20" = "q41" , "age" = "d1", "age6" = "d1a",
    "employ" = "d5", "educ" = "d7", "hisp" = "d8", "race_a" = "qd8a_1", "race_b" = "qd8a_2",
    "race" ="d8com", "immig" = "d9", "faminc" = "d11"
  ) 
