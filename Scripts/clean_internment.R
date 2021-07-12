library(tidyverse)

# I chose not to include the source data because it includes PII
# I found it in the national archives
# codebook in documentation: Documentation/Internment/102.1DP.pdf
raw_data <- read_fwf("Not for Git/Data/RG210.JAPAN.WRA26.txt", 
               skip = 42,
               # there are more columns that could be used
               fwf_cols(
                 relocation_center = c(20),
                 assembly_center = c(21),
                 residence = c(22,26),
                 birth_country = c(27),
                 age_in_japan = c(37),
                 len_in_japan = c(35),
                 military = c(38),
                 gender_marstat = c(45),
                 race = c(46),
                 birth_year = c(47,48),
                 birth_place = c(49,50),
                 # SS/AR + Japanese Language school
                 reg_lang_school = c(50,51),
                 educ = c(52),
                 religion = c(54)
                 )
              )

out_data <- raw_data %>%
  transmute(
    # Residence Info
    # -----------------
    residence_state = str_extract(residence,"[0-9]{2}") %>%
      recode(
        "11" = "Washington",  "12" = "Oregon", "13" = "California", 
        "26" = "Arizona", "70" = "Hawaii", "71" = "Hawaii",
        "72" = "Hawaii", "73" = "Hawaii", "74" = "Hawaii",
        "81" = "Alaska"
      ),
    residence_state = ifelse(!str_detect(residence_state, "[A-z]"),
                             "Other", residence_state) %>%
      # order of number of people...
      factor(levels = c("California", "Washington", "Oregon",
                        "Hawaii", "Arizona", "Alaska", "Other")),
    residence_area = str_extract(residence, "^.{3}") %>%
      recode(
        "131" = "Northwestern Coastal Hills and Valleys",
        "132" = "Sierra and Northeastern Area",
        "133" = "Sacramento River Valley",
        "134" = "Central Coastal Hills and Valleys",
        "135" = "San Joaquin River Valley",
        "136" = "Santa Barbara-Ventura Area",
        "137" = "Southeastern Desert and Irrigated Valleys",
        "138" = "San Francisco-Oakland Metropolitan Counties",
        "139" = "Sacramento Metropolitan County",
        "13-" = "San Jose Metropolitan County",
        "13&" = "San Diego Metropolitan County",
        "130" = "Los Angeles Metropolitan Counties",
        "121" = "Northwestern Area",
        "122" = "Southwestern Area",
        "123" = "Eastern Wheat Area",
        "124" = "Eastern Irrigation and Grasing Area",
        "125" = "Portland Metropolitan Counties",
        "111" = "Western Slope",
        "112" = "Western Slope (Inland)",
        "113" = "Central and Northeastern Area",
        "114" = "Columbia Plateau Wheat Area",
        "115" = "Seattle-Tacoma Metropolitan Counties",
        "116" = "Spokane Metropolitan County"
      ),
    # if it still starts with a number we'll use state for now
    residence_area = case_when(
      str_detect(residence_area, "^[0-9]") ~ as.character(residence_state),
      # Group together b/c small numbers
      residence_area %in%
        c("Columbia Plateau Wheat Area", "Spokane Metropolitan County") ~ 
        "Columbia Plateau Wheat Area and Spokane Metropolitan County",
      T ~ residence_area
    ),
    # UPDATE: so I'm going to keep this more detailed residence_area as residence_area_detail
    residence_area_detail = residence_area,
    # and then do a recode that groups together some places in WA and CA with few people
    residence_area = case_when(
      residence_area %in% c(
        "Columbia Plateau Wheat Area and Spokane Metropolitan County", 
        "Central and Northeastern Area") ~ "Central and Eastern Washington",
      str_detect(residence_area, "Western Slope") ~ "Western Slope",
      residence_area %in% c("Northwestern Area", "Southwestern Area") ~ "Western Oregon",
      residence_area %in% 
        c("Eastern Irrigation and Grasing Area", "Eastern Wheat Area") ~ "Eastern Oregon",
      T ~ residence_area),
    # Camps
    # -----------------
    relocation_center = 
      recode(
        relocation_center,
        "1" = "Manzanar", "2" = "Poston", "3" = "Gila River",
        "4" = "Tule Lake", "5" = "Minidoka", "6" = "Topaz",
        "7" = "Heart Mountain", "8" = "Granada", "9" = "Rohwer",
        "0" = "Jerome"
        ),
    relocation_state = case_when(
      relocation_center %in% c("Manzanar", "Tule Lake") ~ "California",
      relocation_center %in% c("Gila River", "Poston") ~ "Arizona",
      relocation_center %in% c("Minidoka") ~ "Idaho",
      relocation_center %in% c("Topaz") ~ "Utah",
      relocation_center %in% c("Heart Mountain") ~ "Wyoming",
      relocation_center %in% c("Granada") ~ "Colorado",
      relocation_center %in% c("Rohwer", "Jerome") ~ "Arkansas"
    ),
    camp_full = paste(relocation_center, relocation_state, sep = ", "),
    # Birth Place
    # -----------------
    birth_place = case_when(
      birth_place < 66 | (birth_place > 69 & birth_place < 75) ~ "United States",
      birth_place > 89 & birth_place < 100 ~ "Japan",
      birth_place > 79 & birth_place < 90 ~ "Other",
      T ~ NA_character_
    ) %>%
      factor(levels = c("United States", "Japan", "Other")),
    # AGE
    # -----------------
    birth_year = str_trim(birth_year) %>% 
      str_pad(width = 2, pad = "0", side = "left"),
    birth_year = case_when(
      birth_year == "SO" ~ NA_character_,
      # eh not great... definitely room for error. could use other vars to help
      birth_year < 43 ~ paste0("19", birth_year),
      birth_year > 42 ~  paste0("18", birth_year)
    ),
    birth_year = as.integer(birth_year),
    # seems to be year when started... we'll do this for approximations
    age = 1942 - birth_year,
    age5 = case_when(
      age < 18 ~ "0-17",
      age < 30 ~ "18-29",
      age < 45 ~ "30-44",
      age < 60 ~ "45-59",
      age >= 60 ~ "60+"
    ) %>%
      factor(levels = c("0-17", "18-29", "30-44", "45-59", "60+")),
    age3 = case_when(
      age < 18 ~ "0-17",
      age < 50 ~ "18-49",
      age >= 50 ~ "50+"
    ) %>%
      factor(levels = c("0-17", "18-49", "50+")),
    # Age in Japan
    # -----------------
    # just going to do oldest age
    age_in_japan = case_when(
      age_in_japan == "0" ~ "Never Lived in Japan",
      age_in_japan %in% c("1") ~ "0-9",
      age_in_japan %in% c("2", "4") ~ "10-19",
      age_in_japan %in% c("3", "5", "6", "7") ~ "20+",
      T ~ NA_character_
    ) %>% factor(levels = c("Never Lived in Japan", "0-9", "10-19", "20+")),
    len_in_japan = case_when(
      len_in_japan == "0" ~ "None",
      # should do finer categories if we plan to use this
      len_in_japan %in% c("1", "2") ~ "Less than 1 year",
      len_in_japan %in% c("3", "4") ~ "1-10 years",
      len_in_japan %in% c("5", "6", "7") ~ "More than 10 years",
      len_in_japan == "8" ~ "Time in another country",
      T ~ NA_character_
    ) %>% 
      factor(levels =  c(
        "None", "Less than 1 year", "1-10 years", "More than 10 years",
        "Time in another country")), 
    # Gender + Marstat
    # -----------------
    gender = case_when(
      gender_marstat %in% c("1", "2", "3", "4", "5", "0") ~ "Man",
      gender_marstat %in% c("6", "7", "8", "9", "-", "&") ~ "Woman",
      T ~ NA_character_
    ) %>% factor(levels = c("Man", "Woman")),
    marstat = case_when(
      gender_marstat %in% c("1", "6") ~ "Single",
      gender_marstat %in% c("2", "7") ~ "Married",
      gender_marstat %in% c("3", "8") ~ "Widowed",
      gender_marstat %in% c("4", "9") ~ "Divorced",
      gender_marstat %in% c("5", "-") ~ "Seperated",
      #gender_marstat %in% c("0", "&") ~ "Unknown",
    ) %>% factor(levels = c("Single", "Married", "Widowed", "Divorced", "Seperated")),
    # Race
    # -----------------
    # they call it race. perhaps not correct label
    # 99.8% Japanese
    race = case_when(
      race %in% c(3,4,7) ~ "Japanese",
      race %in% c(5) ~ "Japanese and White",
      race %in% c(8) ~ "White",
      race %in% c(1) ~ "Other",
      race %in% c(2) ~ "White and Other"
    ) %>% factor(levels = c("Japanese", "Japanese and White", "White",
                            "White and Other", "Other")),
    # seems to be Japanese, No Spouse or Unknown. leaving out for now
    # spouse_race = case_when(
    #   race %in% c(7, 1, 2) ~ "Japanese",
    # ) %>% factor(levels = c("Japanese", "Japanese and White", "White",
    #                         "White and Other", "Other")),
    # Military
    # -----------------
    military = case_when(
      military %in% c("1", "2") ~ "US Military",
      military %in% c("3", "4") ~ "Japanese Military",
      !is.na(military) ~ "No Military Service"
    ) %>% factor(levels = c("US Military", "Japanese Military", "No Military Service"))
    # will leave off here for now...
    # Education
    # -----------------
    # Religion
    # -----------------
    # I also want to add SS, but I don't understand the coding
  )


arrow::write_feather(out_data, "Clean_Data/internment_camps.feather")


