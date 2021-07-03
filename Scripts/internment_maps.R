library(tmap)
library(tidyverse)

sf_sub <- sf::read_sf("Not for Git/Clean_Data/pacific_subregion.gpkg") %>%
  # recoding names of columns used in this visualization!
  # keeping others as is so that grouped data is easier to program with
  rename(
    # eh may need a better label
    "People Displaced" = "n",
    "Percent of Records" = "percent_records",
    "Percent Never Been to Japan" = "never_in_japan",
    "Percent Born in Japan" = "japan_born",
    "Percent Children" = "under_18",
    "Most Common Camp" = "mode_camp"
  )
  

tm <- tmap::tm_shape(sf_sub) + 
  tm_polygons(
    "People Displaced",
    id = "subregion",
    palette = "-viridis",
    popup.vars = 
      c(
        "People Displaced",
        "Percent of Records",
        "Percent Never Been to Japan",
        "Percent Born in Japan",
        "Percent Children",
        "Most Common Camp"
      ),
    breaks = c(1, 1000, 4000, 10000, 20000, 40000)
    )

tmap_save(tm, filename = "Misc/internment_maptest.html")
