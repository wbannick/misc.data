library(tmap)
library(tidyverse)

sf_sub <- sf::read_sf("Not for Git/Clean_Data/pacific_subregion.gpkg") %>%
  # recoding names of columns used in this visualization!
  # keeping others as is so that grouped data is easier to program with
  rename(
    # eh may need a better label
    "People Incarcerated" = "n",
    "Percent of Records" = "percent_records",
    "Percent Never Been to Japan" = "never_in_japan",
    "Percent Born in Japan" = "japan_born",
    "Percent Born in the US" = "us_born",
    "Percent Children" = "under_18",
    "Most Common Camp" = "mode_camp"
  )


# TO DO
# MAYBE PERCENT OF POPULATION
# can either A put a second layer or B try facet wrappin in RMD
# write text
tm <- 
  tmap::tm_shape(sf_sub, name = "People Incarcerated") + 
  tm_polygons(
    "People Incarcerated",
    id = "subregion",
    palette = "-magma",
    popup.vars = 
      c(
        "People Incarcerated",
        "Percent of Records",
        "Percent Never Been to Japan",
        "Percent Born in the US",
        "Percent Children",
        "Most Common Camp"
      ),
    breaks = c(50, 1000, 4000, 10000, 20000, 40000)
    ) +
  tm_view(set.zoom.limits = c(5,10)) +
  tmap_options(basemaps = c("Esri.WorldGrayCanvas")) 

tmap_save(tm, filename = "Misc/japanese_american_incarceration_maptest.html")
