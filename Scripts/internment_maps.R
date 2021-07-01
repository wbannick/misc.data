library(tmap)
library(tidyverse)

sf_sub <- sf::read_sf("Not for Git/Clean_Data/pacific_subregion.gpkg") 

tm <- tmap::tm_shape(sf_sub) + 
  tm_polygons(
    "n",
    palette = "-viridis",
    popup.vars = c("subregion", "n", "never_in_japan"),
    breaks = c(1, 1000, 4000, 10000, 20000, 40000)
    )

tmap_save(tm, filename = "Misc/internment_maptest.html")
