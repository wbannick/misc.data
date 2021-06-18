library(tidyverse)
library(sf)

# Read in Map
# ------------------
tmp_fl <- tempfile()
# map from IPUMS NHGIS, University of Minnesota, www.nhgis.org
# not pushing to github because not sure if I am allowed to.
# I mean also it's really big
# Unzipping here because big file
unzip("Not For Git/nhgis0001_shapefile_tl2008_us_county_1940.zip", exdir = tmp_fl)

shp_fls <- list.files(tmp_fl, full.names = T)
shp_fl <- shp_fls[str_detect(shp_fls, "\\.shp$")]

sf_1940 <- sf::read_sf(shp_fl, promote_to_multi = F)

unlink(tmp_fl)

# Recodes
# ------------------

sf_1940 <- sf_1940 %>%
  filter(
    # only these states for now
    STATENAM %in% c("California", "Oregon", "Washington")
  ) %>%
  transmute(
    state = STATENAM,
    # are these old fips perhaps
    state_code = STATE,
    county = NHGISNAM,
    county_code = COUNTY,
    county_fip = ICPSRFIP,
    geometry
  )


# Join on Sub Regions
# ------------------

# # not doing city... maybe county
# county_xwalk <- tribble(
#   ~residence_code, ~residence_county, ~residencs_subregion,
#   1311, "Del Norte", "Northwestern Coastal Hills and Valleys",
#   1312, "Humboldt",  "Northwestern Coastal Hills and Valleys",
#   1313, "Lake", "Northwestern Coastal Hills and Valleys",
#   1314, "Mondocino", "Northwestern Coastal Hills and Valleys",
#   1315, "Napa", "Northwestern Coastal Hills and Valleys",
#   1316, "Sonoma", "Northwestern Coastal Hills and Valleys",
#   1317, "Trinity", "Northwestern Coastal Hills and Valleys",
#   1321, "Alpine/Mono", "Sierra and Northeastern Area",
#   1322, "Anador", "Sierra and Northeastern Area",
#   1323, "Calaveras", "Sierra and Northeastern Area",
#   1324, "El Dorado", "Sierra and Northeastern Area",
#   1325, "Inyo", "Sierra and Northeastern Area",
#   1326, "Lacson", "Sierra and Northeastern Area",
# )

sub_region_xwalk <- tibble(
  state = c(
    rep("California", 58)
    #rep("Oregon", 36),
    #rep("Washington", 39)
    ),
  sub_region = c(
    rep("Northwestern Coastal Hills and Valleys", 7),
    rep("Sierra and Northeastern Area", 16),
    rep("Sacramento River Valley", 7),
    rep("Central Coastal Hills and Valleys", 4),
    rep("San Joaquin River Valley", 8),
    rep("Santa Barbara-Ventura Area", 2),
    rep("Southeastern Desert and Irrigated Valleys", 3),
    rep("San Francisco-Oakland Metropolitan Counties", 6),
    "Sacramento Metropolitan County",
    "San Jose Metropolitan County",
    rep("Los Angeles Metropolitan Counties", 2),
    "San Diego Metropolitan County"
  ),
  county = c(
    "Del Norte", "Humboldt", "Lake", "Mondocino", "Napa", "Sonoma",
    "Trinity", "Alpine", "Anador", "Calaveras", "El Dorado", "Inyo",
    "Lacson", "Mariposa", "Modoc", "Mono", "Nevada", "Placer", "Plumas",
    "Shasta", "Sierra", "Siskiyou", "Tuolumne", "Butte", "Colusa", "Glenn",
    "Sutter", "Tehama", "Yolo", "Yuba", "Monterey", "San Benito",
    "San Luis Obispo", "Santa Cruz", "Fresno", "Kern", "Kings", "Madera",
    "Merced", "San Joaquin", "Stanislaus", "Tulare", "Santa Barbara",
    "Ventura", "Imperial", "Riverside", "San Bernardino", "Alameda",
    "Contra Costa", "Marin", "San Francisco", "San Mateo", "Solano",
    "Sacramento", "Santa Clara", "Los Angeles", "Orange", "San Diego"
  )
)
