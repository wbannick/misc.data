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
