library(tidyverse)
library(sf)
library(arrow)

# I. Read in Map + Grouped Stats
# ------------------------
# ------------------------

# Map!
tmp_fl <- tempfile()
# map from IPUMS NHGIS, University of Minnesota, www.nhgis.org
# not pushing to github because not sure if I am allowed to.
# I mean also it's really big
# Unzipping here because big file
unzip("Not For Git/Data/nhgis0001_shapefile_tl2008_us_county_1940.zip", exdir = tmp_fl)

shp_fls <- list.files(tmp_fl, full.names = T)
shp_fl <- shp_fls[str_detect(shp_fls, "\\.shp$")]

sf_1940 <- sf::read_sf(shp_fl, promote_to_multi = F)

unlink(tmp_fl)

# stats to join to map
stats_by_subregion <- read_feather("Clean_Data/internment_stats_by_subregion.feather")


# II. Map Recodes
# ------------------------
# ------------------------

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


# III. Create Subregion XWALK
# ------------------------
# ------------------------

sub_region_xwalk <- tibble(
  state = c(
    rep("California", 58),
    rep("Oregon", 36),
    rep("Washington", 39)
    ),
  subregion = c(
    # CALIFORNIA
    # ------------
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
    "San Diego Metropolitan County",
    # OREGON
    # ------------
    rep("Northwestern Area", 12),
    rep("Southwestern Area", 5),
    rep("Eastern Wheat Area", 6),
    rep("Eastern Irrigation and Grasing Area", 11),
    rep("Portland Metropolitan Counties", 2),
    # Washington
    # ------------
    rep("Western Slope", 5),
    rep("Western Slope (Inland)", 12),
    rep("Central and Northeastern Area", 9),
    rep("Columbia Plateau Wheat Area", 10),
    rep("Seattle-Tacoma Metropolitan Counties", 2),
    "Spokane Metropolitan County"
  ),
  county = c(
    # CALIFORNIA
    # ------------
    "Del Norte", "Humboldt", "Lake", "Mendocino", "Napa", "Sonoma",
    "Trinity", "Alpine", "Amador", "Calaveras", "El Dorado", "Inyo",
    "Lassen", "Mariposa", "Modoc", "Mono", "Nevada", "Placer", "Plumas",
    "Shasta", "Sierra", "Siskiyou", "Tuolumne", "Butte", "Colusa", "Glenn",
    "Sutter", "Tehama", "Yolo", "Yuba", "Monterey", "San Benito",
    "San Luis Obispo", "Santa Cruz", "Fresno", "Kern", "Kings", "Madera",
    "Merced", "San Joaquin", "Stanislaus", "Tulare", "Santa Barbara",
    "Ventura", "Imperial", "Riverside", "San Bernardino", "Alameda",
    "Contra Costa", "Marin", "San Francisco", "San Mateo", "Solano",
    "Sacramento", "Santa Clara", "Los Angeles", "Orange", "San Diego",
    # OREGON
    # ------------
    "Benton", "Clatsop", "Columbia", "Hood River", "Lane", "Lincoln",
    "Linn", "Marion", "Polk", "Tillamook", "Washington", "Yamhill", "Coos",
    "Curry", "Douglas", "Jackson", "Josephine", "Gilliam", "Jefferson", 
    "Morrow", "Sherman", "Umatilla", "Wasco", "Baker", "Crook", "Deschutes",
    "Grant", "Harney", "Klamath", "Lake", "Malheur", "Union", "Wallowa",
    "Wheeler", "Clackamas", "Multnomah",
    # Washington
    # ------------
    "Clallam", "Grays Harbor", "Jefferson", "Pacific", "Wahkiakum", "Clark",
    "Cowlitz", "Island", "Kitsap", "Lewis", "Mason", "San Juan", "Skagit",
    "Skamania", "Snohomish", "Thurston", "Whatcom", "Benton", "Chelan",
    "Ferry", "Kittitas", "Klikatat", "Okanogan", "Pend Oreille", "Stevens",
    "Yakima", "Adams", "Asotin", "Columbia", "Douglas", "Franklin", "Garfield",
    "Grant", "Lincoln", "Walla Walla", "Whitman", "King", "Pierce", "Spokane"
  )
)

# Join + Group Data
# ------------------------
# ------------------------

sf_1940 <- sf_1940 %>%
  # join on sub regions (so we can join on stats)
  left_join(sub_region_xwalk)

# group geometries by subregion
sf_subregion <- sf_1940 %>%
  mutate(
    # grouping cuz of small numbers
    subregion = case_when(
      subregion %in% c(
        "Columbia Plateau Wheat Area", "Spokane Metropolitan County",
        "Central and Northeastern Area") ~ "Central and Eastern Washington",
      str_detect(subregion, "Western Slope") ~ "Western Slope",
      subregion %in% c("Northwestern Area", "Southwestern Area") ~ "Western Oregon",
      subregion %in% 
        c("Eastern Irrigation and Grasing Area", "Eastern Wheat Area") ~ "Eastern Oregon",
      T ~ subregion)
  ) %>%
  group_by(state, state_code, subregion) %>%
  summarise() %>%
  ungroup() %>%
  # convert to 4269 crs
  st_transform(4269) %>%
  # ... and validate (bay and la are invalid)
  st_make_valid() %>%
  # finaly join on the summarized stats
  # for the concentration camps
  left_join(stats_by_subregion)


# not pushing it to github because not sure if allowed to given IPUMS
sf::write_sf(sf_subregion, "Not for Git/Clean_Data/pacific_subregion.gpkg")



