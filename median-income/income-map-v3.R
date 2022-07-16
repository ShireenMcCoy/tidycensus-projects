## -----------------------------------------------------------------

# Median household income map -- Updated for new district boundaries

# Get data and make choropleth map from ACS 2016-2020 ACS  
# for MO Congressional District 2:
# Warren, St. Charles, Franklin, and St. Louis counties

## -----------------------------------------------------------------

 
library(tidycensus)
library(tidyverse)
library(scales)
library(mapview)
library(leafsync)
library(tigris)
library(sf)
library(ggspatial)
library("openxlsx")

# replace "your key here" with an api key you can get from the census.
census_api_key("your key here")

## ----get data for 4 counties-------------------------------------------------------------
# this will include some census tracts that are outside the district

results <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "MO",
  county = c("Warren", "St. Charles", "Franklin", "St. Louis County"),
  geometry = TRUE,
  year = 2020,
  cb = FALSE #<<
) %>%
  st_transform(crs=st_crs('NAD83')) %>% #<<
  erase_water(area_threshold = 0.99) #<<


## ----FILTER RESULTS  --------------------------------------------------------------
# ... so they only show census tracts inside (or partially inside) the district

# 1st step is to download this folder that Nick S. linked to:
# https://redistrictingdatahub.org/download/?datasetid=36002&document=%2Fweb_ready_stage%2Flegislative%2F2021_adopted_plans%2Fmo_cong_adopted_2022.zip

# The folder is named "mo_cong_adopted_2022"
# There is a file inside this folder named "5799H_02T.xlsx"
# This file has table with one column of GEOIDs for every census block in Missouri,
# and another column for the congressional district that each census block belongs to.

# read the xlsx file, leaving off the first row, which contains column names
reldata <- read.xlsx(xlsxFile = "mo_cong_adopted_2022/5799H_02T.xlsx", startRow=2)

# The GEOID for each block is a 15-digit number where the last 4 digits are 
# the block group and block. 

# The GEOIDs returned by our get_acs() call has GEOIDs for tracts, which are
# 11 digits long. 

# The first 11 digits of every 15-digit GEOID gives you the GEOID of the 
# tract that it belongs to.

# So we need to find the unique tract GEEOIDs in our list of census blocks in 
# district 2, and remove of any tracts that are not in that list from 
# our get_acs() results

# get all block-level (15-digit) GEOIDs in district 2
blocks <- filter(reldata, reldata[, 2] == "2")

# get the first 11 digits of the block-level GEOID, which gives us the 
# tract-level GEOID.Then remove duplicates.
tracts <- unique(substr(blocks[, 1], 1, 11))

# Now we need to build up a table with all the same columns as the "results"
# table. It should have a row for each tract in "tracts". Unlike the
# table returned by get_acs(), this new table doesn't
# include tracts that are fully outside the district.

# get the first row of the get_acs() results, just so we have the 
# column names
row1 <- filter(results, GEOID==tracts[1])

# now remove the first (and only) row, so we have an empty table with 
# correct column names
cd2 <- row1[-1,]

# Loop for the list of tracts. For each tract in the list,
# add the row of the get_acs() result table whose GEOID matches
# that tract number 
for (tract in tracts){
  cd2 <- add_row(cd2, filter(results, GEOID==tract))
}
## ----make map------------------------------------------------------------------------

final_map <- ggplot(cd2, aes(fill = estimate)) + 
  annotation_map_tile(type = "cartolight", zoom = 11) + #<<
  theme_void(base_size = 14) + 
  geom_sf(alpha = 0.4, lwd = 0.1) + 
  scale_fill_viridis_c(option = "mako", labels = label_dollar()) + 
  labs(title = "Median household income:",
       subtitle = "Warren, St. Charles, Franklin, and St. Louis Counties - 2016-2020 ACS estimates",
       caption = "Tile source: CARTO / OpenStreetMap contributors",
       fill = "ACS estimate  ")

## ----show map--- this can take a few seconds to load---------------------------------------------------------------
final_map







