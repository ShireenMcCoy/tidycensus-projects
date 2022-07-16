## -----------------------------------------------------------------
# Mobility map - Franklin County, MO
## -----------------------------------------------------------------

# This code is based on a tutorial by Kyle Walker:
# https://walker-data.com/tidycensus/articles/other-datasets.html

##----------------------------------------------------------------------------

library(tidycensus)
library(tidyverse)
library(tigris)
census_api_key("your key here")

## ----get data for Franklin County-------------------------------------------------------------

county_flows <- get_flows(
  geography = "county",
  state = "MO",
  county = "Franklin",
  year = 2019,
  geometry = TRUE
)


# Find how many pepole moved to the county from outside the US
moved_from_outside_us <- county_flows %>% 
  filter(variable == "MOVEDIN") %>% 
  filter(is.na(GEOID2))
arrange(desc(estimate)) 

# show results
moved_from_outside_us 


## ---------------------------------------------------------------------------
# Map the top 25 US counties from which people moved to this county
##----------------------------------------------------------------

library(mapdeck)

top_move_in <- county_flows %>% 
  # get rid of international rows b/c they don't have geoid so we can't map them
  # select only "MOVEDIN" rows
  filter(!is.na(GEOID2), variable == "MOVEDIN") %>% 
  slice_max(n = 25, order_by = estimate) %>% 
  mutate(
    # width of the lines between places:
    width = 2.6,
    tooltip = paste0(
      # 2nd arguement to scales::comma tells you the accuaracy to round to: 
      # "1" means round to the ones' place (no decimals)
      #multiply by 5 because it's the 5-year ACS, and the estimates are only for 1 year.
      scales::comma(estimate * 5, 1),
      " people moved from ", str_remove(FULL2_NAME, "County"),
      " to ", FULL1_NAME, " between 2015 and 2019"
      
    )
  )

map <- top_move_in %>% 
  mapdeck(token = "YOUR TOKEN HERE", 
  style = mapdeck_style("light"), pitch = 45) %>% 
  add_arc(
    origin = "centroid1",
    destination = "centroid2",
    stroke_width = "width",
    auto_highlight = TRUE,
    highlight_colour = "#F23D82FF",
    tooltip = "tooltip"
  )

map








