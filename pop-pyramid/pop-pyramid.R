# Make a population pyramid for the 4 counties in Missouri's 
# 2nd Congressional district

# NOTE: this will include people who are in the parts of the counties that fall
# outside the district!


library(tidycensus)
library(tidyverse)
library(plotly)
library("openxlsx")


## ----get data for 4 counties-------------------------------------------------------------
# this will include some census tracts that are outside the district

results <- get_estimates(
  geography = "county",
  state = "MO",
  county = c("Warren", "St. Charles", "Franklin", "St. Louis County"),
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  year = 2019
)


# Select just the rows with 5-year age bands, and get rid of rows for both sexes.
# Make male pop counts negative so they mirror around central axis.
filtered <- filter(results, str_detect(AGEGROUP, "^Age"), 
  SEX != "Both sexes") %>%
  mutate(value = ifelse(SEX == "Male", -value, value))



## --------------------------------------------------------------------------------
pop_pyramid <- ggplot(filtered, aes(x = value, y = AGEGROUP, fill = SEX)) + 
  geom_col(width = 0.95, alpha = 0.75) + 
  scale_x_continuous(labels = function(x) paste0(abs(x / 1000), "k")) + 
  scale_y_discrete(labels = function(y) gsub("Age | years", "", y)) + 
  labs(x = "", 
       y = "2019 Census Bureau population estimate", 
       title = "Pop Structure: Warren, St. Charles, Franklin, & St. Louis Counties", 
       fill = "", 
       caption = "Data source: US Census Bureau population estimates & tidycensus R package")

ggsave("pop_pyramid.png", pop_pyramid)

## --------------------------------------------------------------------------------
pop_pyramid