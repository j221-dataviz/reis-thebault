# load required packages
library(tidycensus)
library(rgdal)
library(readr)
library(dplyr)
library(tigris)

# set census api key. Obtain from http://api.census.gov/data/key_signup.html
census_api_key("ea622cc1e1cfe64aa1d585cd7e2e360f9de089ad", install = TRUE, overwrite=TRUE)
readRenviron("~/.Renviron")

# get list of variables in the 2015 American Community Survey, 5-year estimates (i.e. 2011-2015)
acs15 <- load_variables(2015, "acs5", cache = TRUE)

# get list of variables in the 2010 Census (i.e. 2011-2015)
census10_sf1 <- load_variables(2010, "sf1", cache = TRUE)


#############
# get 2010 Census data, by state

# population
population <- get_decennial(geography = "state", 
                      variables = "P0010001",
                      year = 2010) %>%
  rename(population = value)  %>%
  select(1,2,4)

# number of households
households <- get_decennial(geography = "state",
                            variables = "P0190001",
                            year = 2010) %>%
  rename(households = value) %>%
  select(1,2,4)

# white population (white alone, not hispanic)
white_population <- get_decennial(geography = "state",
                            variables = "P0090005",
                            year = 2010) %>%
  rename(white_population = value) %>%
  select(1,2,4)

# join data frames into one, calculate percentages where necessary
states <- inner_join(population,households) %>%
  inner_join(white_population) %>%
  mutate(pc_white_population = round(white_population/population*100,2),
         pc_minority_population = 100 - pc_white_population)

# Some cleaning: remove any NaNs from data, replace with zero
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
states[is.nan(states)] <- 0

# save as CSV
write_csv(states,"states.csv", na="")

# works with tigris package to obtain Census Bureau shapefiles
options(tigris_use_cache = TRUE)

# load Census Bureau TIGER/LINE states shapefile using tigris
states_map <- states()

# join shapefile to Census data (fix row names first)
states_map@data <- left_join(states_map@data, states) 

# save shapefile
writeOGR(states_map,"states_map", layer="states_map", driver = "ESRI Shapefile")






