# load required packages
library(tidycensus)
library(rgdal)
library(readr)
library(dplyr)
library(tigris)

# set census api key. Obtain from http://api.census.gov/data/key_signup.html
census_api_key("your API key here!", install = TRUE)
readRenviron("~/.Renviron")

# get list of variables in the 2015 American Community Survey, 5-year estimates (i.e. 2011-2015)
acs15 <- load_variables(2015, "acs5", cache = TRUE)

#############
# get ACS 2015 5-year data, by counties

# population
population <- get_acs(geography = "state", 
                      variables = "B01003_001",
                      year = 2015) %>%
  rename(population = estimate)  %>%
  select(1,2,4)

# number of households
households <- get_acs(geography = "state", 
                      variables = c("B17017_001"), 
                      year = 2015) %>%
  rename(households = estimate) %>%
  select(1,2,4)

# white population (white alone, not latino)
white_population <- get_acs(geography = "state",
                            variables = c("B01001H_001"),
                            year = 2015) %>%
  rename(white_population = estimate) %>%
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






