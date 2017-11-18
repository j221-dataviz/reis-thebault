
# loading required packages
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# reading csvs
refineries_demo <- read_csv('refineries_demo.csv')
refineries_names <- read_csv('refineries_names.csv')

# joining the two
refineries_demo_names <- inner_join(refineries_demo, refineries_names, by=c("frs_id" = "frs_id"))

#deleting all those pesky nulls --- why are they there??
refineries_demo_names <- refineries_demo_names %>%
  filter(!is.na(frs_id))

#reordering columns, so that facility_name is first
refineries_demo_names <- refineries_demo_names %>%
  select(1,46,2:45)

#importing toxic release data, must join two 
tri_release <- read_csv('TRI_petroleum_release.csv')
tri_ID <- read_csv('tri_frs_ids.CSV')

#joining them and ditching all everything that isn't a refinery in 'tri_release'
refineries_release <- inner_join(tri_ID, tri_release, by=c("V_TRI_FORM_R_EZ.TRI_FACILITY_ID" = "TRIF ID"))

#joining release data with refinery demo data
refineries_demo_release <- inner_join(refineries_demo_names, refineries_release, by=c("frs_id" = "V_TRI_FORM_R_EZ.FRS_ID"))

#exporting refineries_demo_release for cleaning
write.csv(refineries_demo_release, file = 'refineries_demo_release.csv', row.names = FALSE)

#bringing clean data back
refineries_master <- read_csv('refineries_demo_release_clean.csv')

#creating column for pc of minority population
refineries_master <- refineries_master %>%
  mutate(pct_minority = round(minority_pop/total_persons*100,2))

#creating column for pc of ppl in poverty
refineries_master <- refineries_master %>%
  mutate(pct_poverty = round(persons_below_poverty_level/total_persons*100,2))

#selecting variables we're interested in

refineries_master <- refineries_master %>%
  select(1:3,5:7,11,56,15,57,48:50,52:53)



#creating three separate data frames for each radius
refineries_1mile <- refineries_master %>%
  filter(radius == '1')

refineries_3mile <- refineries_master %>%
  filter(radius == '3')

refineries_5mile <- refineries_master %>%
  filter(radius == '5')


urban_refineries3mile <- read_csv("urban_refineries3mile.csv") %>%
  mutate(urban_rural = "urban")

rural_refineries3mile <- anti_join(refineries_3mile,urban_refineries3mile, by="frs_id") %>%
  mutate(urban_rural = "rural")

refineries_3mile_edit <- bind_rows(urban_refineries3mile,rural_refineries3mile)




# loading census data

# load required packages
library(tidycensus)
library(rgdal)
library(tigris)

# set census api key. Obtain from http://api.census.gov/data/key_signup.html
census_api_key("ea622cc1e1cfe64aa1d585cd7e2e360f9de089ad", install = TRUE)
readRenviron("~/.Renviron")

# get list of variables in the 2010 American Community Survey, 5-year estimates (i.e. 2011-2015)
acs10 <- load_variables(2010, "acs5", cache = TRUE)

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

name <- as.data.frame(state.name)
abb <- as.data.frame(state.abb)
states <- bind_cols(name,abb)

states <- data_frame %>%
  mutate(NAME = name ,ABB = abb)
names(states) <- c("NAME","ABB")

# number of households
households <- get_decennial(geography = "state",
                            variables = "P0190001",
                            year = 2010) %>%
  rename(households = value) %>%
  select(1,2,4)

# number of persons in poverty
poverty <- get_acs(geography = "state",
                         variables = "B17001_002E",
                         year =  2010) %>%
  rename(people_in_poverty = estimate) %>%
  select(1,2,4)


# white population (white alone, not hispanic)
white_population <- get_decennial(geography = "state",
                                  variables = "P0090005",
                                  year = 2010) %>%
  rename(white_population = value) %>%
  select(1,2,4)

# join data frames into one, calculate percentages where necessary
states <- inner_join(population,white_population)

states <- inner_join(states,poverty, by=c("GEOID"="GEOID","NAME"="NAME")) %>%
  mutate(pc_white_population = round(white_population/population*100,2),
         pc_minority_population = 100 - pc_white_population)

states <- states %>%
  mutate(pc_people_in_poverty = round(people_in_poverty/population*100,2))


#renaming and reordering columns for clarity
states <- states %>%
  select(1:3,7,8)

states <- states %>%
  rename(state_pc_minority_pop = pc_minority_population,
         state_pc_poverty_pop = pc_people_in_poverty)


#bringing in csv of state abbs
state_abb <- read_csv("states_abb.csv")

#joining here ... losing Puerto Rico
states <- inner_join(states, state_abb, by=c("NAME"="State"))

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

#joining refineries_master with states to compare demographics
refineries_compare <- inner_join(refineries_master, states, by = c("state"="Abbreviation"))

#creating column that compares the pct of minorities/pct of poverty surrounding refineries to the state pct
refineries_compare <- refineries_compare %>%
  mutate(minority_compare = pct_minority/state_pc_minority_pop) %>%
  mutate(poverty_compare = pct_poverty/state_pc_poverty_pop)

# renaming problem TRI emissions variable, and then creating new one in millions of lbs
refineries_compare <- refineries_compare %>%
  rename(total_releases = `total_releases_on-off-site_lbs`) %>%
  mutate(total_releases_mil = total_releases/10^6)

# correlation between releases and minority compare
ggplot(subset(refineries_compare, radius==5), aes(x=minority_compare, y=total_releases_mil)) +
  geom_point(aes(size=minority_pop), color = "red", alpha = 0.5) +
  ggtitle("Population within 5 miles of refinery") +
  scale_x_continuous(breaks = c(0,0.5,1,2,3,4), labels = c("Zero","Half","","Twice","3 times","4 times")) +
  scale_y_continuous(labels = comma) +
  scale_size_area(max_size = 10, guide = FALSE) +
  xlab("% minority population, compared to % for entire state") +
  ylab("Toxic releases (million lbs)") +
  geom_vline(xintercept = 1, linetype = "dotted") +
  theme_minimal()
  
ggplot(subset(refineries_compare, radius==3), aes(x=minority_compare, y=total_releases_mil)) +
  geom_point(aes(size=minority_pop), color = "red", alpha = 0.5) +
  ggtitle("Population within 3 miles of refinery") +
  scale_x_continuous(breaks = c(0,0.5,1,2,3,4), labels = c("Zero","Half","","Twice","3 times","4 times")) +
  scale_y_continuous(labels = comma) +
  scale_size_area(max_size = 10, guide = FALSE) +
  xlab("% minority population, compared to % for entire state") +
  ylab("Toxic releases (million lbs)") +
  geom_vline(xintercept = 1, linetype = "dotted") +
  theme_minimal()

ggplot(subset(refineries_compare, radius==1), aes(x=minority_compare, y=total_releases_mil)) +
  geom_point(aes(size=minority_pop), color = "red", alpha = 0.5) +
  ggtitle("Population within 1 mile of refinery") +
  scale_x_continuous(breaks = c(0,0.5,1,2,3,4), labels = c("Zero","Half","","Twice","3 times","4 times")) +
  scale_y_continuous(labels = comma) +
  scale_size_area(max_size = 10, guide = FALSE) +
  xlab("% minority population, compared to % for entire state") +
  ylab("Toxic releases (million lbs)") +
  geom_vline(xintercept = 1, linetype = "dotted") +
  theme_minimal()


# correlation between releases and poverty compare
ggplot(subset(refineries_compare, radius==5), aes(x=poverty_compare, y=total_releases_mil)) +
  geom_point(aes(size=persons_below_poverty_level), color = "red", alpha = 0.5) +
  ggtitle("Population within 5 miles of refinery") +
  scale_x_continuous(breaks = c(0,0.5,1,2,3,4,5), labels = c("Zero","Half","","Twice","3 times","4 times","5 times")) +
  scale_y_continuous(labels = comma) +
  scale_size_area(max_size = 10, guide = FALSE) +
  xlab("% population in poverty, compared to % for entire state") +
  ylab("Toxic releases (million lbs)") +
  geom_vline(xintercept = 1, linetype = "dotted") +
  theme_minimal()

ggplot(subset(refineries_compare, radius==3), aes(x=poverty_compare, y=total_releases_mil)) +
  geom_point(aes(size=persons_below_poverty_level), color = "red", alpha = 0.5) +
  ggtitle("Population within 3 miles of refinery") +
  scale_x_continuous(breaks = c(0,0.5,1,2,3,4,5), labels = c("Zero","Half","","Twice","3 times","4 times","5 times")) +
  scale_y_continuous(labels = comma) +
  scale_size_area(max_size = 10, guide = FALSE) +
  xlab("% population in poverty, compared to % for entire state") +
  ylab("Toxic releases (million lbs)") +
  geom_vline(xintercept = 1, linetype = "dotted") +
  theme_minimal()

ggplot(subset(refineries_compare, radius==1), aes(x=poverty_compare, y=total_releases_mil)) +
  geom_point(aes(size=persons_below_poverty_level), color = "red", alpha = 0.5) +
  ggtitle("Population within 1 miles of refinery") +
  scale_x_continuous(breaks = c(0,0.5,1,2,3,4,5), labels = c("Zero","Half","","Twice","3 times","4 times","5 times")) +
  scale_y_continuous(labels = comma) +
  scale_size_area(max_size = 10, guide = FALSE) +
  xlab("% population in poverty, compared to % for entire state") +
  ylab("Toxic releases (million lbs)") +
  geom_vline(xintercept = 1, linetype = "dotted") +
  theme_minimal()






