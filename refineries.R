
# loading required packages
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(leaflet)
library(rgdal)
library(quantmod)
library(highcharter)
library(RColorBrewer)
library(forcats)
library(htmlwidgets)
library(ggiraph)
library(tidyverse)
library(data.table)

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
# get 2010 Census/ACS data, by state

name <- as.data.frame(state.name)
abb <- as.data.frame(state.abb)
states <- bind_cols(name,abb)

states <- data_frame %>%
  mutate(NAME = name ,ABB = abb)
names(states) <- c("NAME","ABB")

# population
population <- get_decennial(geography = "state", 
                            variables = "P0010001",
                            year = 2010) %>%
  rename(population = value)  %>%
  select(1,2,4)

# white population (white alone, not hispanic)
white_population <- get_decennial(geography = "state",
                                  variables = "P0090005",
                                  year = 2010) %>%
  rename(white_population = value) %>%
  select(1,2,4)

# join these  
population <- inner_join(white_population,population)

# # number of households
# households <- get_decennial(geography = "state",
#                             variables = "P0190001",
#                             year = 2010) %>%
#   rename(households = value) %>%
#   select(1,2,4)

# number of persons in poverty
poverty <- get_acs(geography = "state",
                         variables = "B17001_002",
                         year =  2010) %>%
  rename(people_in_poverty = estimate) %>%
  select(1,2,4)

# denominator for number of persons in poverty
denom_poverty <- get_acs(geography = "state",
                   variables = "B17001_001",
                   year =  2010) %>%
  rename(denom_poverty = estimate) %>%
  select(1,2,4)

# join those
poverty <- inner_join(poverty,denom_poverty)

# join data frames into one, calculate percentages where necessary
states <- inner_join(states,poverty) 
states <- inner_join(states,population) %>%
  mutate(pc_white_population = round(white_population/population*100,2),
         pc_minority_population = 100 - pc_white_population,
         pc_people_in_poverty = round(people_in_poverty/denom_poverty*100,2))


#renaming and reordering columns for clarity
states <- states %>%
  select(1:3,9,10) %>%
  rename(state_pc_minority_pop = pc_minority_population,
         state_pc_poverty_pop = pc_people_in_poverty)

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
refineries_compare <- inner_join(refineries_master, states, by = c("state"="ABB"))

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
  geom_point(aes(size=minority_pop), color = "#006d2c", alpha = 0.5) +
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
  geom_point(aes(size=persons_below_poverty_level), color = "#54278f", alpha = 0.5) +
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




#grabbing only refineries with more than x waste

poverty_map <- refineries_compare %>%
  filter(radius==3) %>%
  filter(total_releases_mil>=.5)


# setting quantile breaks
breaks <- quantile(poverty_map$poverty_compare, probs = seq(0,1,0.2), na.rm=TRUE)

# set color palette
binpal <- colorBin("Purples", poverty_map$poverty_compare, breaks)

# mapping refineries with more than x waste by effect on poor
poor_map <- leaflet() %>%
  setView(lng = -98.5795, lat = 39.828175, zoom = 4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircles(
    data = poverty_map,
    radius = sqrt(poverty_map$total_releases_mil)*100000,
    color = "#54278f",
    weight = 0.2,
    fillColor = ~binpal(poverty_compare),
    fillOpacity = 0.5,
    popup = paste0("<strong>Percent of surrounding people in poverty: </strong>", poverty_map$pct_poverty, "</br>",
                   "<strong>Total pounds of toxic releases (millions): </strong>", poverty_map$total_releases_mil)
    )

print(poor_map)


# save chart as a web page
saveWidget(poor_map, "poor_map.html", selfcontained = TRUE, libdir = NULL, background = "white")

# creating df of refineries in Texas, Louisiana and California, limiting to top 15 toxic releasers
tx_chart <- refineries_compare %>%
  filter(radius==3) %>%
  filter(state=="TX") %>%
  arrange(desc(total_releases_mil)) %>%
  head(15)

ca_chart <- refineries_compare %>%
  filter(radius==3) %>%
  filter(state=="CA") %>%
  arrange(desc(total_releases_mil)) %>%
  head(15)

la_chart <- refineries_compare %>%
  filter(radius==3) %>%
  filter(state=="LA") %>%
  arrange(desc(total_releases_mil)) %>%
  head(15)

# creating new columns with edited refinery names

tx_chart <- tx_chart %>%
  mutate(refinery_names = c("Blanchard, Texas City", "Valero, Corpus Christi", "ExxonMobil, Baytown", "Valero, Texas City", "Diamond Shamrock, Sunray", "Motiva, Port Arthur", "Flint Hills Resources (West), Corpus Christi", "Flint Hills Resources (East), Corpus Christi", "Deer Park, Deer Park", "ExxonMobil, Beaumont", "Citgo, Corpus Christi", "Phillips 66, Borger", "Premcor, Port Arthur", "Total, Port Arthur", "Phillips 66, Old Ocean"))

ca_chart <- ca_chart %>%
  mutate(refinery_names = c("Chevron, El Segundo", "Phillips 66, Rodeo", "Chevron, Richmond", "Valero, Benicia", "Tesoro, Carson", "Shell, Martinez", "Tesoro, Martinez", "ExxonMobil, Torrance", "Phillips 66, Wilmington", "Ultramar, Wilmington", "Phillips 66, Carson", "Kern, Bakersfield", "Phillips 66, Arroyo Grande", "San Joaquin, Bakersfield", "Lunday-Thagard, South Gate"))

la_chart <- la_chart %>%
  mutate(refinery_names = c("ExxonMobil, Baton Rouge", "Citgo, Sulphur", "Phillips 66, Westlake", "Valero, Norco", "Marathon, Garyville", "Chalmette, Chalmette", "Phillips 66, Belle Chasse", "Motiva, Norco", "Alon, Krotz Springs", "Convent, Convent", "Placid, Port Allen", "Calumet, Shreveport", "Valero, Meraux", "Rain CII Carbon, Sulphur", "Calcasieu, Lake Charles"))

# plotting charts for poverty

tx_poverty_gg <- ggplot(tx_chart) +
  geom_segment(color="#6a51a3", aes(xend=refinery_names, x=reorder(refinery_names, total_releases_mil), y=state_pc_poverty_pop, yend=pct_poverty), arrow = arrow(length = unit(0.2,"cm"), type="closed")) +
  geom_point( aes(x=refinery_names, y=state_pc_poverty_pop), color="#6a51a3", size=1.5 ) +
  geom_point_interactive(aes(tooltip = paste0("Toxic waste released (million lbs): ", round(total_releases_mil,2)),
                             size = total_releases_mil, x=refinery_names, y=0, data_id = refinery_names), alpha = 0.5, color="#e6550d") +
  scale_size_area(max_size = 10) +
  scale_y_continuous(limits=c(-10,70), breaks = c(20,40,60)) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(color = "#e6550d"),
    plot.subtitle = element_text(color = "#e6550d"),
    axis.title = element_text(color = "#6a51a3",
                              hjust = -0.2)) +
  xlab("") +
  labs(title="Toxic emissions", subtitle="Hover for details", color="#e6550d") +
  ylab("% in poverty (arrow: local; point: state)") +
  geom_hline(yintercept=0, size=0.1, color="#e6550d") +
  coord_flip()

tx_poverty_interactive <- ggiraph(code = print(tx_poverty_gg), height_svg=5,
                                  hover_css = "cursor:pointer;fill-opacity:0.8;stroke:#e6550d",
                                  tooltip_extra_css = "background-color:#f8f8f8;color:#e6550d;font-family:'Helvetica';font-size:10px;padding:2px")
print(tx_poverty_interactive)


ca_poverty_gg <- ggplot(ca_chart) +
  geom_segment(color="#6a51a3", aes(xend=refinery_names, x=reorder(refinery_names, total_releases_mil), y=state_pc_poverty_pop, yend=pct_poverty), arrow = arrow(length = unit(0.2,"cm"), type="closed")) +
  geom_point( aes(x=refinery_names, y=state_pc_poverty_pop), color="#6a51a3", size=1.5 ) +
  geom_point_interactive(aes(tooltip = paste0("Toxic waste released (million lbs): ", round(total_releases_mil,2)),
                             size = total_releases_mil, x=refinery_names, y=0, data_id = refinery_names), alpha = 0.5, color="#e6550d") +
  scale_size_area(max_size = 10) +
  scale_y_continuous(limits=c(-10,70), breaks = c(20,40,60)) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(color = "#e6550d"),
    plot.subtitle = element_text(color = "#e6550d"),
    axis.title = element_text(color = "#6a51a3",
                              hjust = -0.2)) +
  xlab("") +
  labs(title="Toxic emissions", subtitle="Hover for details", color="#e6550d") +
  ylab("% in poverty (arrow: local; point: state)") +
  geom_hline(yintercept=0, size=0.1, color="#e6550d") +
  coord_flip()

ca_poverty_interactive <- ggiraph(code = print(ca_poverty_gg), height_svg=5,
                                  hover_css = "cursor:pointer;fill-opacity:0.8;stroke:#e6550d",
                                  tooltip_extra_css = "background-color:#f8f8f8;color:#e6550d;font-family:'Helvetica';font-size:10px;padding:2px")
print(ca_poverty_interactive)

la_poverty_gg <- ggplot(la_chart) +
  geom_segment(color="#6a51a3", aes(xend=refinery_names, x=reorder(refinery_names, total_releases_mil), y=state_pc_poverty_pop, yend=pct_poverty), arrow = arrow(length = unit(0.2,"cm"), type="closed")) +
  geom_point( aes(x=refinery_names, y=state_pc_poverty_pop), color="#6a51a3", size=1.5 ) +
  geom_point_interactive(aes(tooltip = paste0("Toxic waste released (million lbs): ", round(total_releases_mil,2)),
                             size = total_releases_mil, x=refinery_names, y=0, data_id = refinery_names), alpha = 0.5, color="#e6550d") +
  scale_size_area(max_size = 10) +
  scale_y_continuous(limits=c(-10,70), breaks = c(20,40,60)) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(color = "#e6550d"),
    plot.subtitle = element_text(color = "#e6550d"),
    axis.title = element_text(color = "#6a51a3",
                              hjust = -0.2)) +
  xlab("") +
  labs(title="Toxic emissions", subtitle="Hover for details", color="#e6550d") +
  ylab("% in poverty (arrow: local; point: state)") +
  geom_hline(yintercept=0, size=0.1, color="#e6550d") +
  coord_flip()

la_poverty_interactive <- ggiraph(code = print(la_poverty_gg), height_svg=5,
                                  hover_css = "cursor:pointer;fill-opacity:0.8;stroke:#e6550d",
                                  tooltip_extra_css = "background-color:#f8f8f8;color:#e6550d;font-family:'Helvetica';font-size:10px;padding:2px")
print(la_poverty_interactive)

# plotting charts for race
tx_minority_gg <- ggplot(tx_chart) +
  geom_segment(color="#006d2c", aes(xend=refinery_names, x=reorder(refinery_names, total_releases_mil), y=state_pc_minority_pop, yend=pct_minority), arrow = arrow(length = unit(0.2,"cm"), type="closed")) +
  geom_point( aes(x=refinery_names, y=state_pc_minority_pop), color="#006d2c", size=1.5 ) +
  geom_point_interactive(aes(tooltip = paste0("Toxic waste released (million lbs): ", round(total_releases_mil,2)),
                             size = total_releases_mil, x=refinery_names, y=0, data_id = refinery_names), alpha = 0.5, color="#e6550d") +
  scale_size_area(max_size = 10) +
  scale_y_continuous(limits=c(-10,100), breaks = c(20,40,60,80)) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(color = "#e6550d"),
    plot.subtitle = element_text(color = "#e6550d"),
    axis.title = element_text(color = "#006d2c",
                              hjust = -0.2)) +
  xlab("") +
  labs(title="Toxic emissions", subtitle="Hover for details", color="#e6550d") +
  ylab("% racial minority (arrow: local; point: state)") +
  geom_hline(yintercept=0, size=0.1, color="#e6550d") +
  coord_flip()

tx_minority_interactive <- ggiraph(code = print(tx_minority_gg), height_svg=5,
                                   hover_css = "cursor:pointer;fill-opacity:0.8;stroke:#e6550d",
                                   tooltip_extra_css = "background-color:#f8f8f8;color:#e6550d;font-family:'Helvetica';font-size:10px;padding:2px")
print(tx_minority_interactive)


ca_minority_gg <- ggplot(ca_chart) +
  geom_segment(color="#006d2c", aes(xend=refinery_names, x=reorder(refinery_names, total_releases_mil), y=state_pc_minority_pop, yend=pct_minority), arrow = arrow(length = unit(0.2,"cm"), type="closed")) +
  geom_point( aes(x=refinery_names, y=state_pc_minority_pop), color="#006d2c", size=1.5 ) +
  geom_point_interactive(aes(tooltip = paste0("Toxic waste released (million lbs): ", round(total_releases_mil,2)),
                             size = total_releases_mil, x=refinery_names, y=0, data_id = refinery_names), alpha = 0.5, color="#e6550d") +
  scale_size_area(max_size = 10) +
  scale_y_continuous(limits=c(-10,100), breaks = c(20,40,60,80)) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(color = "#e6550d"),
    plot.subtitle = element_text(color = "#e6550d"),
    axis.title = element_text(color = "#006d2c",
                              hjust = -0.2)) +
  xlab("") +
  labs(title="Toxic emissions", subtitle="Hover for details", color="#e6550d") +
  ylab("% racial minority (arrow: local; point: state)") +
  geom_hline(yintercept=0, size=0.1, color="#e6550d") +
  coord_flip()

ca_minority_interactive <- ggiraph(code = print(ca_minority_gg), height_svg=5,
                                   hover_css = "cursor:pointer;fill-opacity:0.8;stroke:#e6550d",
                                   tooltip_extra_css = "background-color:#f8f8f8;color:#e6550d;font-family:'Helvetica';font-size:10px;padding:2px")
print(ca_minority_interactive)

la_minority_gg <- ggplot(la_chart) +
  geom_segment(color="#006d2c", aes(xend=refinery_names, x=reorder(refinery_names, total_releases_mil), y=state_pc_minority_pop, yend=pct_minority), arrow = arrow(length = unit(0.2,"cm"), type="closed")) +
  geom_point( aes(x=refinery_names, y=state_pc_minority_pop), color="#006d2c", size=1.5 ) +
  geom_point_interactive(aes(tooltip = paste0("Toxic waste released (million lbs): ", round(total_releases_mil,2)),
                             size = total_releases_mil, x=refinery_names, y=0, data_id = refinery_names), alpha = 0.5, color="#e6550d") +
  scale_size_area(max_size = 10) +
  scale_y_continuous(limits=c(-10,100), breaks = c(20,40,60,80)) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(color = "#e6550d"),
    plot.subtitle = element_text(color = "#e6550d"),
    axis.title = element_text(color = "#006d2c",
                              hjust = -0.2)) +
  xlab("") +
  labs(title="Toxic emissions", subtitle="Hover for details", color="#e6550d") +
  ylab("% racial minority (arrow: local; point: state)") +
  geom_hline(yintercept=0, size=0.1, color="#e6550d") +
  coord_flip()

la_minority_interactive <- ggiraph(code = print(la_minority_gg), height_svg=5,
                                   hover_css = "cursor:pointer;fill-opacity:0.8;stroke:#e6550d",
                                   tooltip_extra_css = "background-color:#f8f8f8;color:#e6550d;font-family:'Helvetica';font-size:10px;padding:2px")
print(la_minority_interactive)



# making poverty and race charts like the above, but for most polluting refineries overall, not by state

# creating df for top 10,15,20 refineries by toxic released
all_chart <- refineries_compare %>%
  filter(radius==3) %>%
  arrange(desc(total_releases_mil)) %>%
  head(15)

#creating new columns with edited refinery names
all_chart <- all_chart %>%
  mutate(refinery_names = c("Blanchard (Texas City, TX)", "Delaware City (Delaware City, DE)", "Phillips 66 (Lindon, NJ)", "Valero (Corpus Christi, TX)", "Chevron (El Segundo, CA)", "ExxonMobil (Baton Rouge, LA)","ExxonMobil (Baytown, TX)", "Valero (Texas City,TX)", "Chevron (Pascagoula, MS)", "Citgo (Sulphur,LA)", "Diamond Shamrock (Sunray, TX)", "Phillips 66 (Westlake, LA)",  "Motiva (Port Arthur, TX)", "Flint Hills West (Corpus Christi, TX)", "Paulsboro (Paulsboro, NJ)"))

#adding U.S. rates for person in poverty and minorities
# pct of persons in poverty nationally: 13.82
# pct of nonwhite persons nationally: 35.33

all_chart <- all_chart %>%
  mutate(nat_pc_poverty = 13.82, nat_pc_minority = 35.33)

# creating chart for poverty 
all_poverty_gg <- ggplot(all_chart) +
  geom_segment(color="#6a51a3", aes(xend=refinery_names, x=reorder(refinery_names, total_releases_mil), y=state_pc_poverty_pop, yend=pct_poverty), arrow = arrow(length = unit(0.2,"cm"), type="closed")) +
  geom_point( aes(x=refinery_names, y=state_pc_poverty_pop), color="#6a51a3", size=1.5 ) +
  geom_point_interactive(aes(tooltip = paste0("Toxic waste released (million lbs): ", round(total_releases_mil,2)),
                             size = total_releases_mil, x=refinery_names, y=0, data_id = refinery_names), alpha = 0.5, color="#e6550d") +
  scale_size_area(max_size = 10) +
  scale_y_continuous(limits=c(-10,70), breaks = c(10,20,40,60)) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(color = "#e6550d",
                              hjust = -0.05),
    plot.subtitle = element_text(color = "#e6550d"),
    axis.title = element_text(color = "#6a51a3",
                              hjust = -0.2)) +
  xlab("") +
  labs(title="Toxic emissions", subtitle="Hover for details", color="#e6550d") +
  ylab("% in poverty (arrow: local; point: state)") +
  geom_hline(yintercept=0, size=0.1, color="#e6550d") +
  coord_flip()

all_poverty_interactive <- ggiraph(code = print(all_poverty_gg), height_svg=4.5,
                                   hover_css = "cursor:pointer;fill-opacity:0.8;stroke:#e6550d",
                                   tooltip_extra_css = "background-color:#f8f8f8;color:#e6550d;font-family:'Helvetica';font-size:10px;padding:2px")
print(all_poverty_interactive)

saveWidget(all_poverty_interactive, "all_poverty_interactive.html", selfcontained = TRUE, libdir = NULL, background = "white")


# creating chart for minority
all_minority_gg <- ggplot(all_chart) +
  geom_segment(color="#006d2c", aes(xend=refinery_names, x=reorder(refinery_names, total_releases_mil), y=state_pc_minority_pop, yend=pct_minority), arrow = arrow(length = unit(0.2,"cm"), type="closed")) +
  geom_point( aes(x=refinery_names, y=state_pc_minority_pop), color="#006d2c", size=1.5 ) +
  geom_point_interactive(aes(tooltip = paste0("Toxic waste released (million lbs): ", round(total_releases_mil,2)),
                             size = total_releases_mil, x=refinery_names, y=0, data_id = refinery_names), alpha = 0.5, color="#e6550d") +
  scale_size_area(max_size = 10) +
  scale_y_continuous(limits=c(-10,100), breaks = c(20,40,60,80)) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(color = "#e6550d",
                              hjust = -0.05),
    plot.subtitle = element_text(color = "#e6550d"),
    axis.title = element_text(color = "#006d2c",
                              hjust = -0.2)) +
  xlab("") +
  labs(title="Toxic emissions", subtitle="Hover for details", color="#e6550d") +
  ylab("% racial minority (arrow: local; point: state)") +
  geom_hline(yintercept=0, size=0.1, color="#e6550d") +
  coord_flip()

all_minority_interactive <- ggiraph(code = print(all_minority_gg), height_svg=4.5,
                                    hover_css = "cursor:pointer;fill-opacity:0.8;stroke:#e6550d",
                                    tooltip_extra_css = "background-color:#f8f8f8;color:#e6550d;font-family:'Helvetica';font-size:10px;padding:2px")
print(all_minority_interactive)

saveWidget(all_minority_interactive, "all_minority_interactive.html", selfcontained = TRUE, libdir = NULL, background = "white")


  
# try overall charts compared to US poverty and minority rates instead of states

# creating chart for poverty 
all_poverty_gg <- ggplot(all_chart) +
  geom_segment(color="#6a51a3", aes(xend=refinery_names, x=reorder(refinery_names, total_releases_mil), y=nat_pc_poverty, yend=pct_poverty), arrow = arrow(length = unit(0.2,"cm"), type="closed")) +
  geom_point( aes(x=refinery_names, y=nat_pc_poverty), color="#6a51a3", size=1.5 ) +
  geom_point_interactive(aes(tooltip = paste0("Toxic waste released (million lbs): ", round(total_releases_mil,2)),
                             size = total_releases_mil, x=refinery_names, y=0, data_id = refinery_names), alpha = 0.5, color="#e6550d") +
  scale_size_area(max_size = 10) +
  scale_y_continuous(limits=c(-10,70), breaks = c(10,20,40,60)) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(color = "#e6550d"),
    plot.subtitle = element_text(color = "#e6550d"),
    axis.title = element_text(color = "#6a51a3",
                              hjust = -0.2)) +
  xlab("") +
  labs(title="Toxic emissions", subtitle="Hover for details", color="#e6550d") +
  ylab("% in poverty (arrow: local; point: national)") +
  geom_hline(yintercept=0, size=0.1, color="#e6550d") +
  coord_flip()

all_poverty_interactive <- ggiraph(code = print(all_poverty_gg), height_svg=4.5,
                                   hover_css = "cursor:pointer;fill-opacity:0.8;stroke:#e6550d",
                                   tooltip_extra_css = "background-color:#f8f8f8;color:#e6550d;font-family:'Helvetica';font-size:10px;padding:2px")
print(all_poverty_interactive)


# creating chart for minority
all_minority_gg <- ggplot(all_chart) +
  geom_segment(color="#006d2c", aes(xend=refinery_names, x=reorder(refinery_names, total_releases_mil), y=nat_pc_minority, yend=pct_minority), arrow = arrow(length = unit(0.2,"cm"), type="closed")) +
  geom_point( aes(x=refinery_names, y=nat_pc_minority), color="#006d2c", size=1.5 ) +
  geom_point_interactive(aes(tooltip = paste0("Toxic waste released (million lbs): ", round(total_releases_mil,2)),
                             size = total_releases_mil, x=refinery_names, y=0, data_id = refinery_names), alpha = 0.5, color="#e6550d") +
  scale_size_area(max_size = 10) +
  scale_y_continuous(limits=c(-10,100), breaks = c(20,40,60,80)) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(color = "#e6550d"),
    plot.subtitle = element_text(color = "#e6550d"),
    axis.title = element_text(color = "#006d2c",
                              hjust = -0.2)) +
  xlab("") +
  labs(title="Toxic emissions", subtitle="Hover for details", color="#e6550d") +
  ylab("% racial minority (arrow: local; point: national)") +
  geom_hline(yintercept=0, size=0.1, color="#e6550d") +
  coord_flip()

all_minority_interactive <- ggiraph(code = print(all_minority_gg), height_svg=4.5,
                                    hover_css = "cursor:pointer;fill-opacity:0.8;stroke:#e6550d",
                                    tooltip_extra_css = "background-color:#f8f8f8;color:#e6550d;font-family:'Helvetica';font-size:10px;padding:2px")
print(all_minority_interactive)


# making scatter chart interactive for minority and poverty

poverty_plot <- ggplot(subset(refineries_compare, radius==3), aes(x=poverty_compare, y=total_releases_mil)) +
  geom_point_interactive(aes(tooltip = paste0(facility_name, "<br>", "Toxic waste released (million lbs): ", round(total_releases_mil,2), "<br>",round(poverty_compare,2), " times % in poverty, compared to state"),
                             size = persons_below_poverty_level, x=poverty_compare, y=total_releases_mil, data_id = frs_id), alpha = 0.5, color="#54278f") +
  ggtitle("Population within 3 miles of refinery") +
  scale_x_continuous(breaks = c(0,0.5,1,2,3,4,5), labels = c("Zero","Half","","Twice","3 times","4 times","5 times")) +
  scale_y_continuous(labels = comma) +
  scale_size_area(max_size = 10, guide = FALSE) +
  xlab("% population in poverty, compared to % for entire state") +
  ylab("Toxic releases (million lbs)") +
  geom_vline(xintercept = 1, linetype = "dotted") +
  theme_minimal()
  
poverty_plot_interactive <- ggiraph(code = print(poverty_plot), height_svg=4.5,
                                      hover_css = "cursor:pointer;fill-opacity:0.8;stroke:#e6550d",
                                      tooltip_extra_css = "background-color:#f8f8f8;color:#e6550d;font-family:'Helvetica';font-size:10px;padding:2px")
print(poverty_plot_interactive)

saveWidget(poverty_plot_interactive, "poverty_plot_interactive.html", selfcontained = TRUE, libdir = NULL, background = "white")

minority_plot <- ggplot(subset(refineries_compare, radius==3), aes(x=minority_compare, y=total_releases_mil)) +
  geom_point_interactive(aes(tooltip = paste0(facility_name, "<br>", "Toxic waste released (million lbs): ", round(total_releases_mil,2), "<br>",round(minority_compare,2), " times % racial minority, compared to state"),
                             size = minority_pop, x=minority_compare, y=total_releases_mil, data_id = frs_id), alpha = 0.5, color="#006d2c") +
  ggtitle("Population within 3 miles of refinery") +
  scale_x_continuous(breaks = c(0,0.5,1,2,3,4), labels = c("Zero","Half","","Twice","3 times","4 times")) +
  scale_y_continuous(labels = comma) +
  scale_size_area(max_size = 10, guide = FALSE) +
  xlab("% minority population, compared to % for entire state") +
  ylab("Toxic releases (million lbs)") +
  geom_vline(xintercept = 1, linetype = "dotted") +
  theme_minimal()

minority_plot_interactive <- ggiraph(code = print(minority_plot), height_svg=4.5,
                                     hover_css = "cursor:pointer;fill-opacity:0.8;stroke:#e6550d",
                                     tooltip_extra_css = "background-color:#f8f8f8;color:#e6550d;font-family:'Helvetica';font-size:10px;padding:2px")
print(minority_plot_interactive)

saveWidget(minority_plot_interactive, "minority_plot_interactive.html", selfcontained = TRUE, libdir = NULL, background = "white")


#finding relationship between refinery location and amount of pollution and presence of African Americans 

# population
population <- get_decennial(geography = "state", 
                            variables = "",
                            year = 2010) %>%
  rename(population = value)  %>%
  select(1,2,4)

# black population (black alone, not hispanic)
black_population <- get_decennial(geography = "state",
                                  variables = "P0090006",
                                  year = 2010) %>%
  rename(black_population = value) %>%
  select(1,2,4)

# join these  
black_population <- inner_join(black_population,population)

black_population <- black_population %>%
  select(1,2,5,3)

black_population <- black_population %>%
  mutate(pct_black_population = round(black_population/population*100,2))

#adding black population onto refinery compare

refineries_demo_release <- read_csv('refineries_demo_release_clean.csv')

black_population <- inner_join(black_population, state_abb, by = c("NAME" = "State"))

refineries_black_pop <- inner_join(refineries_demo_release, black_population, by = c("state" = "Abbreviation"))

refineries_black_pop <- refineries_black_pop %>%
  rename(state_black_pop = black_population,
         state_pct_black_pop = pct_black_population)

refineries_black_pop <- refineries_black_pop %>%
  mutate(pct_aa = pct_aa*100)

refineries_black_pop <- refineries_black_pop %>%
  mutate(black_compare = pct_aa/state_pct_black_pop)

refineries_black_pop <- refineries_black_pop %>%
  rename(total_releases = `total_releases_on-off-site_lbs`) %>%
  mutate(total_releases_mil = total_releases/10^6)


# plotting relationship between refinery location and black population
aa_plot <- ggplot(subset(refineries_black_pop, radius==3), aes(x=black_compare, y=total_releases_mil)) +
  geom_point_interactive(aes(tooltip = paste0(facility_name, "<br>", "Toxic waste released (million lbs): ", round(total_releases_mil,2), "<br>",round(black_compare,2), " times % black population, compared to state"),
                             size = african_american_pop, x=black_compare, y=total_releases_mil, data_id = frs_id), alpha = 0.5, color="red") +
  ggtitle("Population within 3 miles of refinery") +
  scale_x_continuous(breaks = c(0,0.5,1,2,3,4,5,6,7,8), labels = c("Zero","Half","","Twice","3 times","4 times", "5 times", "6 times", "7 times", "8 times")) +
  scale_y_continuous(labels = comma) +
  scale_size_area(max_size = 10, guide = FALSE) +
  xlab("% black population, compared to % for entire state") +
  ylab("Toxic releases (million lbs)") +
  geom_vline(xintercept = 1, linetype = "dotted") +
  theme_minimal()

aa_plot_interactive <- ggiraph(code = print(aa_plot), height_svg=4.5,
                                       hover_css = "cursor:pointer;fill-opacity:0.8;stroke:#e6550d",
                                       tooltip_extra_css = "background-color:#f8f8f8;color:#e6550d;font-family:'Helvetica';font-size:10px;padding:2px")
print(aa_plot_interactive)

# save chart as a web page
saveWidget(aa_plot_interactive, "aa_plot_interactive.html", selfcontained = TRUE, libdir = NULL, background = "white")



refineries_compare_3mile <- refineries_compare %>%
  filter(radius==3)

refineries_black_pop_3mile <- refineries_black_pop %>%
  filter(radius==3)




