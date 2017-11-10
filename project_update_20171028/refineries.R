
# loading required packages
library(readr)
library(dplyr)
library(ggplot2)

# reading csvs
refineries_demo <- read_csv('refineries_demo.csv')
refineries_names <- read_csv('refineries_names.csv')

# joining the two
refineries_demo_names <- inner_join(refineries_demo, refineries_names, by=c("frs_id" = "frs_id"))

#deleting all those pesky nulls --- why are they there?? 
# A - BOTH OF YOUR CSVs have hundreds of empty rows at the end, so every null gets joined to every null from the 
# other data frame in the join. Let's look at why that was the case, just to be sure nothing went wrong earlyer
refineries_demo_names <- refineries_demo_names[-c(755:170294),]

# DON'T DO IT BY COUNTING ROWS, INSTEAD:
refineries_demo_names <- refineries_demo_names %>%
  filter(!is.na(frs_id))

#reordering columns, so that facility_name is first
refineries_demo_names <- refineries_demo_names[c(1,46,2:45)]

# YOUR METHOD IS OK, BUT I WOULD DO THIS IN DPLYR ALSO:
refineries_demo_names <- refineries_demo_names %>%
  select(1,46,2:45)

#importing toxic release data, must join two 
tri_release <- read_csv('TRI_petroleum_release.csv')
tri_ID <- read_csv('tri_frs_ids.CSV')

#joining them and ditching all everything that isn't a refinery in 'tri_release'
refineries_release <- inner_join(tri_ID, tri_release, by=c("V_TRI_FORM_R_EZ.TRI_FACILITY_ID" = "TRIF ID"))

#joining release data with refinery demo data
# WHY DO WE DROP FROM 754 to 444 REFINERIES HERE. IT'S PROBABLY OK, I JUST WANT TO UNDERSTAND
refineries_demo_release <- inner_join(refineries_demo_names, refineries_release, by=c("frs_id" = "V_TRI_FORM_R_EZ.FRS_ID"))

#exporting refineries_demo_release for cleaning
write.csv(refineries_demo_release, file = 'refineries_demo_release.csv', row.names = FALSE)
# AM I CORRECT IN THINKING THAT THE CLEANING IS ALL CHANGING COLUMN NAMES AND ORDER?
# IF SO, YOU CAN DO IN R VERY EASILY
# CHANGE COLUMN NAMES LIKE THIS:
names(refineries_demo_release) <- c("field1","field2","field3","etc")
# WHERE FIELD1, FIELD2, FIELD3 ETC ARE YOUR NEW COLUMN NAMES.
# THEN USE DPLYR SELECT TI CHANGE ORDER LIKE I did in lines 27-28

#bringing clean data back
refineries_master <- read_csv('refineries_demo_release_clean.csv')

#creating three separate data frames for each radius
refineries_1mile <- refineries_master %>%
  filter(radius == '1')

refineries_3mile <- refineries_master %>%
  filter(radius == '3')

refineries_5mile <- refineries_master %>%
  filter(radius == '5')

# create data frame for correlation matrix
refineries_1mile_cor <- refineries_1mile %>%
  select(5,19,21,40) %>%
  rename(tri = `total_releases_on-off-site_lbs`)

# correlation matrix - not working because of some problem with tri data
round(cor(refineries_1mile_cor),2)

# sample scatterplot
ggplot(refineries_1mile_cor, aes(y=pct_15k_25k,x=tri)) +
  geom_point() +
  geom_smooth()










