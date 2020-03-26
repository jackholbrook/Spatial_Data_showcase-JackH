library(tidyverse)
PersonData <- read_rds('Data/PersonData_111A.Rds')
HHData <- read_rds('Data/HHData_111A.Rds')
hh_bgDensity <- read_rds('Data/hh_bgDensity.Rds')
personHHData <- left_join(PersonData, HHData) %>%
  left_join(hh_bgDensity)
library(sf)
counties_shp <- st_read("Data/counties.shp")
plot(counties_shp["NAME"])

library(mapview)
# above, code does not have background leaflet

# below, code does have background leaflet
# above, code does not have background leaflet

# below, code does have background leaflet

ppl_pr_county <- personHHData %>% 
  group_by(CTFIP, County) %>%
  summarise(n_ppl=n())

prhh_aggreg <- personHHData %>% 
  mutate(Sum_VMT = DriveAlone_Dist + Driveothers_Dist +Passenger_Dist) %>%
  select(hhid, pnum, CTFIP, County, bg_group,Sum_Trips, Sum_PMT, Sum_VMT, bornUSA, starts_with("pers")) %>%
  group_by(CTFIP) %>%
  summarise_at(vars(-hhid, -pnum, -County, -bg_group), mean) %>%
  left_join(ppl_pr_county) 

county_prhh_shp <- counties_shp %>% left_join(prhh_aggreg)


mapview(county_prhh_shp, zcol = "bornUSA", legend=TRUE, label= as.character(county_prhh_shp$NAME), 
        popup = leafpop::popupTable(x = county_prhh_shp, zcol = c("bornUSA", "n_ppl")))


urban_data <- personHHData %>% 
  filter(bg_group == "Urban") 
urban_ppl_pr_county <- urban_data %>%
  group_by(CTFIP, County) %>% summarise(n_ppl=n())
urban_aggreg <- urban_data %>% 
  mutate(Sum_VMT = DriveAlone_Dist + Driveothers_Dist +Passenger_Dist) %>%
  select(hhid, pnum, CTFIP, County, bg_group,Sum_Trips, Sum_PMT, Sum_VMT, bornUSA, starts_with("pers")) %>%
  group_by(CTFIP) %>%
  summarise_at(vars(-hhid, -pnum, -County, -bg_group), mean)%>%
  left_join(urban_ppl_pr_county)
urban_shp <- counties_shp %>% left_join(urban_aggreg)
rm(urban_data, urban_ppl_pr_county, urban_aggreg)

#suburban blockgroup group

suburb_data <- personHHData %>% 
  filter(bg_group == "Suburban") 
suburb_ppl_pr_county <- suburb_data %>%
  group_by(CTFIP, County) %>% 
  summarise(n_ppl=n())
suburb_aggreg <- suburb_data %>% 
  mutate(Sum_VMT = DriveAlone_Dist + Driveothers_Dist +Passenger_Dist) %>%
  select(hhid, pnum, CTFIP, County, bg_group,Sum_Trips, Sum_PMT, Sum_VMT, bornUSA, starts_with("pers")) %>%
  group_by(CTFIP) %>%
  summarise_at(vars(-hhid, -pnum, -County, -bg_group), mean)%>%
  left_join(suburb_ppl_pr_county)
suburb_shp <- counties_shp %>% left_join(suburb_aggreg)
rm(suburb_data, suburb_ppl_pr_county, suburb_aggreg)

# Exurban shapefile

exurb_data <- personHHData %>% 
  filter(bg_group == "Exurban")
exurb_ppl_pr_county <- exurb_data %>% 
  group_by(CTFIP, County) %>% 
  summarise(n_ppl=n())
exurb_aggreg <- exurb_data %>% 
  mutate(Sum_VMT = DriveAlone_Dist + Driveothers_Dist +Passenger_Dist) %>%
  select(hhid, pnum, CTFIP, County, bg_group,Sum_Trips, Sum_PMT, Sum_VMT, bornUSA, starts_with("pers")) %>%
  group_by(CTFIP) %>%
  summarise_at(vars(-hhid, -pnum, -County, -bg_group), mean)%>%
  left_join(exurb_ppl_pr_county)
exurb_shp <- counties_shp %>% left_join(exurb_aggreg)
rm(exurb_data, exurb_ppl_pr_county, exurb_aggreg)

# Rural Shapefile

rural_data <- personHHData %>% 
  filter(bg_group == "Rural") 
rural_ppl_pr_county <- rural_data %>% 
  group_by(CTFIP, County) %>%
  summarise(n_ppl=n())
rural_aggreg <- rural_data %>% 
  mutate(Sum_VMT = DriveAlone_Dist + Driveothers_Dist +Passenger_Dist) %>%
  select(hhid, pnum, CTFIP, County, bg_group,Sum_Trips, Sum_PMT, Sum_VMT, bornUSA, starts_with("pers")) %>%
  group_by(CTFIP) %>%
  summarise_at(vars(-hhid, -pnum, -County, -bg_group), mean)%>%
  left_join(rural_ppl_pr_county)
rural_shp <- counties_shp %>% left_join(rural_aggreg)
rm(rural_data, rural_ppl_pr_county, rural_aggreg) 

#Dataclean for biking statisitcs
# biking data for the final project:

# set up urban block group data for biking:

urban_data <- personHHData %>% 
  filter(bg_group == "Urban") 
urban_ppl_pr_county <- urban_data %>%
  group_by(CTFIP, County) %>% summarise(n_ppl=n())
bike_aggreg <- urban_data %>% 
  mutate(BikingTripsWk_na0= if_else(is.na(BikeTripsWk),true = 0, false = BikeTripsWk), 
         Weekly_biking = BikingTripsWk_na0 + Bike_trips) %>%
  select(hhid, pnum, CTFIP, County, bg_group, Sum_Trips, Sum_PMT, Bike_Dist, Weekly_biking, Bike_trips, bornUSA,) %>%
  group_by(CTFIP) %>%
  summarise_at(vars(-hhid, -pnum, -County, -bg_group), mean)%>%
  left_join(urban_ppl_pr_county)
bikeurban_shp <- counties_shp %>% left_join(bike_aggreg)


bikeurban_TripMap <- mapview(bikeurban_shp, zcol = "Weekly_biking", 
                             legend = TRUE, layer.name = "Biking")