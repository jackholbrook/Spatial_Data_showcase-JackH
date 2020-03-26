library(tidyverse)
library(ggplot2)
library(rmarkdown)
library(summarytools)
library(mapview)

#load data
PersonData <- read_rds('Data/PersonData_111A.Rds')
HHData <- read_rds('Data/HHData_111A.Rds')
hh_bgDensity <- read_rds('Data/hh_bgDensity.Rds')
personHHData <- left_join(PersonData, HHData) %>% left_join(hh_bgDensity)


#assign objects, 
# revisit this, I see unharnesed potential here
PersonData %>% select(BikeTripsWk) %>% filter(BikeTripsWk >=1)
PersonData %>% select(Bike_trips) %>% filter(Bike_trips >0)
personHHData %>% select(HH_nBikes) %>% filter(HH_nBikes >0)

# create objects to wrangle data

bikesdescriptivestats <- select(personHHData, Bike_trips, Bike_Dist, 
                                BikeTripsWk, Walk_trips, Walk_Dist, 
                                Sum_Trips, Sum_PMT, County, hhid, 
                                bg_group, Student, Employed, workday, 
                                FlexSched, EducationCompl) %>% filter(Bike_trips >0)
#bikesdescrptivestats is used for everything.

personHHData %>% summarize(mean(HH_nBikes)) # see the average number of bikes in each respondents household

BikeTravellingbyCounty <- bikesdescriptivestats %>% group_by(County) %>%
  summarise(biking_trips = sum(Bike_trips), biking_miles = sum(Bike_Dist))
# the total trips and total distance of bike commuting in each county

UrbanBikers <- bikesdescriptivestats  %>% group_by(County, bg_group)%>% 
  filter(bg_group == 'Urban') %>% summarise(bikestripsrespondents = n())

SuburbanBikers <- bikesdescriptivestats  %>% group_by(County, bg_group)%>% 
  filter(bg_group == 'Suburban') %>% summarise(bikestripsrespondents = n())

ExurbanBikers <- bikesdescriptivestats  %>% group_by(County, bg_group)%>% 
  filter(bg_group == 'Exurban') %>% summarise(bikestripsrespondents = n())

RuralBikers <- bikesdescriptivestats  %>% group_by(County, bg_group)%>% 
  filter(bg_group == 'Rural') %>% summarise(bikestripsrespondents = n())



# individually selected counties that satisfied the statistical criteria in section 1. 
# contructed ggplots for each of these counties. 
# counties were: San Francisco, Los Angeles, Alameda & Santa Clara. 


#SF

BikesSanFran <- filter(bikesdescriptivestats, County == 'San Francisco', bg_group == 'Urban') %>% 
  select(Bike_trips:EducationCompl)

BikesSanFranFiltered <- filter(bikesdescriptivestats, County == 'San Francisco', 
                               bg_group == 'Urban', Sum_PMT <100, Bike_Dist <17.5) 
# this object has outliers removed

BikesSanFran %>% 
  summarise(Bike_trips = mean(Bike_trips),
    Sum_trips = mean(Sum_Trips), 
            Biking_distance = mean(Bike_Dist),
            tot.distance = mean(Sum_PMT))

BikesSanFran %>% group_by(Bike_trips) %>% 
  summarise(Sum_trips = mean(Sum_Trips), 
            Bikingdistance = mean(Bike_Dist), 
            tot.distance = mean(Sum_PMT))

BikesSanFranFiltered %>% 
  summarise(Bike_trips = mean(Bike_trips),
            Sum_trips = mean(Sum_Trips), 
            Biking_distance = mean(Bike_Dist),
            tot.distance = mean(Sum_PMT))

BikesSanFranFiltered %>% group_by(Bike_trips) %>% 
  summarise(Sum_trips = mean(Sum_Trips), 
            Bikingdistance = mean(Bike_Dist), 
            tot.distance = mean(Sum_PMT))

gglineSF <- ggplot(BikesSanFranFiltered, aes(Bike_trips, Sum_PMT, color = Bike_trips))+
  ggtitle("Bike Trips vs. Total Commuting Miles SF") +
  geom_density(aes(y= ..count..))

ggsave(filename = "gglineSF.png", plot = gglineSF)

ggplotSF <- ggplot(BikesSanFranFiltered, aes(Bike_trips, Sum_Trips, fill = Bike_trips))+
  ggtitle("Bike Trips vs. All Trips San Francisco")+
  stat_summary(fun.y = mean, geom = "bar")+
  scale_fill_gradient(low="darkred",high="lightgreen")

ggsave(filename = "ggpltSF.png", plot = ggplotSF)

# LA

BikesLA <- filter(bikesdescriptivestats, County == 'Los Angeles', bg_group == 'Urban') %>%  
  select(Bike_trips:EducationCompl) 
                  
BikesLAfiltered <- filter(bikesdescriptivestats, County == 'Los Angeles', bg_group == 'Urban',
                          Sum_PMT <300, Bike_Dist <40) 
# this object has outliers removed


BikesLA %>% 
  summarise(Bike_trips = mean(Bike_trips),
            Sum_trips = mean(Sum_Trips), 
            Biking_distance = mean(Bike_Dist),
            tot.distance = mean(Sum_PMT))

BikesLA %>% group_by(Bike_trips) %>% 
  summarise(Sum_trips = mean(Sum_Trips), 
            Bikingdistance = mean(Bike_Dist), 
            tot.distance = mean(Sum_PMT))

BikesLAfiltered %>% 
  summarise(Bike_trips = mean(Bike_trips),
            Sum_trips = mean(Sum_Trips), 
            Biking_distance = mean(Bike_Dist),
            tot.distance = mean(Sum_PMT))

BikesLAfiltered %>% group_by(Bike_trips) %>% 
  summarise(Sum_trips = mean(Sum_Trips), 
            Bikingdistance = mean(Bike_Dist), 
            tot.distance = mean(Sum_PMT))

gglineLA <- ggplot(BikesLAfiltered, aes(Bike_trips, Sum_PMT, color = Bike_trips))+
  ggtitle("Bike Trips vs. Total Commuting Miles LA") +
  geom_density(aes(y= ..count..))

ggsave(filename = "gglineLA.png", plot = gglineLA)

ggplotLA <- ggplot(BikesLAfiltered, aes(Bike_trips, Sum_Trips, fill = Bike_trips))+
  ggtitle("Bike Trips vs. all trips Los Angeles")  +                
  stat_summary(fun.y = mean, geom = "bar")+
  scale_fill_gradient(low="darkred",high="lightgreen")

ggsave(filename = "ggpltLA.png", plot = ggplotLA)
                  
# Alameda

BikesAlameda <-  filter(bikesdescriptivestats, County == 'Alameda', bg_group == 'Urban') %>%  
  select(Bike_trips:EducationCompl) 

BikesAlamedaFiltered <- filter(bikesdescriptivestats, County == 'Alameda', bg_group == 'Urban',
                                Sum_PMT < 75, Bike_Dist < 25) %>% select(Bike_trips:EducationCompl) 

BikesAlameda %>% 
  summarise(Bike_trips = mean(Bike_trips),
            Sum_trips = mean(Sum_Trips), 
            Biking_distance = mean(Bike_Dist),
            tot.distance = mean(Sum_PMT))

BikesAlameda %>% group_by(Bike_trips) %>% 
  summarise(Sum_trips = mean(Sum_Trips), 
            Bikingdistance = mean(Bike_Dist), 
            tot.distance = mean(Sum_PMT))

BikesAlamedaFiltered %>% 
  summarise(Bike_trips = mean(Bike_trips),
            Sum_trips = mean(Sum_Trips), 
            Biking_distance = mean(Bike_Dist),
            tot.distance = mean(Sum_PMT))

BikesAlamedaFiltered %>% group_by(Bike_trips) %>% 
  summarise(Sum_trips = mean(Sum_Trips), 
            Bikingdistance = mean(Bike_Dist), 
            tot.distance = mean(Sum_PMT))

gglineAl. <- ggplot(BikesAlamedaFiltered, aes(Bike_trips, Sum_PMT, color = Bike_trips))+
  ggtitle("Bike Trips vs. Total Commuting Miles Al.") +
  geom_density(aes(y= ..count..))

ggsave(filename = "gglineAl..png", plot = gglineAl.)


ggplotAlameda <- ggplot(BikesAlamedaFiltered, aes(Bike_trips, Sum_Trips, fill = Bike_trips))+
  ggtitle("Bike Trips vs. All Trips Alameda") +
  stat_summary(fun.y = mean, geom = "bar")+
  scale_fill_gradient(low="darkred",high="lightgreen")

ggsave(filename = "ggpltAlameda.png", plot = ggplotAlameda)

# Santa Clara

BikesSantaClara <-  filter(bikesdescriptivestats, County == 'Santa Clara', bg_group == 'Urban') %>%  
  select(Bike_trips:EducationCompl) 

BikesSantaClaraFiltered <-  filter(bikesdescriptivestats, County == 'Santa Clara', bg_group == 'Urban', 
                                   Sum_PMT < 250, Bike_Dist < 20) %>%  
                                   select(Bike_trips:EducationCompl) 
  
BikesSantaClara %>% 
  summarise(Bike_trips = mean(Bike_trips),
            Sum_trips = mean(Sum_Trips), 
            Biking_distance = mean(Bike_Dist),
            tot.distance = mean(Sum_PMT))

BikesSantaClara %>% group_by(Bike_trips) %>% 
  summarise(Sum_trips = mean(Sum_Trips), 
            Bikingdistance = mean(Bike_Dist), 
            tot.distance = mean(Sum_PMT))

BikesSantaClaraFiltered %>% 
  summarise(Bike_trips = mean(Bike_trips),
            Sum_trips = mean(Sum_Trips), 
            Biking_distance = mean(Bike_Dist),
            tot.distance = mean(Sum_PMT))

BikesSantaClaraFiltered %>% group_by(Bike_trips) %>% 
  summarise(Sum_trips = mean(Sum_Trips), 
            Bikingdistance = mean(Bike_Dist), 
            tot.distance = mean(Sum_PMT))

gglineSC. <- ggplot(BikesSantaClaraFiltered, aes(Bike_trips, Sum_PMT, color = Bike_trips))+
  ggtitle("Bike Trips vs. Total Commuting Miles SC.") +
  geom_density(aes(y= ..count..))

ggsave(filename = "gglineSC..png", plot = gglineSC.)

ggplotSantaClara <- ggplot(BikesSantaClaraFiltered, aes(Bike_trips, Sum_Trips, fill = Bike_trips))+
  ggtitle("Bike Trips vs. All trips Santa Clara") +
  stat_summary(fun.y = mean, geom = "bar")+
  scale_fill_gradient(low="darkred",high="lightgreen")

ggsave(filename = "ggpltSantaClara.png", plot = ggplotSantaClara)

###############

Countystats <- filter(bikesdescriptivestats, County == 'San Francisco', County == 'Los Angeles',
                      County == 'Alameda', County == 'Santa Clara', bg_group == 'Urban')

ggplot()

###############
# SUBURBAN DATA  

###############

# making summaries of the data: 

bikesdescriptivestats %>% descr(stats = c("mean", "sd", "IQR", "med", "max", "kurtosis", "N.valid")) %>% 
  summarytools::view(file = "Bike_stats.html")
