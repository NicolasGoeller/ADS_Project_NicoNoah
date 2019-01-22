## This script puts everything nicely together by constructiing plots

library(tidyverse)
library(stargazer)
library(hrbrthemes) 
library(extrafont)
library(memisc)
#install.packages(c("sf", "rnaturalearth", "rnaturalearthdata", "rgeos"))
#install.packages("rworldmap") #useful package to solve the geodata problem
library(sf)         
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
#library(rworldmap)

<<<<<<< HEAD
=======
#Codebook
?`codebook,factor-method`

EVS_final <- within(EVS_final,{
  description(eureg) <- "Geographical region"
  measurement(eureg) <- "nominal"
  missing.values(eureg) <- NA
})
description(Data)
codebook(Data)


EVS_final <- within(EVS_final,{
  description(EVS_final$sat) <- "Life satisfaction"
  wording(EVS_final$sat) <- "All things taken into account, how satisfied are you with your 
  life right now?"
  measurement(EVS_final$sat) <- "interval"
  missing.values(EVS_final$sat) <- NA
})
description(Data)
codebook(Data)
?measurement
#Plots: theme_ipsum() produces warnings, but nothing serious
>>>>>>> master
EVS_final <- read_rds("Data/EVS_final.rds")


#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------

###   1. Some simple bar plots

##    1.1 Plot 1
#Plots: theme_ipsum() produces warnings, but nothing serious

ggplot(EVS_final, aes(x = sat))+
  geom_bar()+
     #from next line, publication ready & nice
  labs(x = "Life satisfaction",
       title = "Titel",
       subtitle = "Untertitel",
       caption = "Credits to us") + 
  theme_ipsum(grid = "Y")


#--------------------------------------------------------------------------------------------------------
##    1.2 Plot 2
ggplot(EVS, aes(y = edu, x = siops))+
  geom_jitter()+
  geom_smooth(method = "lm", size = 1, se = T)+
  labs(x = "SIOPS-Index", 
       y = "Educational level",
       title = "Titel",
       subtitle = "Untertitel",
       caption = "Credits to us") + 
  theme_ipsum(grid = "Y")


warnings()
ggplot(EVS, aes(y = sat, x = siops, color = edu))+
  geom_jitter(alpha = 0.5)+
  geom_smooth(method = "lm")

EVS %>% 
  select(edu, sat, siops) %>% 
  group_by(edu) %>% 
  ggplot(aes(y = sat, x = siops))+
  geom_jitter()+
  geom_smooth(method = "lm")


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

###   2. Creating map

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

<<<<<<< HEAD
##    2.1 World dataset
=======
nat <- c("Albania", "Austria", "Armenia", "Belgium", "Bosnia Herzegovina", 
         "Bulgaria", "Belarus", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
         "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", 
         "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
         "Moldova", "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", 
         "Romania", "Serbia", "Slovak Republic", "Slovenia", "Spain", "Sweden", 
         "Switzerland", "Turkey", "Ukraine", "Macedonia", "United Kingdom", "Kosovo")

# World dataset
>>>>>>> master
world <- ne_countries(scale = "medium", returnclass = "sf")
euro <- ne_countries(scale = "medium", country = nat, returnclass = "sf")
class(world)#Bosnia Herzegovina (Bosnia and Herzegovina), Slovak Republic (Slovakia) and Serbia (Republic of Serbia) are missing

# add centroids
world <- st_centroid(world)
world <- cbind(world, st_coordinates(st_centroid(world$geometry)))

<<<<<<< HEAD
=======
?join

# plot Europe 
ggplot(data = eur) +
  geom_sf()
>>>>>>> master

#------------------------------------------------------------------------------------------------------------

## 2.2 Some world maps as training

# Basic map with country codes

world_points <- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

ggplot(data = world) +
  geom_sf()+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Europe map", 
          subtitle = "Life satisfaction across countries")+
  coord_sf(xlim = c(-20, 59), ylim = c(35, 71), expand = FALSE)+
  geom_text(data= world, aes(x=X, y=Y, label=brk_a3),
            color = "darkblue", fontface = "bold" , size = 2, check_overlap = FALSE)

# Countries and Income Group

ggplot(data = world) +
  geom_sf(aes(fill = income_grp))+
  labs(fill = "Income Group")+
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle("Europe map", 
          subtitle = "Countries and OECD Income Group")+
  coord_sf(xlim = c(-20, 59), ylim = c(35, 71), expand = FALSE)+
  geom_text(data= world, aes(x=X, y=Y, label=brk_a3),
            color = "black", fontface = "bold" , size = 3, check_overlap = FALSE)

#----------------------------------------------------------------------------------------------------------

## 2.3 Europe dataset 

nat_vis <- c("Albania", "Austria", "Armenia", "Belgium", "Bosnia and Herzegovina", 
         "Bulgaria", "Belarus", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
         "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", 
         "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
         "Moldova", "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", 
         "Romania", "Republic of Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", 
         "Switzerland", "Turkey", "Ukraine", "Macedonia", "United Kingdom", "Kosovo")

eur <- ne_countries(country = nat_vis ,scale = "medium", returnclass = "sf")

# rename countries (1) Bosnia and Herzegovina, (2) Serbia (which variables to rename (admin, geounit, subunit, format_en)?)
eur$sovereignt <- mapvalues(eur$sovereignt, from = c("Bosnia and Herzegovina", "Republic of Serbia"), 
                              to = c("Bosnia Herzegovina", "Serbia"))

# add centroids with coordinates X and Y
#eur$centroid <- st_centroid(eur$geometry)
eur <- cbind(eur, st_coordinates(st_centroid(eur$geometry)))


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## 2.4 Plot Europe

# basic plot of Europe  
ggplot(data = eur) +
  geom_sf() # need to zoom in 

# second plot of Europe (zoomed in properly)
ggplot(data = eur) +
  geom_sf() +
  ggtitle("Europe map")+
  coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## 2.5 Selecting variables from Europe dataset to 
#  merge with aggregate nat_data

# select necessary variables
eur_s <- dplyr::select(eur, sovereignt, geometry, adm0_a3, X, Y)

# investigate if selection has worked i.e., if map can be drawn
ggplot(data = eur_s) +
  geom_sf() +
  ggtitle("Europe map (simplfied dataset)")+
  coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)

# merge selected eur_s dataset with aggregate nat_data 

# Adding nation variable
eur_s$nation <- eur_s$sovereignt

# Joining nat_data and European geo_data to obtain new nat_data for visualisation (nat_data_vis)
nat_data_vis <- left_join(nat_data, eur_s, by = "nation")


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##    2.6 Create some plots

#     2.6.1 Countries and GDP per capita 
ggplot(data = nat_data_vis) +
  geom_sf(aes(fill = GDPpc))+
  labs(fill = "GDP per capita")+
  ggtitle("Europe Map", 
          subtitle = "Countries and GDP per capita")+
  coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)
  

#     2.6.2 Countries and Freedom House rating 2008 
nat_data_vis$fhrate08 <- as.numeric(nat_data_vis$fhrate08)

ggplot(data = nat_data_vis) +
  geom_sf(aes(fill = fhrate08))+
  labs(fill = "Freedom House Democracy Rating 2008")+
  ggtitle("Europe Map", 
          subtitle = "Countries and Freedom House Rating")+
  coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)


#     2.6.3 Countries and UNHDP HDI rating 2008 
nat_data_vis$fhrate08 <- as.numeric(nat_data_vis$fhrate08)

ggplot(data = nat_data_vis) +
  geom_sf(aes(fill = UNDP_HDI2008))+
  labs(fill = "HDI Index 2008")+
  ggtitle("Europe Map", 
          subtitle = "Countries and HDI Index")+
  coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)

#     2.6.4 Countries and life_sat

ggplot(data = nat_data_vis) +
  geom_sf(aes(fill = life_sat))+
  labs(fill = "Life satisfaction (from low to high)")+
  ggtitle("Europe Map", 
          subtitle = "Countries and Life Satisfaction")+
  coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)


#---------------------------------------------------------------------------------------------------------------------------------

##    2.7 Create plots with country indication 

# Countries and GDP per capita 
ggplot(data = nat_data_vis) +
  geom_sf(aes(fill = GDPpc))+
  labs(fill = "GDP per capita")+
  ggtitle("Europe Map", 
          subtitle = "Countries and GDP per capita")+
  coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE) +
  geom_text(data= nat_data_vis, aes(x = X, y = Y, label = adm0_a3),
            color = "black", fontface = "bold" , size = 3, check_overlap = FALSE) #label for FRA is dis-orientated





