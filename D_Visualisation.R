## This script puts everything nicely together by constructing plots
#install.packages(c("tidyverse", "stargazer", "hrbrthemes", "extrafont", "memisc", "sf", 
#                   "rnaturalearth", "rnaturalearthdata", "rgeos"))

library(tidyverse)
library(stargazer)
library(hrbrthemes) 
library(extrafont)
library(memisc)
library(sf)         
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)


#Read in data sets
EVS_final <- read_rds("Data/EVS_final.rds")
nat_geodata <- read_rds("Data/Nation_geoData.rds")

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

## 2.1 Plot Europe

# basic plot of Europe  
ggplot(data = eur) +
  geom_sf() # need to zoom in 

# second plot of Europe (zoomed in properly)
ggplot(data = eur) +
  geom_sf() +
  ggtitle("Europe map")+
  coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##    2.2 Create some plots

#   Countries and GDP per capita 
ggplot(data = nat_data_vis)+
  geom_sf(aes(fill = GDPpc))+
  labs(fill = "GDP per capita 2008")+
  ggtitle("Europe Map", 
          subtitle = "Countries and GDP per capita")+
  coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)
  

#   Countries and Freedom House rating 2008 
#Issue titles replace the rendering of the plot
ggplot(data = nat_data_vis)+
  geom_sf(aes(fill = fhrate))+
  labs(fill = "Freedom House Democracy Rating 2008")+
  ggtitle("Europe Map", 
          subtitle = "Countries and Freedom House Rating")+
  coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)


#   Countries and UNHDP HDI rating 2008 
ggplot(data = nat_data_vis)+
  geom_sf(aes(fill = hdi))+
  labs(fill = "HDI Index 2008")+
  ggtitle("Europe Map", 
          subtitle = "Countries and HDI Index")+
  coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)

#     2.2.4 Countries and life_sat
ggplot(data = nat_data_vis)+
  geom_sf(aes(fill = life_sat))+
  labs(fill = "Life satisfaction (from low to high) 2008")+
  ggtitle("Europe Map", 
          subtitle = "Countries and Life Satisfaction")+
  coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)


#---------------------------------------------------------------------------------------------------------------------------------

##    2.3 Create plots with country indication 

# Countries and GDP per capita 
ggplot(data = nat_data_vis)+
  geom_sf(aes(fill = GDPpc))+
  labs(fill = "GDP per capita")+
  ggtitle("Europe Map", 
          subtitle = "Countries and GDP per capita")+
  coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE) +
  geom_text(data= nat_data_vis, aes(x = X, y = Y, label = adm0_a3),
            color = "black", fontface = "bold" , size = 3, check_overlap = FALSE) #label for FRA is dis-orientated

#----------------------------------------------------------------------------------------------------------

##  4. Codebook

?data.set
?codebook

EVS_book <- dplyr::select(EVS_final,
                   eureg, siops)
EVS_book <- as.data.set(EVS_book)

EVS_book <- within(EVS_book,{
  
  description(eureg) <- "Geopgraphical region in Europe"
  measurement(eureg) <- "nominal"
  missing.values(eureg) <- c(NA)
  annotation(eureg)["Remark"] <- "Item was recoded after the UN classification from the variable 'nation'"
})

EVS_book <- within(EVS_book,{
  
  description(siops) <- "SIOPS-Index"
  wording(siops) <- "Standard Index of Occupational Prestige Scala"
  measurement(siops) <- "ratio"
  missing.values(siops) <- c(NA)
})

codebook(EVS_book)
description(EVS_book)
Write(codebook(EVS_book), file = "EVS_final_cdbk.")


