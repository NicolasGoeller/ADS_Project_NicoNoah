library(tidyverse)
library(stargazer)
library(hrbrthemes) 
library(extrafont)
library(memisc)

#Plots: theme_ipsum() produces warnings, but nothing serious
EVS_final <- read_rds("Data/EVS_final.rds")

ggplot(EVS_final, aes(x = sat))+
  geom_bar()+
     #from next line, publication ready & nice
  labs(x = "Life satisfaction",
       title = "Titel",
       subtitle = "Untertitel",
       caption = "Credits to us") + 
  theme_ipsum(grid = "Y")

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



##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## 2. Building ggmap

#install.packages(c("sf", "rnaturalearth", "rnaturalearthdata", "rgeos"))
install.packages("rworldmap") #useful package to solve the geodata problem
library(sf)         
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(rworldmap)

# World dataset
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# filter for Europe dataset (best strategy still to be found!)
eur <- 
  world %>%
  filter(continent == "Europe") %>%
  filter(admin != "Russia") %>%
  filter(type == "Sovereign country")

# plot Europe 
ggplot(data = eur) +
  geom_sf()


# Basic map with country codes

world_points <- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

ggplot(data = world) +
  geom_sf()+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Europe map", 
          subtitle = "Life satisfaction across countries")+
  coord_sf(xlim = c(-20, 59), ylim = c(35, 71), expand = FALSE)+
  geom_text(data= world_points,aes(x=X, y=Y, label=brk_a3),
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
  geom_text(data= world_points,aes(x=X, y=Y, label=brk_a3),
            color = "black", fontface = "bold" , size = 3, check_overlap = FALSE)

  


