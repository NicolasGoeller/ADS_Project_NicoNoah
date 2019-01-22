###
#This script is used to make necessary preparation for treatment of variables in the 
#ongoing project

library(haven)
library(plyr)
library(tidyverse)
library(dplyr)
library(magrittr)

#-----------------------------------------------------------------------------------------

###  1. European Value Survey 2008
EVS_2008 <- read_spss("Data/ZA4800_v4-0-0.sav") # Seems to have the missing countries

#Preliminary exclusions and selection
EVS <- EVS_2008 %>% 
  filter(country != 643) %>% #Exclude Russian Federation
  filter(country != 31) %>%  #Exclude Azerbaijan
  select( #Pre-selection of variables
    country, c_abrv, v371b_N1, v66, v8, #base
    v62, v63, v64, v90, v339SIOPS, v203, #non-pecuniary
    v89, v353YR, v353MM, v353M_ppp, v339ISEI, v198, #pecuniary
    v1, v91, v92, v95, v96, v341, v338, #work variables
    v205:v218, v222, #index institutional trust
    v233, v234, v235, v237, v239, v245, v247, #index justification
    v302, v303, v313, v336, v336_2, v370a, v337) #demographics

#---------------------------------------#
#Export new dataset EVS                 #
write_rds(EVS, path = "Data/EVS.rds")   #
#---------------------------------------#

#----------------------------------------------------------------------------------------------

### 2. Countries used in analysis
nat <- c("Albania", "Austria", "Armenia", "Belgium", "Bosnia Herzegovina", 
         "Bulgaria", "Belarus", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
         "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", 
         "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
         "Moldova", "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", 
         "Romania", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", 
         "Switzerland", "Turkey", "Ukraine", "Macedonia", "United Kingdom", "Kosovo")

#---------------------------------------------------------------------------------------------------------------------------------------------------------------

###  3. Level-2-variables: Democracy Cross-National Data from Pippa Norris

PN <- read_spss("Data/Democracy Cross-National Data V4.1 09092015.sav")

#Selection of relevant variables
PN_select <- dplyr::select(PN, Nation, fhrate08, WGI_voice2008, FreePress2008, UNDP_HDI2008)

#Filling up missing values
PN_select[91,2] <- 5.5 #Freedom House rating 2008 from Freedom House
PN_select[91,5] <- 0.743 #HDI value 2008 from Serbia
PN_select[118,2] <- 3.0 #Freedom House rating 2008 from Freedom House

#Adapting country names
PN_select$Nation <- mapvalues(PN_select$Nation, from = c("Bosnia & Herzegovina","Moldova, Republic of"), 
                               to = c("Bosnia Herzegovina", "Moldova"))

#----------------------------------------------------#
#Export new dataset PN_select                        #
write_rds(PN_select, path = "Data/PN_select.rds")    #
#----------------------------------------------------#

#---------------------------------------------------------------------------------------

### 4. Level-2-variables: World Bank Data for GDP per capita and Gini-coefficient

##  4.1 GDP per capita in 2018 USD

gdp_per_cap <- read.csv("Data/GDP_per_capita_current_USD_data.csv", header = F)

#omit first two rows and last column (dataset description)
gdp_per_cap <- gdp_per_cap[3:267,1:62]

#Renaming header
colnames(gdp_per_cap) <- as.character(unlist(gdp_per_cap[1,]))
gdp_per_cap = gdp_per_cap[-1, ]
colnames(gdp_per_cap)[colnames(gdp_per_cap)=="52"] <- "country"
colnames(gdp_per_cap)[colnames(gdp_per_cap)=="47"] <- "Country_Code"

#Adapting country names
gdp_per_cap$country <- mapvalues(gdp_per_cap$country, from = c("Bosnia and Herzegovina", "Macedonia, FYR", "Slovak Republic"), 
                                 to = c("Bosnia Herzegovina", "Macedonia", "Slovakia"))

#New dataset
gdp_per_cap_new <- dplyr::select(gdp_per_cap, country, Country_Code, "2008")


##  4.2 Gini coefficient for 2008

Gini <- read.csv("Data/Gini_WB.csv", header = F)

#omit first two rows and last column (dataset description)
Gini <- Gini[3:267,1:62]

#Renaming header
colnames(Gini) <- as.character(unlist(Gini[1,]))
Gini = Gini[-1, ]
colnames(Gini)[colnames(Gini)=="52"] <- "country"
colnames(Gini)[colnames(Gini)=="47"] <- "Country_Code"

#Adapting country names
Gini$country <- mapvalues(Gini$country, from = c("Bosnia and Herzegovina", "Macedonia, FYR", "Slovak Republic"), 
                          to = c("Bosnia Herzegovina", "Macedonia", "Slovakia"))

#New dataset
Gini_new <- dplyr::select(Gini, country, "2008")


## 4.3 Create a combined dataset for World Bank data

wb_mat <- as.matrix(nat, ncol = 1) #Create from vector of country names 
colnames(wb_mat) <- c("country") #Name column "nation"
wb_nat <- as_tibble(wb_mat) #Recreate it as a tibble (data frame)
wb_data <- left_join(wb_nat, gdp_per_cap_new, by = "country") 
wb_data <- left_join(wb_data, Gini_new, by = "country")
colnames(wb_data) <- c("country", "Country_Code", "GDPpc", 
                       "gini")
#Adding missing data
wb_data[8,4] <- 32.6 #Gini Croatia approximated from WB Gini 2009
wb_data[16,4] <- 30.2 #Gini Germany from Eurostat
wb_data[30,4] <- 32.0 #Gini Poland from Eurostat 
wb_data[33,4] <- 28.2 #Gini Serbia 2008 from CIA Factbook
wb_data[43,4] <- 31.8 #Gini Kosovo from WB Gini 2009
wb_data[5,4] <- 33.1 #Gini Bosnia Herzogewina from WB Gini 2007
wb_data[41,4] <- 42.8 #Gini Macedonia from WB Gini 2009

#-------------------------------------------------#
#Export new World Bank dataset                    #
write_rds(wb_data, path = "Data/WB_Data.rds")     #
#-------------------------------------------------#

