###
# This script provides basic preparation of variables used
# for further analyses. 

#install.packages(c("haven", "plyr", "tidyverse", "magrittr", "psych", "sf", "naturalearth", 
#                   "rnaturalearthdata", "rgeos", "eurostat"))

library(haven)
library(plyr)
library(dplyr)
library(tidyverse)
library(magrittr)
library(psych)
library(sf)         
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(eurostat)


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
###   1. European Values Survey

##    1.1 Read European Values Survey 2008
EVS <- read_rds("Data/EVS.rds")


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
##    1.2 Level-1-variables: Data formatting
EVS %<>% within({ #base variables
  nation <- country #Country
  nation[country %in% c(-5, -4, -3, -2, -1)] <- NA
  nation[country %in% c(197)] <- 196 #Overwrite North Cyprus with Cyprus
  nation[country %in% c(909)] <- 372 #Overwrite Northern Ireland with Great Britain
  nation <- as_factor(nation, ordered = F)
  
  c_code <- c_abrv #Country code abbreviation
  
  reg <- v371b_N1 #Region on federal state level
  reg[v371b_N1 %in% c(-5, -4, -3, -2, -1)] <- NA
  reg[country %in% c(197)]  <- "CY: Northern Cyprus"
  reg[country %in% c(792)] <- "TK: Turkey"
  reg <- as_factor(reg, ordered = F)
  
  eureg <- country
  eureg <- as.numeric(country)
  eureg[country %in% c(752, 578, 208, 372, 246, 352, 428, 233, 440, 
                       909, 826)]  <- 1 #Northern Europe
  eureg[country %in% c(276, 250, 56, 442, 528, 40, 756)]  <- 2 #Western Europe
  eureg[country %in% c(724, 380, 300, 792, 620, 196, 470, 705, 8, 499, 70, 191, 
                      688, 807, 915, 51, 197)]  <- 3 #Southern Europe
  eureg[country %in% c(100, 703, 112, 348, 616, 268, 498, 642, 804, 
                      203)]  <- 4 #Eastern Europe
  eureg <- as.character(eureg)
  eureg <- as_factor(eureg, ordered = F, levels = "values")
  
  sat <- v66 #Life satisfaction (10 point)
  sat[v66 %in% c(-5, -4, -3, -2, -1)] <- NA
  sat <- as.numeric(sat)
  sat_c <- sat - mean(sat, na.rm = T)
  
  happ <- v8 #Feeling of happiness (4 point)
  happ[v8 %in% c(-5, -4, -3, -2, -1)] <- NA
  happ <- as.numeric(happ)
  happ_c <- happ - mean(happ, na.rm = T)
}) #base variables


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##    1.3 Adapting geo-variables on factor level: Renaming and exclusion of unused factor 
          #levels in variables "reg", "nation" and "eureg"
EVS$nation <- mapvalues(EVS$nation, from = c("Great Britain", "Slovak Republic"),
                             to = c("United Kingdom", "Slovakia")) 
EVS$nation <- droplevels(EVS$nation, exclude = c(as.character(c("Northern Cyprus", 
                                  "Russian Federation", "Northern Ireland")))) 
EVS$reg <- mapvalues(EVS$reg, from = c("GB-NIR: Northern Ireland"), 
      to = c("GB-GBN: Northern Ireland"))
EVS$reg <- droplevels(EVS$reg, 
                exclude = c(as.character(c("RU: Central Federal District", 
                      "RU: North West federal district", "RU: South Federal district",
                      "RU: Privolzhsky federal district", "RU: Urals federal district", 
                      "RU: Siberian federal district", "RU: Far East federal district"  )))) 
EVS$eureg <- mapvalues(EVS$eureg, from = c("1", "2", "3", "4"), 
                       to = c("Northern Europe", "Western Europe", 
                              "Southern Europe", "Eastern Europe"))


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##    1.4 Format non-pecuniary factors variables                     
EVS %<>% within({ #non-pecuniary factors
  intp_trust <- v62 #Dummy: Do you think you can trust other people
  intp_trust[v62 %in% c(-5, -4, -3, -2, -1)] <- NA
  intp_trust <- (intp_trust - 2) * -1 #0 = No, 1 = Yes
  intp_trust <- factor(intp_trust, levels = c(0, 1), labels = c("Distrustful", "Trustful"), 
                       ordered = F)
  
  help <- v64 #Most people are helpful or look out for themselves (10 point)
  help[v64 %in% c(-5, -4, -3, -2, -1)] <- NA
  help <- as.numeric(help)
  help_c <- help - mean(help, na.rm = T)
  
  fair <- v63 #Will people be fair or make their advantage with you (10 point)
  fair[v63 %in% c(-5, -4, -3, -2, -1)] <- NA
  fair <- as.numeric(fair)
  fair_c <- fair - mean(fair, na.rm = T)
  
  job_sat <- v90 #Job satisfaction (10 point)
  job_sat[v90 %in% c(-5, -4, -3, -2, -1)] <- NA
  job_sat <- as.numeric(job_sat)
  job_sat_c <- job_sat - mean(job_sat, na.rm = T)
  
  siops <- v339SIOPS #Standard Index of Occupational Prestige Scala (1-100)
  siops <- as.numeric(siops)
  siops_c <- siops - mean(siops, na.rm = T)
  
  less_money <- v203 #Less emphasis on money and material possession (3 point)
  less_money[v203 %in% c(-5, -4, -3, -2, -1)] <- NA 
  less_money <- as.numeric(less_money)
  
}) #non-pecuniary factors


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##    1.5 Format pecuniary factors variables 
EVS %<>% within({ #pecuniary factors 
  nowork <- v89 #Are yourself employed
  nowork[v89 %in% c(-5, -4, -3, -2, -1)] <- NA
  nowork <- nowork - 1 #0 = Yes, 1 = No
  nowork <- factor(nowork, levels = c(0, 1), labels = c("Employed", "Unemployed"), 
                       ordered = F)
  
  inc_an <- v353YR #Annual income in euros
  inc_an[v353YR %in% c(-5, -4, -3, -2, -1)] <- NA
  inc_an <- as.numeric(inc_an)
  inc_an_c <- inc_an - mean(inc_an, na.rm = T)
  
  inc_mon <- v353MM #Monthly income in euros
  inc_mon[v353MM %in% c(-5, -4, -3, -2, -1)] <- NA
  inc_mon <- as.numeric(inc_mon)
  inc_mon_c <- inc_mon - mean(inc_mon, na.rm = T)
  
  incppp_mon <- v353M_ppp #Monthly income after purchasing power parity
  incppp_mon[v353M_ppp %in% c(-5, -4, -3, -2, -1)] <- NA
  incppp_mon <- as.numeric(incppp_mon)
  incppp_mon_c <- incppp_mon - mean(incppp_mon, na.rm = T)
  
  isei <- v339ISEI #International Socio-Economic Index of Occupational Status
  isei <- as.numeric(isei)
  isei_c <- isei - mean(isei, na.rm = T)
  
  inc_eq <- v198 #Income equality from 1 to 10 (1 indicating more equality)
  inc_eq[v198 %in% c(-5, -4, -3, -2, -1)] <- NA 
  inc_eq <- as.numeric(inc_eq)
  inc_eq_c <- inc_eq - mean(inc_eq, na.rm = T)
  
}) #pecuniary factors


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##    1.6 Format work variables 
EVS %<>% within({ #work variables
  work_impo <- v1 #How important is work in your life (4 point)
  work_impo[v1 %in% c(-5, -4, -3, -2, -1)] <- NA
  work_impo <- as.numeric(work_impo)
  work_impo_c <- work_impo - mean(work_impo, na.rm = T)
  
  free_job <- v91 #How free are you to make decisions in your job (10 point)
  free_job[v91 %in% c(-5, -4, -3, -2, -1)] <- NA
  free_job <- as.numeric(free_job)
  free_job_c <- free_job - mean(free_job, na.rm = T)
  
  talent <- v92 #Need job to develop your talents (5 point)
  talent[v92 %in% c(-5, -4, -3, -2, -1)] <- NA
  talent <- as.numeric(talent)
  talent <- (talent - 5) * -1
  talent_c <- talent - mean(talent, na.rm = T)
  
  duty <- v95 #Work is a duty towards society (5 point)
  duty[v95 %in% c(-5, -4, -3, -2, -1)] <- NA
  duty <- as.numeric(duty)
  duty <- (duty - 5) * -1
  duty_c <- duty - mean(duty, na.rm = T)
  
  work_first <- v96 #Work comes always first (5 point)
  work_first[v96 %in% c(-5, -4, -3, -2, -1)] <- NA
  work_first <- as.numeric(work_first)
  work_first <- (work_first - 5) * -1
  work_first_c <- work_first - mean(work_first, na.rm = T)
  
  sup <- v341 #are you supervising someone
  sup[v341 %in% c(-5, -4, -3, -2, -1)] <- NA
  sup <- (sup - 2) * -1 #0 = No, 1 = Yes
  
  entre <- v338 #Are you self employed
  entre[v338 %in% c(-5, -4, -3, -2, -1)] <- NA
  entre <- entre - 1 #0 = No, 1 = Yes
}) #work variables


#--------------------------------------------------------------------------------------------------------------------------
##    1.7 Format institutional trust index
EVS %<>% within({ #index variables institutional trust
  conf_church <- v205 #church
  conf_church[v205 %in% c(-5, -4, -3, -2, -1)] <- NA
  conf_church <- (conf_church-4)*-1
  conf_church <- as.numeric(conf_church)
  
  conf_armed <- v206 #armed forces
  conf_armed[v206 %in% c(-5, -4, -3, -2, -1)] <- NA
  conf_armed <- (conf_armed-4)*-1
  conf_armed <- as.numeric(conf_armed)
  
  conf_educ <- v207 #educational system
  conf_educ[v207 %in% c(-5, -4, -3, -2, -1)] <- NA
  conf_educ <- (conf_educ-4)*-1
  conf_educ <- as.numeric(conf_educ)
  
  conf_press <- v208 #press
  conf_press[v208 %in% c(-5, -4, -3, -2, -1)] <- NA
  conf_press <- (conf_press-4)*-1
  conf_press <- as.numeric(conf_press)
  
  conf_tu <- v209 #trade unions
  conf_tu[v209 %in% c(-5, -4, -3, -2, -1)] <- NA
  conf_tu <- (conf_tu-4)*-1
  conf_tu <- as.numeric(conf_tu)
  
  conf_police <- v210 #police
  conf_police[v210 %in% c(-5, -4, -3, -2, -1)] <- NA
  conf_police <- (conf_police-4)*-1
  conf_police <- as.numeric(conf_police)
  
  conf_parl <- v211 #parliament
  conf_parl[v211 %in% c(-5, -4, -3, -2, -1)] <- NA
  conf_parl <- (conf_parl-4)*-1
  conf_parl <- as.numeric(conf_parl)
  
  conf_cs <- v212 #civil service
  conf_cs[v212 %in% c(-5, -4, -3, -2, -1)] <- NA
  conf_cs <- (conf_cs-4)*-1
  conf_cs <- as.numeric(conf_cs)
  
  conf_socs <- v213 #social security system
  conf_socs[v213 %in% c(-5, -4, -3, -2, -1)] <- NA
  conf_socs <- (conf_socs-4)*-1
  conf_socs <- as.numeric(conf_socs)
  
  conf_eu <- v214 #European Union
  conf_eu[v214 %in% c(-5, -4, -3, -2, -1)] <- NA
  conf_eu <- (conf_eu-4)*-1
  conf_eu <- as.numeric(conf_eu)
  
  conf_nato <- v215 #NATO
  conf_nato[v215 %in% c(-5, -4, -3, -2, -1)] <- NA
  conf_nato <- (conf_nato-4)*-1
  conf_nato <- as.numeric(conf_nato)
  
  conf_un <- v216 #United Nations
  conf_un[v216 %in% c(-5, -4, -3, -2, -1)] <- NA
  conf_un <- (conf_un-4)*-1
  conf_un <- as.numeric(conf_un)
  
  conf_hs <- v217 #health care system
  conf_hs[v217 %in% c(-5, -4, -3, -2, -1)] <- NA
  conf_hs <- (conf_hs-4)*-1
  conf_hs <- as.numeric(conf_hs)
  
  conf_just <- v218 #justice system
  conf_just[v218 %in% c(-5, -4, -3, -2, -1)] <- NA
  conf_just <- (conf_just-4)*-1
  conf_just <- as.numeric(conf_just)
  
  conf_gov <- v222 #government
  conf_gov[v222 %in% c(-5, -4, -3, -2, -1)] <- NA
  conf_gov <- (conf_gov-4)*-1
  conf_gov <- as.numeric(conf_gov)

}) #index variables institutional trust

#Checking for intercorrelatedness
inst_trust <- dplyr::select(EVS,
                     conf_press, conf_tu, conf_police,
                     conf_parl, conf_cs, conf_eu, 
                     conf_socs, conf_nato, conf_un, 
                     conf_hs, conf_just, conf_gov)
inst_trust %<>% na.omit()
cor_inst_trust <- cor(inst_trust) %>% round(2) #get correlation matrix
inst_trust %>% as.matrix() %>% alpha(check.keys = T) #compute Cronbach's alpha 

# Constructing institutional trust index and trimming scale
EVS %<>% within({ #index institutional trust
  inst_trust <- (conf_press + conf_tu + conf_police +
                 conf_parl + conf_cs + conf_eu + 
                 conf_socs + conf_nato + conf_un + 
                 conf_hs + conf_just + conf_gov)
  
  inst_trust <- inst_trust/4  
}) #index institutional trust: scale from 0 to 9 i.e., from low trust in inst. to high trust in inst. 


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##    1.8 Format trustworthiness index 
EVS %<>% within({ #index variables trustworthiness
  cl_sb <- v233 #state benefits
  cl_sb[v233 %in% c(-5, -4, -3, -2, -1)] <- NA
  cl_sb <- (cl_sb-10)*-1
  cl_sb <- as.numeric(cl_sb)
  
  ch_tax <- v234 #cheating on tax
  ch_tax[v234 %in% c(-5, -4, -3, -2, -1)] <- NA
  ch_tax <- (ch_tax-10)*-1
  ch_tax <- as.numeric(ch_tax) 
  
  joy <- v235 #joyriding
  joy[v235 %in% c(-5, -4, -3, -2, -1)] <- NA
  joy <- (joy-10)*-1
  joy <- as.numeric(joy) 
  
  lying <- v237 #lying in own interest
  lying[v237 %in% c(-5, -4, -3, -2, -1)] <- NA
  lying <- (lying-10)*-1
  lying <- as.numeric(lying) 
  
  bribe <- v239 #bribe
  bribe[v239 %in% c(-5, -4, -3, -2, -1)] <- NA
  bribe <- (bribe-10)*-1
  bribe <- as.numeric(bribe) 
  
  av_tax <- v245 #paying cash to avoid taxes
  av_tax[v245 %in% c(-5, -4, -3, -2, -1)] <- NA
  av_tax <- (av_tax-10)*-1
  av_tax <- as.numeric(av_tax) 
  
  av_pub_f <- v234 #avoid public transfer fair
  av_pub_f[v234 %in% c(-5, -4, -3, -2, -1)] <- NA
  av_pub_f <- (av_pub_f-10)*-1
  av_pub_f <- as.numeric(av_pub_f) 
}) #index variables trustworthiness

#Checking for intercorrelatedness
trust_wrth <- dplyr::select(EVS, av_pub_f, av_tax, bribe,
                    lying, joy, ch_tax, cl_sb)
trust_wrth %<>% na.omit()
trust_wrth_cor <- cor(trust_wrth) %>% round(2) #get correlation matrix
trust_wrth_cor %>% as.matrix() %>% alpha(check.keys = T) #compute Cronbach's alpha 

# Constructing trustworthiness index and trimming scale
EVS %<>% within({ #index trustworthiness
  trust_wrth <- (av_pub_f + av_tax + bribe +
                lying + joy + ch_tax + cl_sb)
  
  trust_wrth <- trust_wrth/7
}) #index trustworthiness: scale from 0 to 9 i.e., from low trust in inst. to high trust in inst. 


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##    1.9 Format demograhics variables
EVS %<>% within({ #demographics 
  sex <- v302 #Sex
  sex[v302 %in% c(-5, -4, -3, -2, -1)] <- NA
  sex <- factor(sex, levels = c(1, 2), labels = c("Male", "Female"), ordered = F)
  
  age <- v303 #Age recoded from birth year
  age[v303 %in% c(-5, -4, -3, -2, -1)] <- NA
  age <- as.numeric(age)
  age <- 2008 - age
  age_c <- age - mean(age, na.rm = T)
  
  mar_stat <- v313 #Current marital status
  mar_stat[v313 %in% c(-5, -4, -3, -2, -1)] <- NA
  mar_stat <- as_factor(mar_stat, ordered = F)
  
  edu_cat <- v336 #Education after ISCED code (6 point)
  edu_cat[v336 %in% c(-5, -4, -3, -2, -1)] <- NA
  edu_cat <- as_factor(edu_cat, ordered = T)
  
  edu <- v336_2 #Education after ISCED 2-digit
  edu[v336_2 %in% c(-5, -4, -3, -2, -1)] <- NA
  edu <- as.numeric(edu)
  edu_c <- edu - mean(edu, na.rm = T)
  
  town <- v370a #size of town
  town[v370a %in% c(-5, -4, -3, -2, -1)] <- NA
  town <- as.numeric(town)
  town_c <- town - mean(town, na.rm = T)
  
  employ <- v337 #Employment status
  employ[v337 %in% c(-5, -4, -3, -2, -1)] <- NA
  employ <- as_factor(employ, ordered = F)
}) #demographics


#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

###   2. Level-2-variables 

##    2.1 Countries used in analysis
nat <- c("Albania", "Austria", "Armenia", "Belgium", "Bosnia Herzegovina", 
         "Bulgaria", "Belarus", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
         "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", 
         "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
         "Moldova", "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", 
         "Romania", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", 
         "Switzerland", "Turkey", "Ukraine", "Macedonia", "United Kingdom", "Kosovo")

##    2.2 Read country level data from Pippa Norris
PN_select <- read_rds("Data/PN_select.rds")

PN_select %<>% within({ #Variable treatment
  nation <- Nation #Country variable
  nation <- as_factor(nation)
  
  fhrate <- fhrate08 #Freedom House: Political Rights and Civil Liberties (1=highest, 7=lowest)
  fhrate <- as.numeric(fhrate)
  
  voice_acc <- WGI_voice2008 #Voice and Accountability (WGI 2014)
  voice_acc <- as.numeric(voice_acc)
  
  press_free <- FreePress2008 #Freedom House: Freedom of Press (low=free) (6 point)
  press_free <- as.numeric(press_free)
  
  hdi <- UNDP_HDI2008 #Human Development Index
  hdi <- as.numeric(hdi)
})


#--------------------------------------------------------------------------------------------

##    2.3 Read World Bank data with GDP per capita and Gini-coefficent

wb_data <- read_rds("Data/WB_Data.rds")

#Renaming of country variable
wb_data$nation <- wb_data$country


#----------------------------------------------------------------------------------------

##    2.4 European Value Survey 2008: Level-2-variables

# Creating a subsetted dataset for variable to be aggregated
EVS_nat <- dplyr::select(EVS, nation, nowork, sat) # backup: intp_trust
EVS_reg <- dplyr::select(EVS, nation, c_code, reg, intp_trust, inst_trust, trust_wrth)

#Creating 
EVS_nat <- EVS_nat %>% 
  dplyr::group_by(nation) %>% 
  dplyr::summarise(unemployment = mean(as.numeric(nowork), na.rm = T),
            life_sat = mean(sat, na.rm = T))

EVS_reg <- EVS_reg %>% 
  dplyr::group_by(reg, c_code) %>% 
  dplyr::summarise(trust_wrth_reg = mean(trust_wrth, na.rm = T), 
            intp_trust_reg = mean(as.numeric(intp_trust), na.rm = T),
            inst_trust_reg = mean(inst_trust, na.rm = T))


#----------------------------------------------------------------------------------------

##    2.5 Joining the complete Level-2 and Level-3 data files

#National Data File (Level-3)
nat_data <- left_join(wb_data, PN_select, by = "nation")
nat_data <- left_join(nat_data, EVS_nat, by = "nation")
nat_data$nation <- as_factor(nat_data$nation)

nat_data <- dplyr::select(nat_data, nation, unemployment, GDPpc, gini, hdi, press_free, 
                   voice_acc, fhrate, life_sat)

#Applying Grand Mean Centering to Macro-data
nat_data %<>% within({ #Creating centered macro variables
  gini_c <- gini - mean(gini, na.rm = T)
  
  gdppc_c <- GDPpc - mean(GDPpc, na.rm =T)
  
  hdi_c <- hdi - mean(hdi, na.rm = T)
  
  press_free_c <- press_free - mean(press_free, na.rm = T)
  
  voice_acc_c <- voice_acc - mean(voice_acc, na.rm = T)
  
  fhrate_c <- fhrate - mean(fhrate, na.rm = T)
  
  unemployment_c <- unemployment - mean(unemployment, na.rm = T)
})  


#Regional Data File (Level-2)

EVS_reg %<>% within({ #Grand Mean Centering
  intp_trust_reg_c <- intp_trust_reg - mean(intp_trust_reg, na.rm = T)
  
  trust_wrth_reg_c <- trust_wrth_reg - mean(trust_wrth_reg, na.rm = T)
  
  inst_trust_reg_c <- inst_trust_reg - mean(inst_trust_reg, na.rm = T)
})  


#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------

###   3. Combining variables of all 3 Levels
EVS_final <- full_join(EVS, nat_data, by = "nation")
EVS_final <- full_join(EVS_final, EVS_reg, by = "reg")

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------

###   4. Integrating Geodata into the national data set

##    4.1 Create subset of only European geo_data

nat_geo <- c("Albania", "Austria", "Armenia", "Belgium", "Bosnia and Herzegovina", 
             "Bulgaria", "Belarus", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
             "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", 
             "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
             "Moldova", "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", 
             "Romania", "Republic of Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", 
             "Switzerland", "Turkey", "Ukraine", "Macedonia", "United Kingdom", "Kosovo")

eur <- ne_countries(country = nat_geo, scale = "medium", returnclass = "sf")

# rename countries (1) Bosnia and Herzegovina, (2) Serbia 
eur$sovereignt <- plyr::mapvalues(eur$sovereignt, from = c("Bosnia and Herzegovina", "Republic of Serbia"), 
                            to = c("Bosnia Herzegovina", "Serbia"))

# add centroids with coordinates X and Y
eur <- cbind(eur, st_coordinates(st_centroid(eur$geometry)))

# select necessary variables
eur_s <- dplyr::select(eur, sovereignt, geometry, adm0_a3, X, Y)

# Adding nation variable
eur_s$nation <- eur_s$sovereignt

##      4.2 Combine European geo_data with nat_data

# Joining nat_data and European geo_data to obtain new nat_geodata
nat_geodata <- left_join(nat_data, eur_s, by = "nation")

#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

###     5. Integrating regional data 
          #We need EVS_reg to refer to regional EVS data that we grouped above

##      5.1 Get NUTS data for 2006 from eurostat and edit EVS_reg

# Obtain NUTS 1 geodata 
nuts_data <- get_eurostat_geospatial(output_class = "sf", resolution = "60", nuts_level = "1", year = "2006") #NUTS 2006
nuts_data2 <- get_eurostat_geospatial(output_class = "sf", resolution = "60", nuts_level = "1") #no year

# Plot NUTS 1 regions and zoom in appropiately 
ggplot(data = nuts_data) + 
  geom_sf()+
  coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)

# Select necessary variables in nuts_data 
nuts_data <- dplyr::select(nuts_data, CNTR_CODE, NUTS_NAME, geometry)

# reorder EVS_reg data
EVS_reg <- EVS_reg[c(2,1,3,4,5)]

##      5.2 Exclude countries/regions in both datasets 


nuts_data <- nuts_data[-3, ] # excludeAland because empty in nuts_data

nuts_data <- nuts_data %>%
  filter(CNTR_CODE != "TR") %>% # exclude turkey (not asked in EVS)
  filter(NUTS_NAME != "ÅLAND") %>% # exclude ÅLAND because not in EVS
  filter(NUTS_NAME != "Região Autónoma da Madeira") %>% # exclude Madeira because not in EVS
  filter(NUTS_NAME != "Região Autónoma dos Açores") %>% # exclude Acores because not in EVS
  filter(NUTS_NAME != "Départements d'Outre-Mer") %>% # exclude oversea departments because not in EVS
  filter(CNTR_CODE != "LI") %>% # exclude Liechtenstein because not in EVS
  filter(CNTR_CODE != "CY") %>% # exclude Cybrus to avoid issues
  filter(CNTR_CODE != "BG") %>% #exclude Bulgaria because unknown regions in nuts_data
  filter(CNTR_CODE != "GR") %>% #exclude Greece because unknown regions in nuts_data
  filter(CNTR_CODE != "ME") %>% #exclude ME because unknown regions in nuts_data
  filter(CNTR_CODE != "MK") #exclude MK because unknown regions in nuts_data

EVS_reg <- EVS_reg[-116, ] # exclude empty row for Swedish region in EVS
  
EVS_reg <- EVS_reg %>%
  filter(c_code != "TR") %>% # exclude turkey (not asked in EVS)
  filter(c_code != "AL") %>% # exclude Albania because not in Eurostat
  filter(c_code != "AM") %>% # exclude Armenia because not in Eurostat
  filter(c_code != "UA") %>% # exclude Ukraine because not in Eurostat
  filter(c_code != "RS") %>% # exclude Serbia because not in Eurostat
  filter(c_code != "RS-KM") %>% # exclude Kosovo because not in Eurostat
  filter(c_code != "GE") %>% # exclude Georgia because not in Eurostat
  filter(c_code != "CY") %>% # exclude Cybrus to avoid issues
  filter(c_code != "CY-TCC") %>% # exclude North Cyprus because not in Eurostat
  filter(c_code != "BY") %>% # exclude Belarus to avoid issues
  filter(c_code != "BA") %>%# exclude Bosnia H. to avoid issues
  filter(c_code != "MD") %>% # exclude Moldova because not in nuts_data
  filter(c_code != "BG") %>% #exclude Bulgaria because unknown regions in nuts_data
  filter(c_code != "GR") %>% #exclude Greece because unknown regions in nuts_data
  filter(c_code != "ME") %>% #exclude ME because unknown regions in nuts_data
  filter(c_code != "MK") #exclude MK because unknown regions in nuts_data

##      5.3 Rename regions in EVS_reg  

# Rename UK observations in EVS_reg c_code variable
EVS_reg$c_code <- plyr::mapvalues(EVS_reg$c_code, from = c("GB-GBN", "GB-NIR"), 
                                  to = c("UK", "UK"))

#Österreich
EVS_reg$reg <- plyr::mapvalues(EVS_reg$reg, from = c("AT: Ostösterreich", 
                                                     "AT: Südösterreich",
                                                     "AT: Westösterreich"), 
                               to = c("Ostösterreich",
                                      "Südösterreich",
                                      "Westösterreich"))

#Belgique
EVS_reg$reg <- plyr::mapvalues(EVS_reg$reg, from = c("BE: Région de Bruxelles-Capitale/Brussels Hoofdstedelijk Gewest", 
                                                     "BE: Vlaams gewest",
                                                     "BE: Région Wallonne"), 
                               to = c("Région de Bruxelles-Capitale / Brussels Hoofdstedelijk Gewest",
                                      "Vlaams Gewest",
                                      "Région Wallonne"))

#Deutschland
EVS_reg$reg <- plyr::mapvalues(EVS_reg$reg, from = c("DE: Baden-Württemberg", 
                                                     "DE: Bayern",
                                                     "DE: Berlin",
                                                     "DE: Brandenburg",
                                                     "DE: Bremen",
                                                     "DE: Hamburg",
                                                     "DE: Hessen",
                                                     "DE: Mecklenburg-Vorpommern",
                                                     "DE: Niedersachsen",
                                                     "DE: Nordrhein-Westfalen",
                                                     "DE: Rheinland-Pfalz",
                                                     "DE: Saarland",
                                                     "DE: Sachsen",
                                                     "DE: Sachsen-Anhalt",
                                                     "DE: Schleswig-Holstein",
                                                     "DE: Thüringen"), 
                               to = c("Baden-Württemberg",
                                      "Bayern",
                                      "Berlin",
                                      "Brandenburg",
                                      "Bremen",
                                      "Hamburg",
                                      "Hessen",
                                      "Mecklenburg-Vorpommern",
                                      "Niedersachsen",
                                      "Nordrhein-Westfalen",
                                      "Rheinland-Pfalz",
                                      "Saarland",
                                      "Sachsen",
                                      "Sachsen-Anhalt",
                                      "Schleswig-Holstein",
                                      "Thüringen"))

#Espana 
EVS_reg$reg <- plyr::mapvalues(EVS_reg$reg, from = c("ES: Noroeste", 
                                                     "ES: Noreste",
                                                     "ES: Comunidad de Madrid",
                                                     "ES: Centro",
                                                     "ES: Este",
                                                     "ES: Sur",
                                                     "ES: Canarias"), 
                               to = c("Noroeste",
                                      "Noreste",
                                      "Comunidad de Madrid",
                                      "Centro (E)",
                                      "Este",
                                      "Sur",
                                      "Canarias"))

#France
EVS_reg$reg <- plyr::mapvalues(EVS_reg$reg, from = c("FR: Île de France", 
                                                     "FR: Bassin Parisien",
                                                     "FR: Nord-Pas-de-Calais",
                                                     "FR: Est",
                                                     "FR: Ouest",
                                                     "FR: Sud-Ouest",
                                                     "FR: Centre-Est",
                                                     "FR: Méditerranée"), 
                               to = c("Île de France",
                                      "Bassin Parisien",
                                      "Nord - Pas-de-Calais",
                                      "Est",
                                      "Ouest",
                                      "Sud-Ouest",
                                      "Centre-Est",
                                      "Méditerranée"))

#Magyr
EVS_reg$reg <- plyr::mapvalues(EVS_reg$reg, from = c("HU: Közép-Magyarország", 
                                                     "HU: Dunántúl",
                                                     "HU: Alföld és Észak"), 
                               to = c("Közép-Magyarország",
                                      "Dunántúl",
                                      "Alföld És Észak"))

#Italia
EVS_reg$reg <- plyr::mapvalues(EVS_reg$reg, from = c("IT: Nord-Ovest", 
                                                     "IT: Nord-Est",
                                                     "IT: Centro",
                                                     "IT: Sud",
                                                     "IT: Isole"), 
                               to = c("Nord-Ovest",
                                      "Nord-Est",
                                      "Centro (I)",
                                      "Sud",
                                      "Isole"))

#Nederlande 
EVS_reg$reg <- plyr::mapvalues(EVS_reg$reg, from = c("NL: Noord-Nederland", 
                                                     "NL: Oost-Nederland",
                                                     "NL: West-Nederland",
                                                     "NL: Zuid-Nederland"), 
                               to = c("Noord-Nederland",
                                      "Oost-Nederland",
                                      "West-Nederland",
                                      "Zuid-Nederland"))

#Polska
EVS_reg$reg <- plyr::mapvalues(EVS_reg$reg, from = c("PL: Region centralny", 
                                                     "PL: Region Poludniowy",
                                                     "PL: Region Wschodni",
                                                     "PL: Region Pólnocno-Zachodni",
                                                     "PL: Region Póludniowo-Zachodni",
                                                     "PL: Region Pólnocny"), 
                               to = c("Region Centralny",
                                      "Region Poludniowy",
                                      "Region Wschodni",
                                      "Region Pólnocno-Zachodni",
                                      "Region Poludniowo-Zachodni",
                                      "Region Pólnocny"))

#Romania 
EVS_reg$reg <- plyr::mapvalues(EVS_reg$reg, from = c("RO: Macroregiunea unu", 
                                                     "RO: Macroregiunea doi",
                                                     "RO: Macroregiunea trei",
                                                     "RO: Macroregiunea patru"), 
                               to = c("Macroregiunea unu",
                                      "Macroregiunea doi",
                                      "Macroregiunea trei",
                                      "Macroregiunea patru"))

#Sverige
EVS_reg$reg <- plyr::mapvalues(EVS_reg$reg, from = c("SE: Östra Sverige", 
                                                     "SE: Södra Sverige",
                                                     "SE: Norra Sverige"), 
                               to = c("Östra Sverige",
                                      "Södra Sverige",
                                      "Norra Sverige"))

#United Kingdom
EVS_reg$reg <- plyr::mapvalues(EVS_reg$reg, from = c("GB-GBN: North East (England)", 
                                                     "GB-GBN: North West (England)",
                                                     "GB-GBN: Yorkshire and the Humber",
                                                     "GB-GBN: East Midlands (England)",
                                                     "GB-GBN: West Midlands (England)",
                                                     "GB-GBN: East of England",
                                                     "GB-GBN: London",
                                                     "GB-GBN: South East (England)",
                                                     "GB-GBN: South West (England)",
                                                     "GB-GBN: Wales",
                                                     "GB-GBN: Scotland",
                                                     "GB-GBN: Northern Ireland"), 
                               to = c("North East (England)",
                                      "North West (England)",
                                      "Yorkshire and the Humber",
                                      "East Midlands (England)",
                                      "West Midlands (England)",
                                      "East of England",
                                      "London",
                                      "South East (England)",
                                      "South West (England)",
                                      "Wales",
                                      "Scotland",
                                      "Northern Ireland"))

#Various
EVS_reg$reg <- plyr::mapvalues(EVS_reg$reg, from = c("CH: Schweiz/Suisse/Svizzera", 
                                                     "CZ: Ceska Republika",
                                                     "DK: Danmark",
                                                     "EE: Eesti",
                                                     "FI: Manner-Suomi",
                                                     "HR: Hrvatska",
                                                     "IE: Ireland",
                                                     "IS: Ísland",
                                                     "LT: Lietuva",
                                                     "LU: Luxembourg (Grand-Duché)",
                                                     "LV: Latvija",
                                                     "MT: Malta",
                                                     "NO: Norge",
                                                     "PT: Continente",
                                                     "SI: Slovenija",
                                                     "SK: Slovenská Republika"), 
                               to = c("Schweiz/Suisse/Svizzera",
                                      "Ceská Republika",
                                      "Danmark",
                                      "Eesti",
                                      "Manner-Suomi",
                                      "Hrvatska",
                                      "Ireland",
                                      "Ísland",
                                      "Lietuva",
                                      "Luxembourg (Grand-Duché)",
                                      "Latvija",
                                      "Malta",
                                      "Norge",
                                      "Continente",
                                      "Slovenija",
                                      "Slovenská Republika"))

##      5.4 Merge EVS_reg and nuts_data

# Rename NUTS_NAME variable in nuts_data
colnames(nuts_data)[colnames(nuts_data)=="NUTS_NAME"] <- "reg"

# Merge datasets
reg_geodata <- left_join(EVS_reg, nuts_data, by = "reg") #problems with CZ and some Polish regions

##      5.5 Get nation data for regions that are not listed

# Get nation data for relevant variables
EVS_reg2 <- dplyr::select(EVS, nation, c_code, intp_trust, inst_trust, trust_wrth)

# Group nation data 
EVS_reg2 <- EVS_reg2 %>%
  dplyr::group_by(nation, c_code) %>%
  dplyr::summarise(trust_wrth_reg = mean(trust_wrth, na.rm = T), 
                   intp_trust_reg = mean(intp_trust, na.rm = T),
                   inst_trust_reg = mean(inst_trust, na.rm = T))

# Select necessary countries (i.e., those that have no regions in reg_geodata)
selec_nat <- c("TR", "AL", "AM", "UA", "RS", "RS-KM",
               "GE", "CY", "BY", "BA", "MD", "BG", 
               "GR", "ME", "MK")

EVS_reg2 <- EVS_reg2 %>%
  filter(c_code %in% selec_nat)

# Get geodata for necessary countries
eur_s2 <- dplyr::select(eur_s, geometry, nation)

selec_nat2 <- c("Turkey", "Albania", "Armenia", 
                "Ukraine", "Serbia", "Kosovo",
               "Georgia", "Cyprus", "Belarus", 
               "Bosnia Herzegovina", "Moldova", "Bulgaria", 
               "Greece", "Montenegro", "Macedonia")


eur_s2 <- eur_s2 %>%
  filter(nation %in% selec_nat2)

# Rename nation variable in both datasets
colnames(EVS_reg2)[colnames(EVS_reg2)=="nation"] <- "reg"
colnames(eur_s2)[colnames(eur_s2)=="nation"] <- "reg"

# Merge geodata and country data for regional variables
reg2_geodata <- left_join(EVS_reg2, eur_s2, by = "reg") #problems with CZ and some Polish regions

# Merge reg_geodata and reg2_geodata
reg_final_geo <- full_join(reg_geodata, reg2_geodata) #problems with CZ and some Polish regions


#---------------------------------------------------------------------------------------------

# FINAL DATASETS
#--------------------------------------------------------------
#------------------------------------------------------------##
#Export final Data File                                      ##
write_rds(EVS_final, path = "Data/EVS_final.rds")            ##
#------------------------------------------------------------##
#--------------------------------------------------------------
#Export nation Data File                                     ##
write_rds(nat_geodata, path = "Data/Nation_geoData.rds")           ##
#------------------------------------------------------------##
#--------------------------------------------------------------
#Export regions Data File                                     ##
write_rds(reg_geodata, path = "Data/Region_geoData.rds")           ##
#------------------------------------------------------------##
#--------------------------------------------------------------