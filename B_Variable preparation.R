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
  
  happ <- v8 #Feeling of happiness (4 point)
  happ[v8 %in% c(-5, -4, -3, -2, -1)] <- NA
  happ <- as.numeric(happ)
}) #base variables


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##    1.3 Adapting geo-variables on factor level: Renaming and exclusion of unused factor 
          #levels in variables "reg", "nation" and "eureg"
EVS$nation <- mapvalues(EVS$nation, from = c("Great Britain", "Slovak Republic"),
                             to = c("United Kingdom", "Slovakia")) 
EVS$nation <- droplevels(EVS$nation, exclude = c(as.character(c("Northern Cyprus", 
                                  "Russian Federation", "Northern Ireland")))) 
EVS$c_code <- plyr::mapvalues(EVS$c_code, from = c("GB-GBN", "GB-NIR", "CY-TCC"), 
                                  to = c("UK", "UK", "CY"))

EVS$reg <- droplevels(EVS$reg, 
                exclude = c(as.character(c("RU: Central Federal District", 
                      "RU: North West federal district", "RU: South Federal district",
                      "RU: Privolzhsky federal district", "RU: Urals federal district", 
                      "RU: Siberian federal district", "RU: Far East federal district"  )))) 

reg_old <- c("AT: Ostösterreich", "AT: Südösterreich", "AT: Westösterreich",
             "BE: Région de Bruxelles-Capitale/Brussels Hoofdstedelijk Gewest", 
             "BE: Vlaams gewest", "BE: Région Wallonne",
             "DE: Baden-Württemberg", "DE: Bayern", "DE: Berlin", "DE: Brandenburg",
             "DE: Bremen", "DE: Hamburg", "DE: Hessen", "DE: Mecklenburg-Vorpommern",
             "DE: Niedersachsen", "DE: Nordrhein-Westfalen", "DE: Rheinland-Pfalz",
             "DE: Saarland", "DE: Sachsen", "DE: Sachsen-Anhalt", "DE: Schleswig-Holstein",
             "DE: Thüringen",
             "ES: Noroeste", "ES: Noreste", "ES: Comunidad de Madrid", "ES: Centro",
             "ES: Este", "ES: Sur", "ES: Canarias",
             "FR: Île de France", "FR: Bassin Parisien", "FR: Nord-Pas-de-Calais",
             "FR: Est", "FR: Ouest", "FR: Sud-Ouest", "FR: Centre-Est", "FR: Méditerranée",
             "HU: Közép-Magyarország", "HU: Dunántúl", "HU: Alföld és Észak",
             "IT: Nord-Ovest", "IT: Nord-Est", "IT: Centro", "IT: Sud", "IT: Isole",
             "NL: Noord-Nederland", "NL: Oost-Nederland", "NL: West-Nederland",
             "NL: Zuid-Nederland",
             "PL: Region centralny", "PL: Region Poludniowy", "PL: Region Wschodni",
             "PL: Region Pólnocno-Zachodni", "PL: Region Póludniowo-Zachodni",
             "PL: Region Pólnocny",
             "RO: Macroregiunea unu","RO: Macroregiunea doi", "RO: Macroregiunea trei",
             "RO: Macroregiunea patru",
             "SE: Östra Sverige", "SE: Södra Sverige", "SE: Norra Sverige",
             "GB-GBN: North East (England)", "GB-GBN: North West (England)",
             "GB-GBN: Yorkshire and the Humber", "GB-GBN: East Midlands (England)",
             "GB-GBN: West Midlands (England)", "GB-GBN: East of England",
             "GB-GBN: London", "GB-GBN: South East (England)", "GB-GBN: South West (England)",
             "GB-GBN: Wales", "GB-GBN: Scotland", "GB-NIR: Northern Ireland",
             "GR: Voreia Ellada", "GR: Kentriki Ellada", "GR: Attiki", "GR: Nisia Aigaiou, Kriti",
             "UA: West","UA: Centre", "UA: North", "UA: East", "UA: South",
             "CY: Kypros / Kibris", "CY: Northern Cyprus",
             "RS: Centralna Srbija", "RS: Vojvodina",
             "BG: Severna i iztochna Bulgaria", "BG: Yugozapadna i yuzhna tsentralna Bulgaria",
             "CH: Schweiz/Suisse/Svizzera", "CZ: Ceska Republika", "DK: Danmark",
             "EE: Eesti", "FI: Manner-Suomi", "HR: Hrvatska", "IE: Ireland",
             "IS: Ísland", "LT: Lietuva", "LU: Luxembourg (Grand-Duché)",
             "LV: Latvija", "MT: Malta", "NO: Norge", "PT: Continente",
             "SI: Slovenija", "SK: Slovenská Republika", "BY: Belarus", "TK: Turkey",
             "GE: Georgia", "MD: Moldova", "ME: Montenegro", "AM: Armenia",
             "BA: Bosna i Hercegovina", "AL: Albania", "MK: Poranesnata jugoslovenska Republika Makedonija",
             "RS-KM: Kosovo")
reg_new <- c("Ostösterreich", "Südösterreich", "Westösterreich",
             "Région de Bruxelles-Capitale / Brussels Hoofdstedelijk Gewest",
             "Vlaams Gewest", "Région Wallonne",
             "Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", "Bremen",
             "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen",
             "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen",
             "Sachsen-Anhalt", "Schleswig-Holstein", "Thüringen",
             "Noroeste", "Noreste", "Comunidad de Madrid", "Centro (E)", "Este",
             "Sur", "Canarias",
             "Île de France", "Bassin Parisien", "Nord - Pas-de-Calais", "Est",
             "Ouest", "Sud-Ouest", "Centre-Est", "Méditerranée",
             "Közép-Magyarország", "Dunántúl", "Alföld És Észak",
             "Nord-Ovest", "Nord-Est", "Centro (I)", "Sud", "Isole",
             "Noord-Nederland", "Oost-Nederland", "West-Nederland", "Zuid-Nederland",
             "Region Centralny", "Region Poludniowy", "Region Wschodni", "Region Pólnocno-Zachodni",
             "Region Poludniowo-Zachodni", "Region Pólnocny",
             "Macroregiunea unu", "Macroregiunea doi", "Macroregiunea trei",
             "Macroregiunea patru",
             "Östra Sverige", "Södra Sverige", "Norra Sverige",
             "North East (England)", "North West (England)", "Yorkshire and the Humber",
             "East Midlands (England)", "West Midlands (England)", "East of England",
             "London", "South East (England)", "South West (England)", "Wales",
             "Scotland", "Northern Ireland",
             "Voreia Ellada", "Kentriki Ellada", "Attiki", "Nisia Aigaiou, Kriti",
             "West (UA)","Centre (UA)", "North (UA)", "East (UA)", "South (UA)",
             "Kypros / Kibris", "Northern Cyprus",
             "Centralna Srbija", "Vojvodina",
             "Severna i iztochna Bulgaria", "Yugozapadna i yuzhna tsentralna Bulgaria",
             "Schweiz/Suisse/Svizzera", "Ceská Republika", "Danmark", "Eesti",
             "Manner-Suomi", "Hrvatska", "Ireland", "Ísland", "Lietuva", "Luxembourg (Grand-Duché)",
             "Latvija", "Malta", "Norge", "Continente", "Slovenija", "Slovenská Republika",
             "Belarus", "Turkey", "Georgia", "Moldova", "Montenegro", "Armenia",
             "Bosna i Hercegovina", "Albania", "Poranesnata jugoslovenska Republika Makedonija",
             "Kosovo")
EVS$reg <- plyr::mapvalues(EVS$reg, from = reg_old, to = reg_new)

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
  
  fair <- v63 #Will people be fair or make their advantage with you (10 point)
  fair[v63 %in% c(-5, -4, -3, -2, -1)] <- NA
  fair <- as.numeric(fair)
  
  job_sat <- v90 #Job satisfaction (10 point)
  job_sat[v90 %in% c(-5, -4, -3, -2, -1)] <- NA
  job_sat <- as.numeric(job_sat)
  
  siops <- v339SIOPS #Standard Index of Occupational Prestige Scala (1-100)
  siops[v339SIOPS %in% c(-5, -4, -3, -2, -1)] <- NA 
  siops <- as.numeric(siops)
  
  
}) #non-pecuniary factors


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##    1.5 Format pecuniary factors variables 
EVS %<>% within({ #pecuniary factors 
  nowork <- v89 #Are yourself employed
  nowork[v89 %in% c(-5, -4, -3, -2, -1)] <- NA
  nowork <- nowork - 1 #0 = Yes, 1 = No
  nowork <- factor(nowork, levels = c(0, 1), labels = c("Employed", "Unemployed"), 
                       ordered = F)
  
  inc_cat <- v353_r #Income recoded in three categories
  inc_cat[v353_r %in% c(-5, -4, -3, -2, -1)] <- NA
  inc_cat <- factor(inc_cat, levels = c(1, 2, 3), labels = c("Low", "Medium", "High"))
  
  inc_an <- v353YR #Annual income in euros
  inc_an[v353YR %in% c(-5, -4, -3, -2, -1)] <- NA
  inc_an <- as.numeric(inc_an)
  
  inc_mon <- v353MM #Monthly income in euros
  inc_mon[v353MM %in% c(-5, -4, -3, -2, -1)] <- NA
  inc_mon <- as.numeric(inc_mon)
  
  incppp_mon <- v353M_ppp #Monthly income after purchasing power parity
  incppp_mon[v353M_ppp %in% c(-5, -4, -3, -2, -1)] <- NA
  incppp_mon <- as.numeric(incppp_mon)
  
  isei <- v339ISEI #International Socio-Economic Index of Occupational Status
  isei[v339ISEI %in% c(-5, -4, -3, -2, -1)] <- NA 
  isei <- as.numeric(isei)
  
  inc_eq <- v198 #Income equality from 1 to 10 (1 indicating more equality)
  inc_eq[v198 %in% c(-5, -4, -3, -2, -1)] <- NA 
  inc_eq <- as.numeric(inc_eq)
  
}) #pecuniary factors


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##    1.6 Format work variables 
EVS %<>% within({ #work variables
  work_impo <- v1 #How important is work in your life (4 point)
  work_impo[v1 %in% c(-5, -4, -3, -2, -1)] <- NA
  work_impo <- as.numeric(work_impo)
  
  free_job <- v91 #How free are you to make decisions in your job (10 point)
  free_job[v91 %in% c(-5, -4, -3, -2, -1)] <- NA
  free_job <- as.numeric(free_job)
  
  talent <- v92 #Need job to develop your talents (5 point)
  talent[v92 %in% c(-5, -4, -3, -2, -1)] <- NA
  talent <- as.numeric(talent)
  talent <- (talent - 5) * -1
  
  duty <- v95 #Work is a duty towards society (5 point)
  duty[v95 %in% c(-5, -4, -3, -2, -1)] <- NA
  duty <- as.numeric(duty)
  duty <- (duty - 5) * -1
  
  work_first <- v96 #Work comes always first (5 point)
  work_first[v96 %in% c(-5, -4, -3, -2, -1)] <- NA
  work_first <- as.numeric(work_first)
  work_first <- (work_first - 5) * -1
  
  sup <- v341 #are you supervising someone
  sup[v341 %in% c(-5, -4, -3, -2, -1)] <- NA
  sup <- (sup - 2) * -1 #0 = No, 1 = Yes
  sup <- factor(sup, levels = c(0, 1), labels = c("Subordinate", "Supervisor"), 
                       ordered = F)
  
  entre <- v338 #how was the employment relation in your last job?
  entre[v338 %in% c(-5, -4, -3, -2, -1)] <- NA
  entre <- factor(entre, levels = c(1, 2, 3), labels = c("Employee", "Entrepreneur", "Never had paid job"), 
                       ordered = F)
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
                     conf_church, conf_armed, conf_educ, 
                     conf_press, conf_tu, conf_police,
                     conf_parl, conf_cs, conf_socs, 
                     conf_eu, conf_nato, conf_un, 
                     conf_hs, conf_just, conf_gov)

inst_trust %<>% na.omit()
cor_inst_trust <- cor(inst_trust) %>% round(2) #get correlation matrix
inst_trust %>% as.matrix() %>% alpha(check.keys = T) #compute Cronbach's alpha 

# Constructing institutional trust index and trimming scale
EVS %<>% within({ #index institutional trust
  inst_trust <- (conf_church + conf_armed + conf_educ + 
                 conf_press + conf_tu + conf_police +
                 conf_parl + conf_cs + conf_socs + 
                 conf_eu + conf_nato + conf_un + 
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
  
  av_pub_f <- v247 #avoid public transfer fair
  av_pub_f[v247 %in% c(-5, -4, -3, -2, -1)] <- NA
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
  
  age_cat <- age_r2 #age recoded in three categories
  age_cat[age_r2 %in% c(-5, -4, -3, -2, -1)] <- NA
  age_cat <- factor(age_cat, levels = c(1, 2, 3), labels = c("15-29", "30 - 49", "50 and older"), ordered = T)
  
  mar_stat <- v313 #Current marital status
  mar_stat[v313 %in% c(-5, -4, -3, -2, -1)] <- NA
  mar_stat <- as_factor(mar_stat, ordered = F)
  
  edu_cat3 <- v336_r #Education recoded
  edu_cat3[v336_r %in% c(-5, -4, -3, -2, -1)] <- NA
  edu_cat3 <- factor(edu_cat3, levels = c(1, 2, 3),
                     labels = c("Lower", "Middle", "Upper"),ordered = T)
  
  edu_cat7 <- v336 #Education after ISCED 1-digit (6 point)
  edu_cat7[v336 %in% c(-5, -4, -3, -2, -1)] <- NA
  edu_cat7 <- factor(edu_cat7, levels = c(0, 1, 2, 3, 4, 5, 6), 
                     labels = c("Pre-primary education or no education",
                                "Primary education or first stage of basic education",
                                "Lower secondary or second stage of basic education",
                                "(Upper) secondary education",
                                "Post-secondary non-tertiary education",
                                "First stage of tertiary education",
                                "Second stage of tertiary education") ,ordered = T)

  edu <- v336_2 #Education after ISCED 2-digit
  edu[v336_2 %in% c(-5, -4, -3, -2, -1)] <- NA
  edu <- as.numeric(edu)
  
  town <- v370a #size of town
  town[v370a %in% c(-5, -4, -3, -2, -1)] <- NA
  town <- as.numeric(town)
  
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
EVS_nat <- dplyr::select(EVS, nation, nowork, sat) 
EVS_reg <- dplyr::select(EVS, nation, c_code, reg, intp_trust, inst_trust, trust_wrth, fair)

#Creating aggregated averages 
EVS_nat <- EVS_nat %>% 
  dplyr::group_by(nation) %>% 
  dplyr::summarise(unemployment = mean(as.numeric(nowork), na.rm = T),
                  life_sat = mean(sat, na.rm = T))

EVS_reg <- EVS_reg %>% 
  dplyr::group_by(reg) %>% 
  dplyr::summarise(trust_wrth_reg = mean(trust_wrth, na.rm = T), 
            intp_trust_reg = mean(as.numeric(intp_trust), na.rm = T),
            inst_trust_reg = mean(as.numeric(inst_trust), na.rm = T),
            fair_reg = mean(as.numeric(fair), na.rm = T))


#----------------------------------------------------------------------------------------

##    2.5 Joining the complete Level-3 data files

#National Data File (Level-3)
nat_data <- left_join(wb_data, PN_select, by = "nation")
nat_data <- left_join(nat_data, EVS_nat, by = "nation")
nat_data$nation <- as_factor(nat_data$nation)

nat_data <- dplyr::select(nat_data, nation, unemployment, GDPpc, gini, hdi, 
                          fhrate, life_sat)

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

##      5.1 Get NUTS data for 2006 from eurostat and edit EVS_reg

# Obtain NUTS 1 geodata 
nuts_data <- get_eurostat_geospatial(output_class = "sf", resolution = "60", nuts_level = "1", year = "2006") #NUTS 2006

# Plot NUTS 1 regions and zoom in appropiately 
ggplot(data = nuts_data) + 
  geom_sf()+
  coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)

#Exclude problematic regions because not contained in EVS or will cause 
    #problems with regional conflicts
nuts_data <- nuts_data %>% 
              filter(NUTS_NAME != "Åland") %>% 
              filter(NUTS_NAME != "Região Autónoma da Madeira") %>%  
              filter(NUTS_NAME != "Região Autónoma dos Açores") %>% 
              filter(NUTS_NAME != "Départements d'Outre-Mer") %>% 
              filter(NUTS_ID != "CY0") %>% 
              filter(CNTR_CODE != "LI")

#Exclude regions to be replaced by their national geodata for reasons of 
 #missing geo data or no data in EVS
nuts_data <- nuts_data %>% 
  filter(CNTR_CODE != "TR")

#Renaming of regions
nuts_data %<>% within({
  NUTS_NAME[CNTR_CODE %in% c("MK")] <- "Poranesnata jugoslovenska Republika Makedonija"
  
  NUTS_NAME[CNTR_CODE %in% c("ME")] <- "Montenegro"
  
  NUTS_NAME[NUTS_ID %in% c("BG3")] <- "Severna i iztochna Bulgaria"
  NUTS_NAME[NUTS_ID %in% c("BG4")] <- "Yugozapadna i yuzhna tsentralna Bulgaria"

  NUTS_NAME[NUTS_ID %in% c("GR1")] <- "Voreia Ellada"
  NUTS_NAME[NUTS_ID %in% c("GR2")] <- "Kentriki Ellada"
  NUTS_NAME[NUTS_ID %in% c("GR3")] <- "Attiki"
  NUTS_NAME[NUTS_ID %in% c("GR4")] <- "Nisia Aigaiou, Kriti"
  })

# Select necessary variables in nuts_data 
nuts_data <- dplyr::select(nuts_data, CNTR_CODE, NUTS_NAME, geometry)

##Create new regional data set from EVS

EVS_reg2 <- dplyr::select(EVS, nation, reg, c_code,inst_trust, trust_wrth, intp_trust, fair)

#Renaming regions that have no geodata and will be taken as national regions
EVS_reg2$reg <- mapvalues(EVS_reg2$reg, from = c("West (UA)", "North (UA)", "Centre (UA)", 
                            "East (UA)", "South (UA)"), to = c("Ukraine", "Ukraine",
                                        "Ukraine", "Ukraine", "Ukraine"))
EVS_reg2$reg <- mapvalues(EVS_reg2$reg, from = c("Centralna Srbija", "Vojvodina"), 
                                        to = c("Serbia", "Serbia"))
EVS_reg2 <- EVS_reg2 %>% 
  dplyr::group_by(reg, nation, c_code) %>% 
  dplyr::summarise(trust_wrth_reg = mean(trust_wrth, na.rm = T), 
                   intp_trust_reg = mean(as.numeric(intp_trust), na.rm = T),
                   inst_trust_reg = mean(inst_trust, na.rm = T),
                   fair_reg = mean(fair, na.rm = T))

# Exclude problematic regions fomr new data set
EVS_reg2 <- EVS_reg2[-111, ] # exclude empty row for Swedish region in EVS
EVS_reg2 <- EVS_reg2 %>% 
  filter(c_code != c("CY"))
     

##      5.2 Get nation data for regions that are not listed

# Get nation data for relevant variables

# Select necessary countries (i.e., those that have no regions in reg_geodata)
selec_code <- c("AL", "AM", "BA", "BY", "GE", "RS-KM", "MD", "RS", "TR", "UA")
selec_nat <- c("Albania", "Armenia", "Belarus", "Bosnia Herzegovina", "Georgia", 
               "Kosovo","Moldova", "Serbia", "Turkey", "Ukraine")

# Get geodata for necessary countries
natreg_geo <- eur_s %>% 
  dplyr::select(nation, geometry) %>% 
  filter(nation %in% selec_nat)
natreg_geo <- cbind(selec_code, natreg_geo)

# Add the national region data to the NUTS data
colnames(natreg_geo) <- c("CNTR_CODE", "NUTS_NAME", "geometry")
nuts_data <- rbind(nuts_data, natreg_geo)

##      5.4 Merge EVS_reg and nuts_data

# Rename NUTS_NAME variable in nuts_data
colnames(nuts_data) <- c("CNTR_CODE", "reg", "geometry")

# Merge datasets
reg_geodata <- left_join(EVS_reg2, nuts_data, by = "reg")
reg_geodata <- reg_geodata[,-8]


#---------------------------------------------------------------------------------------------

# FINAL DATASETS
#--------------------------------------------------------------
#------------------------------------------------------------##
#Export final Data File                                      ##
write_rds(EVS_final, path = "Data/EVS_final.rds")            ##
#------------------------------------------------------------##
#--------------------------------------------------------------
#Export nation Data File                                     ##
write_rds(nat_geodata, path = "Data/Nation_geoData.rds")     ##
#------------------------------------------------------------##
#--------------------------------------------------------------
#Export regions Data File                                    ##
write_rds(reg_geodata, path = "Data/Region_geoData.rds")     ##
#------------------------------------------------------------##
#--------------------------------------------------------------