###
# This script provides basic preparation of variables used
# for further analyses. 

library(haven)
library(plyr)
library(tidyverse)
library(magrittr)
library(psych)


###   Read European Values Survey 2008

EVS <- read_rds("Data/EVS.rds")

## Level-1-variables: Data formatting
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
  
  sat <- v66 #Life satisfaction (10 point)
  sat[v66 %in% c(-5, -4, -3, -2, -1)] <- NA
  sat <- as.numeric(sat)
  sat_c <- sat - mean(sat, na.rm = T)
  
  happ <- v8 #Feeling of happiness (4 point)
  happ[v8 %in% c(-5, -4, -3, -2, -1)] <- NA
  happ <- as.numeric(happ)
  happ_c <- happ - mean(happ, na.rm = T)
}) #base variables

#Adapting geo-variables on factor level: Renaming and exclusion of unused factor 
 #levels in variables "reg" and "cntry"
EVS$nation <- mapvalues(EVS$nation, from = c("Great Britain"),
                             to = c("United Kingdom")) 
EVS$nation <- droplevels(EVS$nation, exclude = c(as.character(c("Northern Cyprus", 
                                  "Russian Federation", "Northern Ireland")))) 
EVS$reg <- mapvalues(EVS$reg, from = c("GB-NIR: Northern Ireland"), 
      to = c("GB-GBN: Northern Ireland"))
EVS$reg <- droplevels(EVS$reg, 
                exclude = c(as.character(c("RU: Central Federal District", 
                      "RU: North West federal district", "RU: South Federal district",
                      "RU: Privolzhsky federal district", "RU: Urals federal district", 
                      "RU: Siberian federal district", "RU: Far East federal district"  )))) 

EVS %<>% within({ #non-pecuniary factors
  intp_trust <- v62 #Dummy: Do you think you can trust other people
  intp_trust[v62 %in% c(-5, -4, -3, -2, -1)] <- NA
  intp_trust <- (intp_trust - 2) * -1 #0 = No, 1 = Yes
  
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

EVS %<>% within({ #pecuniary factors 
  nowork <- v89 #Are yourself employed
  nowork[v89 %in% c(-5, -4, -3, -2, -1)] <- NA
  nowork <- nowork - 1 #0 = Yes, 1 = No
  
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
inst_trust <- select(EVS,
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
trust_wrth <- select(EVS, av_pub_f, av_tax, bribe,
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

#Demograhics variables
EVS %<>% within({ #demographics 
  sex <- v302 #Sex
  sex[v302 %in% c(-5, -4, -3, -2, -1)] <- NA
  
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

### Level-2-variables 

# Countries used in analysis
nat <- c("Albania", "Austria", "Armenia", "Belgium", "Bosnia Herzegovina", 
         "Bulgaria", "Belarus", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
         "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", 
         "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
         "Moldova", "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", 
         "Romania", "Serbia", "Slovak Republic", "Slovenia", "Spain", "Sweden", 
         "Switzerland", "Turkey", "Ukraine", "Macedonia", "United Kingdom", "Kosovo")

#----------------------------------------------------------------------------------------

##  1. Read country level data from Pippa Norris

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

## 2. Read World Bank data on GDP per capita and Gini-coefficent

wb_data <- read_rds("Data/WB_Data.rds")

#Renaming of country variable
wb_data$nation <- wb_data$country

#----------------------------------------------------------------------------------------

## 3. European Value Survey 2008: Level-2-variables

# Creating a subsetted dataset for variable to be aggregated
EVS_nat <- select(EVS, nation, nowork) # backup: intp_trust
EVS_reg <- select(EVS, nation, reg, intp_trust, inst_trust, trust_wrth)

#Creating 
EVS_nat <- EVS_nat %>% 
  group_by(nation) %>% 
  summarise(unemployment = mean(nowork, na.rm = T))

EVS_reg <- EVS_reg %>% 
  group_by(reg) %>% 
  summarise(trust_wrth_reg = mean(trust_wrth, na.rm = T), 
            intp_trust_reg = mean(intp_trust, na.rm = T),
            inst_trust_reg = mean(inst_trust, na.rm = T))

#----------------------------------------------------------------------------------------

##  4. Joining the complete Level-2 and Level-3 data files

#National Data File (Level-3)
nat_data <- left_join(wb_data, PN_select, by = "nation")
nat_data <- left_join(nat_data, EVS_nat, by = "nation")
nat_data$nation <- as_factor(nat_data$nation)

nat_data <- select(nat_data, nation, unemployment, GDPpc, gini, hdi, press_free, 
                   voice_acc, fhrate)

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

###  5. Combining variables of all 3 Levels
EVS_final <- full_join(EVS, nat_data, by = "nation")
EVS_final <- full_join(EVS_final, EVS_reg, by = "reg")

#Export final Data File
write_rds(EVS_final, path = "Data/EVS_final.rds")
