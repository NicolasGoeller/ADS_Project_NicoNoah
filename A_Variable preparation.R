###
# This script provides basic preparation of variables used
# for further analyses. 

library(haven)
library(plyr)
library(tidyverse)
library(magrittr)
library(psych)


###  Level-1-variables

## European Value Survey 2008
EVS_2008 <- read_spss("Data/ZA4800_v4-0-0.sav") # Seems to have the missing countries

#Preliminary exclusions and selection
EVS <- EVS_2008 %>% 
              filter(country != 643) %>% #Exclude Russian Federation
              filter(country != 31) %>%  #Exclude Azerbaijan
      select( #Pre-selection of variables
          country, c_abrv, v371b_N1, v66, v8, #base
          v62, v63, v64, v90, v339SIOPS, v203, #non-pecuniary
          v89, v353YR, v353MM, v353M_ppp, v339ISEI, v198, #pecuniary
          v205:v218, v222, #index institutional trust
          v233, v234, v235, v237, v239, v245, v247, #index justification
          v302, v303, v313, v336, v336_2, v370a, v337) #demographics

EVS %<>% within({ #base variables
  cntry <- country #Country
  cntry[country %in% c(-5, -4, -3, -2, -1)] <- NA
  cntry[country %in% c(197)] <- 196 #Overwrite North Cyprus with Cyprus
  cntry[country %in% c(909)] <- 372 #Overwrite Northern Ireland with Great Britain
  cntry <- as_factor(cntry, ordered = F)
  
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
EVS$cntry <- mapvalues(EVS$cntry, from = c("Great Britain"),
                             to = c("United Kingdom")) 
EVS$cntry <- droplevels(EVS$cntry, exclude = c(as.character(c("Northern Cyprus", 
                                  "Russian Federation", "Northern Ireland")))) 
EVS$reg <- mapvalues(EVS$reg, from = c("GB-NIR: Northern Ireland"), 
      to = c("GB-GBN: Northern Ireland"))
EVS$reg <- droplevels(EVS$reg, 
                exclude = c(as.character(c("RU: Central Federal District", 
                      "RU: North West federal district", "RU: South Federal district",
                      "RU: Privolzhsky federal district", "RU: Urals federal district", 
                      "RU: Siberian federal district", "RU: Far East federal district"  )))) 

EVS %<>% within({ #non-pecuniary factors
  trst_d <- v62 #Dummy: Do you think you can trust other people
  trst_d[v62 %in% c(-5, -4, -3, -2, -1)] <- NA
  
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
  work <- v89 #Unemployed/ employed
  work[v89 %in% c(-5, -4, -3, -2, -1)] <- NA
  
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
trst_wrth <- select(EVS, av_pub_f, av_tax, bribe,
                    lying, joy, ch_tax, cl_sb)
trst_wrth %<>% na.omit()
trst_wrth_cor <- cor(trst_wrth) %>% round(2) #get correlation matrix
trst_wrth_cor %>% as.matrix() %>% alpha(check.keys = T) #compute Cronbach's alpha 

# Constructing trustworthiness index and trimming scale
EVS %<>% within({ #index trustworthiness
  trst_wrth <- (av_pub_f + av_tax + bribe +
                lying + joy + ch_tax + cl_sb)
  
  trst_wrth <- trst_wrth/7
}) #index trustworthiness: scale from 0 to 9 i.e., from low trust in inst. to high trust in inst. 

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


### Level-2-variables 

# Countries used in analysis
nat <- c("Albania", "Austria", "Armenia", "Belgium", "Bosnia Herzegovina", 
         "Bulgaria", "Belarus", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
         "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", 
         "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
         "Moldova", "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", 
         "Romania", "Serbia", "Slovak Republic", "Slovenia", "Spain", "Sweden", 
         "Switzerland", "Turkey", "Ukraine", "Macedonia", "United Kingdom", "Kosovo")


##  1. Country level data from Pippa Norris
PN <- read_spss("Data/Democracy Cross-National Data V4.1 09092015.sav")

PN %<>% within({ #select necessary variables
  country <- Nation
  country <- as_factor(country)
  
  fhrate <- fhrate08 #Freedom House: Political Rights and Civil Liberties (1=highest, 7=lowest)
  fhrate <- as.numeric(fhrate)
  
  voice_acc <- WGI_voice2008 #Voice and Accountability (WGI 2014)
  voice_acc <- as.numeric(voice_acc)
  
  press_free <- FreePress2008 #Freedom House: Freedom of Press (low=free) (6 point)
  press_free <- as.numeric(press_free)
  
  hdi <- UNDP_HDI2008 #Human Development Index
  hdi <- as.numeric(hdi)
})

#Adapting country names
PN$country <- mapvalues(PN$country, from = c("Bosnia & Herzegovina","Moldova, Republic of", "Slovakia"), 
                                 to = c("Bosnia Herzegovina", "Moldova", "Slovak Republic"))

#New subset with necessary variables
PN_selected <- select(PN, country, fhrate, voice_acc, press_free, hdi)

#Filling up missing values
PN_selected[91,2] <- 5.5 #Freedom House rating 2008 from Freedom House
PN_selected[91,5] <- 0.743 #HDI value 2008 from Serbia
PN_selected[118,2] <- 3.0 #Freedom House rating 2008 from Freedom House


## 2. World Bank data on GDP per capita and Gini-coefficent

##  GDP per capita in 2018 USD
gdp_per_cap <- read.csv("Data/GDP_per_capita_current_USD_data.csv", header = F)

#omit first two rows and last column (dataset description)
gdp_per_cap <- gdp_per_cap[3:267,1:62]

#Renaming header
colnames(gdp_per_cap) <- as.character(unlist(gdp_per_cap[1,]))
gdp_per_cap = gdp_per_cap[-1, ]
colnames(gdp_per_cap)[colnames(gdp_per_cap)=="52"] <- "country"
colnames(gdp_per_cap)[colnames(gdp_per_cap)=="47"] <- "Country_Code"

#Adapting country names
gdp_per_cap$country <- mapvalues(gdp_per_cap$country, from = c("Bosnia and Herzegovina", "Macedonia, FYR"), 
                          to = c("Bosnia Herzegovina", "Macedonia"))

#New dataset
gdp_per_cap_new <- select(gdp_per_cap, country, Country_Code, "2008")

##  Gini coefficient
Gini <- read.csv("Data/Gini_WB.csv", header = F)

#omit first two rows and last column (dataset description)
Gini <- Gini[3:267,1:62]

#Renaming header
colnames(Gini) <- as.character(unlist(Gini[1,]))
Gini = Gini[-1, ]
colnames(Gini)[colnames(Gini)=="52"] <- "country"
colnames(Gini)[colnames(Gini)=="47"] <- "Country_Code"

#Adapting country names
Gini$country <- mapvalues(Gini$country, from = c("Bosnia and Herzegovina", "Macedonia, FYR"), 
          to = c("Bosnia Herzegovina", "Macedonia"))

#New dataset
Gini_new <- select(Gini, country, "2008")

## Create a combined dataset for World Bank data
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


## 3. European Value Survey 2008

# Creating a subsetted dataset for variable to be aggregated
EVS_macro <- select(EVS, country, reg,  inst_trust,  trst_wrth)
#trst_d,


#EVS_macro$trst_d_agg <- EVS_macro %>% 
  #group_by(country) %>% 
  #mutate(mean(trst_d, na.rm = T))

EVS_macro$trst_wrth_agg <- EVS_macro %>% 
  group_by(reg) %>% 
  mutate(mean(trst_wrth, na.rm = T))

EVS_macro %<>% within( { #Aggegating variables
  inst_trust_agg <- EVS_macro %>% 
  group_by(country) %>% 
  mutate(mean(inst_trust, na.rm = T))
})


##  4. Joining the complete Level-2 data file
macrodata <- left_join(wb_data, PN_selected, by = "country")
macrodata$country <- as_factor(macrodata$country)

macrodata %<>% within({ #Creating centered macro variables
  gini_c <- gini - mean(gini, na.rm = T)
  
  gdppc_c <- GDPpc - mean(GDPpc, na.rm =T)
  
  hdi_c <- hdi - mean(hdi, na.rm = T)
  
  press_free_c <- press_free - mean(press_free, na.rm = T)
  
  voice_acc_c <- voice_acc - mean(voice_acc, na.rm = T)
  
  fhrate_c <- fhrate - mean(fhrate, na.rm = T)
})
