###
# This script provides basic preparation of variables used
# for further analyses. 

library(haven)
library(tidyverse)
library(magrittr)
install.packages("psych")
library(psych)

EVS_2008 <- read_spss("Data/ZA4800_v4-0-0.sav") # Seems to have the missing countries

#Preliminary exclusions
EVS_2008 <- EVS_2008 %>% 
              filter(country != 643 | 31) #Russian Federation, Azerbaijan

EVS <- select(EVS_2008, #Pre-selection of variables
              country, c_abrv, v371b_N1, v66, v8, #base
              v62, v63, v64, v90, v339SIOPS, #non-pecuniary
              v89, v353YR, v353MM, v353M_ppp, v339ISEI, #pecuniary
              v302, v303, v313, v336, v336_2, v370a, v337) #demographics

EVS %<>% within({ #base variables
  cntry <- country #Country
  cntry[country %in% c(-5, -4, -3, -2, -1)] <- NA
  cntry[country %in% c(197)] <- 196 #Overwrite North Cyprus with Cyprus
  cntry[country %in% c(909)] <- 372 #Overwrite Northern Ireland with Great Britain
  cntry <- as_factor(cntry, ordered = F)
  
  c_code <- c_abrv #Country code abbreviation
  
  reg <- v371b_N1 #Region on federal state level
  reg[v371b_N1 %in% c(90923)] <- "GB-GBN: Northern Ireland"
  reg[country %in% c(197)]  <- "CY: Northern Cyprus"
  reg[country %in% c(792)] <- "TK: Turkey"
  reg <- as_factor(reg, ordered = F)
  
  sat <- v66 #Life satisfaction (10 point)
  sat[v66 %in% c(-5, -4, -3, -2, -1)] <- NA
  sat <- as.numeric(sat)
  
  happ <- v8 #Feeling of happiness (4 point)
  happ[v8 %in% c(-5, -4, -3, -2, -1)] <- NA
  happ <- as.numeric(happ)
}) #base variables

EVS %<>% within({ #non-pecuniary factors
  trst_d <- v62 #Dummy: Do you think you can trust other people
  trst_d[v62 %in% c(-5, -4, -3, -2, -1)] <- NA
  
  help <- v64 #Most people are helpful or look out for themselves (10 point)
  help[v64 %in% c(-5, -4, -3, -2, -1)] <- NA
  help <- as.numeric(help)
  
  fair <- v63 #Will people be fair or make their advantage with you (10 point)
  fair[v63 %in% c(-5, -4, -3, -2, -1)] <- NA
  fair <- as.numeric(fair)
  
  job_sat <- v90 #Job satisfaction (10 point)
  job_sat[v90 %in% c(-5, -4, -3, -2, -1)] <- NA
  job_sat <- as.numeric(job_sat)
  
  siops <- v339SIOPS #Standard Index of Occupational Prestige Scala (1-100)
  siops <- as.numeric(siops)
  
  less_money <- v203 #less emphasis on money and material possession 
  less_money[v203 %in% c(-5, -4, -3, -2, -1)] <- NA 
  less_money <- as.numeric(less_money)
  
}) #non-pecuniary factors

EVS %<>% within({ #pecuniary factors 
  work <- v89 #Unemployed/ employed
  work[v89 %in% c(-5, -4, -3, -2, -1)] <- NA
  
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
  isei <- as.numeric(isei)
  
  inc_eq <- v198 #Income equality from 1 to 10 (1 indicating more equality)
  inc_eq[v198 %in% c(-5, -4, -3, -2, -1)] <- NA 
  inc_eq <- as.numeric(inc_eq)
  
}) #pecuniary factors

EVS %<>% within({ #demographics 
  sex <- v302 #Sex
  sex[v302 %in% c(-5, -4, -3, -2, -1)] <- NA
  
  age <- v303 #Age recoded from birth year
  age[v303 %in% c(-5, -4, -3, -2, -1)] <- NA
  age <- as.numeric(age)
  age <- 2008 - age
  
  mar_stat <- v313 #Current marital status
  mar_stat[v313 %in% c(-5, -4, -3, -2, -1)] <- NA
  mar_stat <- as_factor(mar_stat, ordered = F)
  
  edu_cat <- v336 #Education after ISCED code (6 point)
  edu_cat[v336 %in% c(-5, -4, -3, -2, -1)] <- NA
  edu_cat <- as_factor(edu_cat, ordered = T)
  
  edu <- v336_2 #Education after ISCED 2-digit
  edu[v336_2 %in% c(-5, -4, -3, -2, -1)] <- NA
  edu <- as.numeric(edu)
  
  town <- v370a #size of town
  town[v370a %in% c(-5, -4, -3, -2, -1)] <- NA
  town <- as.numeric(town)
  
  employ <- v337 #Employment status
  employ[v337 %in% c(-5, -4, -3, -2, -1)] <- NA
  employ <- as_factor(employ, ordered = F)
}) #demographics

EVS %<>% na.omit()

nat <- c("Albania"(30.0), "Austria"y, "Armenia"y, "Belgium"y, "Bosnia Herzegovina"x, 
    "Bulgaria"y, "Belarus"y, "Croatia"x, "Cyprus"y, "Czech Republic"y, "Denmark"y, 
    "Estonia"y, "Finland"y, "France"y, "Georgia"y, "Germany"(30.2), "Greece"y, "Hungary"y, 
    "Iceland"y, "Ireland"y, "Italy"y, "Latvia"y, "Lithuania"y, "Luxembourg"y, "Malta"y,
    "Moldova"y, "Montenegro"y, "Netherlands"y, "Norway"y, "Poland"(27.7), "Portugal"y, 
    "Romania"y, "Serbia"x, "Slovak Republic"y, "Slovenia"y, "Spain"y, "Sweden"y, 
    "Switzerland"y, "Turkey"y, "Ukraine"y, "Macedonia"x, "Great Britain"y, "Kosovo"x)


### Level-2-variables 

##1. Read data from Pipa Norris
PN <- read_spss("Data/Democracy Cross-National Data V4.1 09092015.sav")

PN %<>% within({ #select necessary variables
  
  fhrate <- fhrate08 #Freedom House: Political Rights and Civil Liberties (1=highest, 7=lowest)
  fhrate <- as.numeric(fhrate)
  
  fhcat <- fhcat08 #Freedom House: Status of Freedom (1=free, 2=partly free, 3=not free)
  fhcat <- as.factor(fhcat)
  
  voice_acc <- WGI_voice2008 #Voice and Accountability (WGI 2014)
  voice_acc <- as.numeric(voice_acc)
  
  press_free <- FreePress2008 #Freedom House: Freedom of Press (low=free)
  
  press_free <- as.numeric(press_free)
  hdi <- UNDP_HDI2008 #Human Development Index
  hdi <- as.numeric(hdi)
  
})

PN_selected <- select(PN, Nation, fhrate, fhcat, voice_acc, press_free, hdi)

#Build new subset with necessary variables and country names
PN_selected %<>% na.omit()


## 2. Read World Bank data

gdp_per_cap <- read.csv("Data/GDP_per_capita_current_USD_data.csv", header = F)

#omit first two rows (dataset description)
gdp_per_cap <- gdp_per_cap[3:267,]

#unlist rows to make first row as header
colnames(gdp_per_cap) <- as.character(unlist(gdp_per_cap[1,]))
gdp_per_cap = gdp_per_cap[-1, ]

# omit (new) first row 
#gdp_per_cap <- gdp_per_cap[4:265,]

colnames(gdp_per_cap)[colnames(gdp_per_cap)=="52"] <- "Country_Names"
colnames(gdp_per_cap)[colnames(gdp_per_cap)=="47"] <- "Country_Code"

# new data set
gdp_per_cap_new <- select(gdp_per_cap, Country_Names, Country_Code, "2008")





Gini <- read.csv("Data/Gini_WB.csv", header = F)
Gini <- Gini[3:267,]
names <- colnames(Gini[1,])
write.csv(Gini, file = "WB_Gini.csv" ) 
Gini_new <- read.csv("Data/WB_Gini.csv", header =  T)

Gini_final <- select(Gini_new, V1, V51:V55)
?colnames()
glimpse(Gini_final)
 ?read.csv


## 4. Construct index for social capial
## Draw on initial EVS_2008 to obtain proxies for social capital 

# Our own social capital index with questionable intercorrelatedness:

EVS_2008 %<>% within({
  
  imp_fam <- v2 #importnace of family (=very important; 4=not at all)
  imp_fam[v2 %in% c(-5, -4, -3, -2, -1)] <- NA
  imp_fam <- (imp_fam-4)*-1
  imp_fam <- as.numeric(imp_fam)
  
  imp_frds <- v3 #importance of friends and acquaintances (=very important; 4=not at all) 
  imp_frds[v3 %in% c(-5, -4, -3, -2, -1)] <- NA
  imp_frds <- (imp_frds-4)*-1
  imp_frds <- as.numeric(imp_frds)
  
  trust <- v62 #people can be trusted/cant be too careful (dummy: 1=trusted; 2=be careful)
  trust[v62 %in% c(-5, -4, -3, -2, -1)] <- NA
  
  fair <- v63 #people try to take advantage or are fair (1=advantage; 10=fair)
  fair[v63 %in% c(-5, -4, -3, -2, -1)] <- NA
  fair <- as.numeric(fair)
  
  helpful <- v64 #people are helpful or look after themselves (1=look out for themselves; 10=helpful)
  helpful[v64 %in% c(-5, -4, -3, -2, -1)] <- NA
  helpful <- as.numeric(helpful)
  
  met_pep <- v97 #meeting nice people (1=very important; 4=not important at all)
  met_pep[v97 %in% c(-5, -4, -3, -2, -1)] <- NA
  met_pep <- (met_pep-4)*-1
  met_pep <- as.numeric(met_pep)
  
  conc_fam <- v284 #concerned with familiy (1=very much; 5 not at all)
  conc_fam[v284 %in% c(-5, -4, -3, -2, -1)] <- NA
  conc_fam <- (conc_fam-5)*-1
  conc_fam <- as.numeric(conc_fam)

  conc_neigh <- v285 #concerned with people in neighbourhood (1=very much; 5 not at all)
  conc_neigh[v285 %in% c(-5, -4, -3, -2, -1)] <- NA
  conc_neigh <- (conc_neigh-5)*-1
  conc_neigh <- as.numeric(conc_neigh)
  
  conc_region <- v286 #concerned with people in region (1=very much; 5 not at all)
  conc_region[v286 %in% c(-5, -4, -3, -2, -1)] <- NA
  conc_region <- (conc_region-5)*-1
  conc_region <- as.numeric(conc_region)
  
})

#Construct social capital dataset
soc_cap_data <- select(EVS_2008, imp_fam,
                       imp_frds,
                       #fair,
                       #helpful,
                       met_pep, 
                       conc_fam,
                       conc_neigh,
                       conc_region)

soc_cap_data %<>% na.omit()

#Intercorrelations

cor_mat <- cor(soc_cap_data) %>% round(2) #get correlation matrix

soc_cap_data %>% as.matrix() %>% alpha(check.keys = T) #compute Cronbach's alpha 



# Interpersonal trust as one dimension of social capital

EVS_2008 %<>% within({
  
  trust <- v62 #people can be trusted/cant be too careful (dummy: 1=trusted; 2=be careful)
  trust[v62 %in% c(-5, -4, -3, -2, -1)] <- NA

  })


# Institutional trust as one dimension of social capital (How much confidence in following institutions [1=great deal; 4=none at all])

EVS_2008 %<>% within({
  
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
  
})

inst_trust <- select(EVS_2008, conf_church, conf_armed, conf_educ,
                     conf_press, conf_tu, conf_police,
                     conf_parl, conf_cs, conf_eu, 
                     conf_socs, conf_nato, conf_un, 
                     conf_hs, conf_just, conf_gov)

inst_trust %<>% na.omit()

#Intercorrelations

cor_inst_trust <- cor(inst_trust) %>% round(2) #get correlation matrix

inst_trust %>% as.matrix() %>% alpha(check.keys = T) #compute Cronbach's alpha 


