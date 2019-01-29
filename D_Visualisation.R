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
reg_geodata <- read_rds("Data/Region_geoData.rds")

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

###   2. Maps 

##    2.1 Create some plots

#     2.1.1 Countries and GDP per capita 
ggplot(data = nat_geodata)+
  geom_sf(aes(fill = GDPpc))+
  labs(fill = "GDP per capita 2008")+
  ggtitle("Europe Map", 
          subtitle = "Countries and GDP per capita")+
  coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)
  

#     2.1.2 Countries and Freedom House rating 2008 
#Issue titles replace the rendering of the plot
ggplot(data = nat_geodata)+
  geom_sf(aes(fill = fhrate))+
  labs(fill = "Freedom House Democracy Rating 2008")+
  ggtitle("Europe Map", 
          subtitle = "Countries and Freedom House Rating")+
  coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)


#     2.1.3 Countries and UNHDP HDI rating 2008 
ggplot(data = nat_geodata)+
  geom_sf(aes(fill = hdi))+
  labs(fill = "HDI Index 2008")+
  ggtitle("Europe Map", 
          subtitle = "Countries and HDI Index")+
  coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)

#     2.1.4 Countries and life_sat
ggplot(data = nat_geodata)+
  geom_sf(aes(fill = life_sat))+
  labs(fill = "Life satisfaction (from low to high) 2008")+
  ggtitle("Europe Map", 
          subtitle = "Countries and Life Satisfaction")+
  coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE)


#---------------------------------------------------------------------------------------------------------------------------------

##    2.2 Create plots with country indication 

#     2.2.1 Countries and GDP per capita 
ggplot(data = nat_geodata)+
  geom_sf(aes(fill = GDPpc))+
  labs(fill = "GDP per capita")+
  ggtitle("Europe Map", 
          subtitle = "Countries and GDP per capita")+
  coord_sf(xlim = c(-24, 50), ylim = c(33, 71), expand = FALSE) +
  geom_text(data= nat_data_vis, aes(x = X, y = Y, label = adm0_a3),
            color = "black", fontface = "bold" , size = 3, check_overlap = FALSE) #label for FRA is dis-orientated

#----------------------------------------------------------------------------------------------------------

###   3. Regional maps

#     3.1.1 Regions and trustworthiness 
ggplot(data = reg_geodata)+
  geom_sf(aes(fill = trust_wrth_reg))+
  labs(fill = "Trustworthiness (from low to high)")+
  ggtitle("Regional Europe Map", 
          subtitle = "NUTS 1 Regions and Trustworthiness")+
  coord_sf(xlim = c(-24, 32), ylim = c(33, 71), expand = FALSE)

#     3.1.2 Regions and interpersonal trust
ggplot(data = reg_geodata)+
  geom_sf(aes(fill = intp_trust_reg))+
  labs(fill = "Interpersonal trust (from low to high)")+
  ggtitle("Regional Europe Map", 
          subtitle = "NUTS 1 Regions and Interpersonal Trust")+
  coord_sf(xlim = c(-24, 32), ylim = c(33, 71), expand = FALSE)

#     3.1.2 Regions and institutional trust
ggplot(data = reg_geodata)+
  geom_sf(aes(fill = inst_trust_reg))+
  labs(fill = "Institutional trust (from low to high)")+
  ggtitle("Regional Europe Map", 
          subtitle = "NUTS 1 Regions and Institutional Trust")+
  coord_sf(xlim = c(-24, 32), ylim = c(33, 71), expand = FALSE)

#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------


###  4. Codebook

?data.set
?codebook

EVS_book <- dplyr::select(EVS_final,
                          nation, c_abrv, reg, eureg, sat,
                          siops, intp_trust, job_sat, nowork, isei, inst_trust,
                          trust_wrth, sex, age, mar_stat, edu_cat, edu, town)

EVS_book <- as.data.set(EVS_book)

EVS_book <- within(EVS_book,{
  
  #description() <- ""
  #wording() <- ""
  #measurement() <- ""
  #missing.values() <- c(NA)
  #annotation()["Source"] <- "European Value Survey 2008"
  #annotation()["Remark"] <- ""
  
  description(nation) <- "Country of residence"
  measurement(nation) <- "nominal"
  missing.values(nation) <- c(NA)
  annotation(nation)["Source"] <- "European Value Survey 2008"
  annotation(nation)["Remark"] <- "The Russian Federation was excluded for purposes of visualisation. Northern Ireland and Northern Cyprus were recoded to belong to their respective legal states. Azerbaijan was excluded as there were no respondents in the sample."
  
  description(c_abrv) <- "Country Code"
  measurement(c_abrv) <- "nominal"
  missing.values(c_abrv) <- c(NA)
  annotation(c_abrv)["Source"] <- "European Value Survey 2008"
  annotation(c_abrv)["Remark"] <- "Country codes were recoded to match changes in country selection (see 'Country of residence)."
  
  description(reg) <- "Region of residence"
  measurement(reg) <- "nominal"
  missing.values(reg) <- c(NA)
  annotation(reg)["Source"] <- "European Value Survey 2008"
  annotation(reg)["Remark"] <- "Based on NUTS-1 coding, region codes were recoded to match changes in country selection."
  
  description(eureg) <- "Geopgraphical region in Europe"
  measurement(eureg) <- "nominal"
  missing.values(eureg) <- c(NA)
  annotation(eureg)["Source"] <- "European Value Survey 2008, UN geograhical region code"
  annotation(eureg)["Remark"] <- "The item was recoded after the UN classification from the variable 'nation'. 'Armenia' was added to Southern Europe and 'Georgia' was added to Eastern Europe."
  
  description(sat) <- "Individual level: Life satisfaction"
  wording(sat) <- "All things considered, how satisfied are you with your life as a whole these days? (1-dissatisfied, 10-satisfied)"
  measurement(sat) <- "interval"
  missing.values(sat) <- c(NA)
  annotation(sat)["Source"] <- "European Value Survey 2008"
  annotation(sat)["Remark"] <- "The assumption of quasi-metric variable was taken when recoding to numeric."
  
  description(siops) <- "Individual level: SIOPS-Index"
  wording(siops) <- "Standard Index of Occupational Prestige Scala (Values: 0-100)"
  measurement(siops) <- "interval"
  missing.values(siops) <- c(NA)
  annotation(siops)["Source"] <- "European Value Survey 2008"
  annotation(siops)["Remark"] <- "The SIOPS-Index is  a widely known measure for social prestige of different occupations."
  
  description(intp_trust) <- "Individual level: Interpersonal trust"
  wording(intp_trust) <- "Generally speaking, would you say that most people can be trusted or that you can’t be too careful in dealing with people? (0-Distrustful, 1-Trustful)"
  measurement(intp_trust) <- "nominal"
  missing.values(intp_trust) <- c(NA)
  annotation(intp_trust)["Source"] <- "European Value Survey 2008"
  annotation(intp_trust)["Remark"] <- "The item was recoded and scales were switched."
  
  description(job_sat) <- "Individual level: Job satisfaction"
  wording(job_sat) <- "Overall, how satisfied or dissatisfied are you with your job? (1-dissatisfied, 10-satisfied)"
  measurement(job_sat) <- "interval"
  missing.values(job_sat) <- c(NA)
  annotation(job_sat)["Source"] <- "European Value Survey 2008"
  annotation(job_sat)["Remark"] <- "The assumption of quasi-metric variable was taken when recoding to numeric."
  
  description(nowork) <- "Individual level: Employment"
  wording(nowork) <- "Are you yourself employed or not? (0-Employed, 1-Unemployed)"
  measurement(nowork) <- "nominal"
  missing.values(nowork) <- c(NA)
  annotation(nowork)["Source"] <- "European Value Survey 2008"
  annotation(nowork)["Remark"] <- "Item was recoded as factor variable."
  
  description(isei) <- "Individual level: ISEI-Index"
  wording(isei) <- "International Socio-Economic Index of Occupational Status (Values: 16-90)"
  measurement(isei) <- "interval"
  missing.values(isei) <- c(NA)
  annotation(isei)["Source"] <- "European Value Survey 2008"
  annotation(isei)["Remark"] <- "The ISEI-Index is  a widely known measure for socio-economic status of different occupations."
 
  #------------------index inst_trust variables
  
  description(inst_trust) <- "Individual level: Index for institutional trust"
  measurement(inst_trust) <- "interval"
  missing.values(inst_trust) <- c(NA)
  annotation(inst_trust)["Source"] <- "European Value Survey 2008"
  annotation(inst_trust)["Remark"] <- "The index was calculated from 12 variables regarding trust in different institutions. All were coded after the same 10-scale. This procedure was based on research literature, refer to the theoretical paper for more detailed legitimation. The index variables have a high Cronbach's alpha of 0.89."
  
  #----------------index trustworthiness variables
  
  description(trust_wrth) <- "Individual level: Index for trustworthiness"
  measurement(trust_wrth) <- "interval"
  missing.values(trust_wrth) <- c(NA)
  annotation(trust_wrth)["Source"] <- "European Value Survey 2008"
  annotation(trust_wrth)["Remark"] <- "The index was calculated from 7 variables regarding the propensity to commit certain minor offenses for egoistic benefit. All were coded after the same 10-scale, that was switched. This procedure was based on research literature, refer to the theoretical paper for more detailed legitimation. The index variables have a high Cronbach's alpha of 0.84."

  description(sex) <- "Individual level: Sex of the respondent"
  measurement(sex) <- "nominal"
  missing.values(sex) <- c(NA)
  annotation(sex)["Source"] <- "European Value Survey 2008"
  annotation(sex)["Remark"] <- "Item was recoded as nominal factors."
  
  description(age) <- "Individual level: Age of the respondent"
  measurement(age) <- "ratio"
  missing.values(age) <- c(NA)
  annotation(age)["Source"] <- "European Value Survey 2008"
  annotation(age)["Remark"] <- "Item was coded based on the original variable of the respondent's year of birth, subtracted from 2008, the year of surveying."
  
  description(mar_stat) <- "Individual level: Marital status of the respondent"
  wording(mar_stat) <- "What is your current legal marital status?"
  measurement(mar_stat) <- "nominal"
  missing.values(mar_stat) <- c(NA)
  annotation(mar_stat)["Source"] <- "European Value Survey 2008()"
  annotation(mar_stat)["Remark"] <- "Item was recoded as nominal factors."
  
  description(edu_cat) <- "Individual level: Educational categories for respondent"
  wording(edu_cat) <- "What is the highest level you have completed in your education?"
  measurement(edu_cat) <- "ordinal"
  missing.values(edu_cat) <- c(NA)
  annotation(edu_cat)["Source"] <- "European Value Survey 2008"
  annotation(edu_cat)["Remark"] <- "The item was asked based on the 'ISCED-one digit code'(6-scale)."
  
  description(edu) <- "Individual level: Education of respondent"
  wording(edu) <- "What is the highest level you have completed in your education?"
  measurement(edu) <- "interval"
  missing.values(edu) <- c(NA)
  annotation(edu)["Source"] <- "European Value Survey 2008"
  annotation(edu)["Remark"] <- "The item was asked based on the 'ISCED-two digit code' (13-scale). The assumption of quasi-metric variable was taken when recoding to numeric."
  
  description(town) <- "Individual level: Size of town where interview was conducted"
  measurement(town) <- "interval"
  missing.values(town) <- c(NA)
  annotation(town)["Source"] <- "European Value Survey 2008"
  annotation(town)["Remark"] <- "The assumption of quasi-metric variable was taken when recoding to numeric."
  
})

codebook(EVS_book)
description(EVS_book)
Write(codebook(EVS_book), file = "EVS_final_cdbk.txt")

?Write
