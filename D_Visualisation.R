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

#     3.1.2 Regions and Perception of fairness
ggplot(data = reg_geodata)+
  geom_sf(aes(fill = fair_reg))+
  labs(fill = "Perception of fairness (from low to high)")+
  ggtitle("Regional Europe Map", 
          subtitle = "NUTS 1 Regions and Percieved  Fairness")+
  coord_sf(xlim = c(-24, 32), ylim = c(33, 71), expand = FALSE)


#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------


###  4. Codebook

EVS_book <- dplyr::select(EVS_final,
                          nation, c_code, reg, eureg, sat, happ,
                          fair, siops,  job_sat, nowork, intp_trust, 
                          conf_church, conf_armed, conf_educ, conf_press, 
                          conf_tu, conf_police, conf_parl, conf_cs, conf_socs, 
                          conf_eu, conf_nato, conf_un, conf_hs, conf_just, conf_gov,
                          inst_trust,
                          cl_sb, ch_tax, joy, lying, bribe, av_tax, av_pub_f,  
                          trust_wrth,
                          work_impo, free_job, talent, duty, work_first, sup, entre,
                          isei, inc_mon, inc_an, incppp_mon, inc_eq,
                          sex, age, mar_stat, edu_cat, edu, town,
                          fhrate, hdi, gini, GDPpc, unemployment, life_sat,
                          intp_trust_reg, inst_trust_reg, trust_wrth_reg, fair_reg)

EVS_book <- as.data.set(EVS_book)
  
EVS_book <- within(EVS_book,{
  
  description(nation) <- "Country of residence"
  measurement(nation) <- "nominal"
  missing.values(nation) <- c(NA)
  annotation(nation)["Source"] <- "European Value Survey 2008 (country)"
  annotation(nation)["Remark"] <- "The Russian Federation was excluded for purposes of visualisation. Northern Ireland and Northern Cyprus were recoded to belong to their respective legal states. Azerbaijan was excluded as there were no respondents in the sample."
  
  description(c_code) <- "Country Code"
  measurement(c_code) <- "nominal"
  missing.values(c_code) <- c(NA)
  annotation(c_code)["Source"] <- "European Value Survey 2008 (c_abrv)"
  annotation(c_code)["Remark"] <- "Country codes were recoded to match changes in country selection (see 'Country of residence)."
  
  description(reg) <- "Region of residence"
  measurement(reg) <- "nominal"
  missing.values(reg) <- c(NA)
  annotation(reg)["Source"] <- "European Value Survey 2008 (v371b_N1)"
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
  annotation(sat)["Source"] <- "European Value Survey 2008 (v66)"
  annotation(sat)["Remark"] <- "The assumption of quasi-metric variable was taken when recoding to numeric."
  
  description(happ) <- "Individual level: Happiness"
  wording(happ) <- "Taking all things together, how happy would you say you are? (1-very happy, 4-not at all happy)"
  measurement(happ) <- "interval"
  missing.values(happ) <- c(NA)
  annotation(happ)["Source"] <- "European Value Survey 2008 (v8)"
  annotation(happ)["Remark"] <- "The assumption of quasi-metric variable was taken when recoding to numeric."
  
  description(fair) <- "Individual level: Fairness"
  wording(fair) <- "Do you think that most people would try to take advantage of you if they got the chance, or would they try to be fair? (1-try to take advantage, 10-try to be fair)"
  measurement(fair) <- "interval"
  missing.values(fair) <- c(NA)
  annotation(fair)["Source"] <- "European Value Survey 2008 (v63)"
  annotation(fair)["Remark"] <- "The assumption of quasi-metric variable was taken when recoding to numeric."
  
  description(siops) <- "Individual level: SIOPS-Index"
  wording(siops) <- "Standard Index of Occupational Prestige Scala (Values: 0-100)"
  measurement(siops) <- "interval"
  missing.values(siops) <- c(NA)
  annotation(siops)["Source"] <- "European Value Survey 2008 (v339SIOPS)"
  annotation(siops)["Remark"] <- "The SIOPS-Index is  a widely known measure for social prestige of different occupations."
    
  description(job_sat) <- "Individual level: Job satisfaction"
  wording(job_sat) <- "Overall, how satisfied or dissatisfied are you with your job? (1-dissatisfied, 10-satisfied)"
  measurement(job_sat) <- "interval"
  missing.values(job_sat) <- c(NA)
  annotation(job_sat)["Source"] <- "European Value Survey 2008 (v90)"
  annotation(job_sat)["Remark"] <- "The assumption of quasi-metric variable was taken when recoding to numeric."
  
  description(nowork) <- "Individual level: Employment"
  wording(nowork) <- "Are you yourself employed or not? (0-Employed, 1-Unemployed)"
  measurement(nowork) <- "nominal"
  missing.values(nowork) <- c(NA)
  annotation(nowork)["Source"] <- "European Value Survey 2008 (v89)"
  annotation(nowork)["Remark"] <- "Item was recoded as factor variable."
  
  description(intp_trust) <- "Individual level: Interpersonal trust"
  wording(intp_trust) <- "Generally speaking, would you say that most people can be trusted or that you can’t be too careful in dealing with people? (0-Distrustful, 1-Trustful)"
  measurement(intp_trust) <- "nominal"
  missing.values(intp_trust) <- c(NA)
  annotation(intp_trust)["Source"] <- "European Value Survey 2008 (v62)"
  annotation(intp_trust)["Remark"] <- "The item was recoded and scales were switched."

  foreach(ind1 = c(conf_church, conf_armed, conf_educ, conf_press, conf_tu, 
                   conf_police, conf_parl, conf_cs, conf_socs, conf_eu, 
                   conf_nato, conf_un, conf_hs, conf_just, conf_gov),{
    description(ind1) <- "variable for index: Trust in institutions"
    measurement(ind1) <- "interval"
    missing.values(ind1) <- c(NA)
    annotation(ind1)["Remark"] <- "The assumption of quasi-metric variable was taken when recoding to numeric. The item was recoded and scales were switched."
  })
  
  wording(conf_church) <- "How much confidence you have in the church? (1-none at all, 4-a great deal)"
  annotation(conf_church)["Source"] <- "European Value Survey 2008 (v205)"
  
  wording(conf_armed) <- "How much confidence you have in the armed forces? (1-none at all, 4-a great deal)"
  annotation(conf_armed)["Source"] <- "European Value Survey 2008 (v206)"
  
  wording(conf_educ) <- "How much confidence you have in ? (1-none at all, 4-a great deal)"
  annotation(conf_educ)["Source"] <- "European Value Survey 2008 (v207)"
  
  wording(conf_press) <- "How much confidence you have in the press? (1-none at all, 4-a great deal)"
  annotation(conf_press)["Source"] <- "European Value Survey 2008 (v208)"
  
  wording(conf_tu) <- "How much confidence you have in trade unsions? (1-none at all, 4-a great deal)"
  annotation(conf_tu)["Source"] <- "European Value Survey 2008 (v209)"
  
  wording(conf_police) <- "How much confidence you have in the police? (1-none at all, 4-a great deal)"
  annotation(conf_police)["Source"] <- "European Value Survey 2008 (v210)"
  
  wording(conf_parl) <- "How much confidence you have in the parliament? (1-none at all, 4-a great deal)"
  annotation(conf_parl)["Source"] <- "European Value Survey 2008 (v211)"
  
  wording(conf_cs) <- "How much confidence you have in the civil service? (1-none at all, 4-a great deal)"
  annotation(conf_cs)["Source"] <- "European Value Survey 2008 (v212)"
  
  wording(conf_socs) <- "How much confidence you have in the social security system? (1-none at all, 4-a great deal)"
  annotation(conf_socs)["Source"] <- "European Value Survey 2008 (v213)"
  
  wording(conf_eu) <- "How much confidence you have in the EU? (1-none at all, 4-a great deal)"
  annotation(conf_eu)["Source"] <- "European Value Survey 2008 (v214)"
  
  wording(conf_nato) <- "How much confidence you have in NATO? (1-none at all, 4-a great deal)"
  annotation(conf_nato)["Source"] <- "European Value Survey 2008 (v215)"
  
  wording(conf_un) <- "How much confidence you have in the United Nations? (1-none at all, 4-a great deal)"
  annotation(conf_un)["Source"] <- "European Value Survey 2008 (v216)"
  
  wording(conf_hs) <- "How much confidence you have in the health care system? (1-none at all, 4-a great deal)"
  annotation(conf_hs)["Source"] <- "European Value Survey 2008 (v217)"
  
  wording(conf_just) <- "How much confidence you have in the justice system? (1-none at all, 4-a great deal)"
  annotation(conf_just)["Source"] <- "European Value Survey 2008 (v218)"
  
  wording(conf_gov) <- "How much confidence you have in the government? (1-none at all, 4-a great deal)"
  annotation(conf_gov)["Source"] <- "European Value Survey 2008 (v222)"
  
  description(inst_trust) <- "Individual level: Index for institutional trust"
  measurement(inst_trust) <- "interval"
  missing.values(inst_trust) <- c(NA)
  annotation(inst_trust)["Source"] <- "European Value Survey 2008"
  annotation(inst_trust)["Remark"] <- "The index was calculated from 15 variables regarding trust in different institutions. All were coded after the same 4-scale. This procedure was based on research literature, refer to the theoretical paper for more detailed legitimation. The index variables have a high Cronbach's alpha of 0.89."
  
  foreach(ind2 = c(cl_sb, ch_tax, joy, lying, bribe, av_tax, av_pub_f),{
                     description(ind2) <- "variable for index: Moral trustworthiness"
                     measurement(ind2) <- "interval"
                     missing.values(ind2) <- c(NA)
                     annotation(ind2)["Remark"] <- "The assumption of quasi-metric variable was taken when recoding to numeric. The item was recoded and scales were switched."
                   })
  
  wording(cl_sb) <- "Please tell me for non-entitled state benefits whether you think it can always be justified, never be justified, or something in between? (1-always, 4-never)"
  annotation(cl_sb)["Source"] <- "European Value Survey 2008 (v233)"
  
  wording(ch_tax) <- "Please tell me for cheating from tax if possible whether you think it can always be justified, never be justified, or something in between? (1-always, 4-never)"
  annotation(ch_tax)["Source"] <- "European Value Survey 2008 (v234)"
  
  wording(joy) <- "Please tell me for joyriding whether you think it can always be justified, never be justified, or something in between? (1-always, 4-never)"
  annotation(joy)["Source"] <- "European Value Survey 2008 (v235)"
  
  wording(lying) <- "Please tell me for lying in your own interest whether you think it can always be justified, never be justified, or something in between? (1-always, 4-never)"
  annotation(lying)["Source"] <- "European Value Survey 2008 (v237)"
  
  wording(bribe) <- "Please tell me for accepting a bribe in course of duty whether you think it can always be justified, never be justified, or something in between? (1-always, 4-never)"
  annotation(bribe)["Source"] <- "European Value Survey 2008 (v239)"
  
  wording(av_tax) <- "Please tell me for paying cash to avoid taxes whether you think it can always be justified, never be justified, or something in between? (1-always, 4-never)"
  annotation(av_tax)["Source"] <- "European Value Survey 2008 (v245)"
  
  wording(av_pub_f) <- "Please tell me for avoiding the public transfer fare whether you think it can always be justified, never be justified, or something in between? (1-always, 4-never)"
  annotation(av_pub_f)["Source"] <- "European Value Survey 2008 (v247)"
  
  description(trust_wrth) <- "Individual level: Index for moral trustworthiness"
  measurement(trust_wrth) <- "interval"
  missing.values(trust_wrth) <- c(NA)
  annotation(trust_wrth)["Source"] <- "European Value Survey 2008"
  annotation(trust_wrth)["Remark"] <- "The index was calculated from 7 variables regarding the propensity to commit certain minor offenses for egoistic benefit. All were coded after the same 10-scale, that was switched. This procedure was based on research literature, refer to the theoretical paper for more detailed legitimation. The index variables have a high Cronbach's alpha of 0.82."

  description(work_impo) <- "Individual level: Work importance in life"
  wording(work_impo) <- "Please indicate, how important is work in your life (1-very, 4-not at all)"
  measurement(work_impo) <- "interval"
  missing.values(work_impo) <- c(NA)
  annotation(work_impo)["Source"] <- "European Value Survey 2008 (v1)"
  annotation(work_impo)["Remark"] <- "The assumption of quasi-metric variable was taken when recoding to numeric."
  
  description(free_job) <- "Individual level: Freedem of job decisions"
  wording(free_job) <- "How free are you to make decisions in your job? (1-not at all, 10-a great deal)"
  measurement(free_job) <- "interval"
  missing.values(free_job) <- c(NA)
  annotation(free_job)["Source"] <- "European Value Survey 2008 (v91)"
  annotation(free_job)["Remark"] <- "The assumption of quasi-metric variable was taken when recoding to numeric."
  
  description(talent) <- "Individual level: Job development"
  wording(talent) <- "Do you agree or disagree with: To fully develop your talents, you need to have a job. (1-disagree strongly, 5-agree strongly)"
  measurement(talent) <- "interval"
  missing.values(talent) <- c(NA)
  annotation(talent)["Source"] <- "European Value Survey 2008 (v92)"
  annotation(talent)["Remark"] <- "The assumption of quasi-metric variable was taken when recoding to numeric. From the original survey, the scales were switched."
  
  description(duty) <- "Individual level: Work duty to society"
  wording(duty) <- "Do you agree or disagree with: Work is a duty towards society. (1-disagree strongly, 5-agree strongly)"
  measurement(duty) <- "interval"
  missing.values(duty) <- c(NA)
  annotation(duty)["Source"] <- "European Value Survey 2008 (v95)"
  annotation(duty)["Remark"] <- "The assumption of quasi-metric variable was taken when recoding to numeric. From the original survey, the scales were switched."
  
  description(work_first) <- "Individual level: Work should come always first"
  wording(work_first) <- "Do you agree or disagree with: Work should always come first, even if it means less spare time.(1-disagree strongly, 5-agree strongly)"
  measurement(work_first) <- "interval"
  missing.values(work_first) <- c(NA)
  annotation(work_first)["Source"] <- "European Value Survey 2008 (v96)"
  annotation(work_first)["Remark"] <- "The assumption of quasi-metric variable was taken when recoding to numeric. From the original survey, the scales were switched."
  
  description(sup) <- "Individual level: Supervisor work"
  wording(sup) <- "Do/did you have any responsibility for supervising the work of other employees? "
  measurement(sup) <- "nominal"
  missing.values(sup) <- c(NA)
  annotation(sup)["Source"] <- "European Value Survey 2008 (v341)"
  annotation(sup)["Remark"] <- "Item was recoded as nominal factors."
  
  description(entre) <- "Individual level: Entrepreneurial work"
  wording(entre) <- "In your LAST job were you employed (either full time or part time) or were you self-employed?"
  measurement(entre) <- "nominal"
  missing.values(entre) <- c(NA)
  annotation(entre)["Source"] <- "European Value Survey 2008 (v338)"
  annotation(entre)["Remark"] <- "Item was recoded as nominal factors."
  
  description(isei) <- "Individual level: ISEI-Index"
  wording(isei) <- "International Socio-Economic Index of Occupational Status (Values: 16-90)"
  measurement(isei) <- "interval"
  missing.values(isei) <- c(NA)
  annotation(isei)["Source"] <- "European Value Survey 2008 (v339ISEI)"
  annotation(isei)["Remark"] <- "The ISEI-Index is  a widely known measure for socio-economic status of different occupations."
  
  description(inc_mon) <- "Individual level: Monthly income"
  wording(inc_mon) <- "Here is a list of incomes and we would like to know in what group your household is, counting all wages, salaries, pensions and other incomes that come in. Just give the letter of the group your household falls into, after taxes and other deductions. (12 categories)"
  measurement(inc_mon) <- "interval"
  missing.values(inc_mon) <- c(NA)
  annotation(inc_mon)["Source"] <- "European Value Survey 2008 (v353MM)"
  annotation(inc_mon)["Remark"] <- "The assumption of quasi-metric variable was taken when recoding to numeric."
  
  description(inc_an) <- "Individual level: Annual income"
  wording(inc_an) <- "Here is a list of incomes and we would like to know in what group your household is, counting all wages, salaries, pensions and other incomes that come in. Just give the letter of the group your household falls into, after taxes and other deductions. (12 categories)"
  measurement(inc_an) <- "interval"
  missing.values(inc_an) <- c(NA)
  annotation(inc_an)["Source"] <- "European Value Survey 2008 (v353YR)"
  annotation(inc_an)["Remark"] <- "The assumption of quasi-metric variable was taken when recoding to numeric."
  
  description(incppp_mon) <- "Individual level: Monthly household income (x1000), corrected for purchasing power parity in €"
  measurement(incppp_mon) <- "interval"
  missing.values(incppp_mon) <- c(NA)
  annotation(incppp_mon)["Source"] <- "European Value Survey 2008 (v353M_ppp)"
  
  description(inc_eq) <- "Individual level: Equality of income incentives"
  wording(inc_eq) <- "How would you place your views on this scale regarding equality of income incentives for individual effort?"
  measurement(inc_eq) <- "interval"
  missing.values(inc_eq) <- c(NA)
  annotation(inc_eq)["Source"] <- "European Value Survey 2008 (v198)"
  annotation(inc_eq)["Remark"] <- "The assumption of quasi-metric variable was taken when recoding to numeric."
  
  description(sex) <- "Individual level: Sex of the respondent"
  measurement(sex) <- "nominal"
  missing.values(sex) <- c(NA)
  annotation(sex)["Source"] <- "European Value Survey 2008 (v302)"
  annotation(sex)["Remark"] <- "Item was recoded as nominal factors."
  
  description(age) <- "Individual level: Age of the respondent"
  measurement(age) <- "ratio"
  missing.values(age) <- c(NA)
  annotation(age)["Source"] <- "European Value Survey 2008 (v303)"
  annotation(age)["Remark"] <- "Item was coded based on the original variable of the respondent's year of birth, subtracted from 2008, the year of surveying."
  
  description(mar_stat) <- "Individual level: Marital status of the respondent"
  wording(mar_stat) <- "What is your current legal marital status?"
  measurement(mar_stat) <- "nominal"
  missing.values(mar_stat) <- c(NA)
  annotation(mar_stat)["Source"] <- "European Value Survey 2008 (v313)"
  annotation(mar_stat)["Remark"] <- "Item was recoded as nominal factors."
  
  description(edu_cat) <- "Individual level: Educational categories for respondent"
  wording(edu_cat) <- "What is the highest level you have completed in your education?"
  measurement(edu_cat) <- "ordinal"
  missing.values(edu_cat) <- c(NA)
  annotation(edu_cat)["Source"] <- "European Value Survey 2008 (v336)"
  annotation(edu_cat)["Remark"] <- "The item was asked based on the 'ISCED-one digit code'(6-scale)."
  
  description(edu) <- "Individual level: Education of respondent"
  wording(edu) <- "What is the highest level you have completed in your education?"
  measurement(edu) <- "interval"
  missing.values(edu) <- c(NA)
  annotation(edu)["Source"] <- "European Value Survey 2008 (v336_2)"
  annotation(edu)["Remark"] <- "The item was asked based on the 'ISCED-two digit code' (13-scale). The assumption of quasi-metric variable was taken when recoding to numeric."
  
  description(town) <- "Individual level: Size of town where interview was conducted"
  measurement(town) <- "interval"
  missing.values(town) <- c(NA)
  annotation(town)["Source"] <- "European Value Survey 2008 (v370a)"
  annotation(town)["Remark"] <- "The assumption of quasi-metric variable was taken when recoding to numeric."
  
  description(fhrate) <- "National level: Freedom House Democracy rating for 2008"
  measurement(fhrate) <- "interval"
  missing.values(fhrate) <- c(NA)
  annotation(fhrate)["Source"] <- "Democracy Cross National Data by Pippa Norris (fhrate08)"
  annotation(fhrate)["Remark"] <- "The assumption of quasi-metric variable was taken when recoding to numeric. Missing values for Kosovo and Montenegro were replace with data for 2008 from the Freedom House website."
  
  description(hdi) <- "National level: Human Development Index for 2008"
  measurement(hdi) <- "interval"
  missing.values(hdi) <- c(NA)
  annotation(hdi)["Source"] <- "Democracy Cross National Data by Pippa Norris (UNDP_HDI2008)"
  annotation(hdi)["Remark"] <- "Kosovo has no HDI value for 2008, so the value of Serbia was taken as a proxy."
  
  description(GDPpc) <- "National level: Gross Domestic Product per capita for 2008 in 2018-Dollars per inhabitant"
  measurement(GDPpc) <- "ratio"
  missing.values(GDPpc) <- c(NA)
  annotation(GDPpc)["Source"] <- "World Bank Data"
  annotation(GDPpc)["Remark"] <- "-"
  
  description(gini) <- "National level: Gini-Coeffiecient for 2008"
  measurement(gini) <- "ratio"
  missing.values(gini) <- c(NA)
  annotation(gini)["Source"] <- "World Bank Data"
  annotation(gini)["Remark"] <- "Missing values for Germany and Poland were taken from the Eurostat viewer, for Serbia from the cIA World Factbook, all three regarding the year 2008. The missing values for Croatia, Kosovo and Macedonia were approximated by their values from 2009, for Bosnia Herzegovina the 2007 value was taken as a proxy."
  
  description(unemployment) <- "National level: Unemployment rate for 2008"
  measurement(unemployment) <- "ratio"
  missing.values(unemployment) <- c(NA)
  annotation(unemployment)["Source"] <- "European Value Survey 2008"
  annotation(unemployment)["Remark"] <- "Aggregated variable from individual level responses, no official data."
  
  description(life_sat) <- "National level: Aggregated average life satisfaction 2008"
  measurement(life_sat) <- "ratio"
  missing.values(life_sat) <- c(NA)
  annotation(life_sat)["Source"] <- "European Value Survey 2008"
  annotation(life_sat)["Remark"] <- "-"
  
  description(inst_trust_reg) <- "Regional level level: Aggregated average institutional trust 2008"
  measurement(inst_trust_reg) <- "ratio"
  missing.values(inst_trust_reg) <- c(NA)
  annotation(inst_trust_reg)["Source"] <- "European Value Survey 2008"
  annotation(inst_trust_reg)["Remark"] <- "-"
  
  description(intp_trust_reg) <- "Regional level: Aggregated average interpersonal trust"
  measurement(intp_trust_reg) <- "ratio"
  missing.values(intp_trust_reg) <- c(NA)
  annotation(intp_trust_reg)["Source"] <- "European Value Survey 2008"
  annotation(intp_trust_reg)["Remark"] <- "-"
  
  description(trust_wrth_reg) <- "Regional level: Aggregated average moral trustworthiness"
  measurement(trust_wrth_reg) <- "ratio"
  missing.values(trust_wrth_reg) <- c(NA)
  annotation(trust_wrth_reg)["Source"] <- "European Value Survey 2008"
  annotation(trust_wrth_reg)["Remark"] <- "-"
  
  description(fair_reg) <- "Regional level: Aggregated average expected fairness"
  measurement(fair_reg) <- "ratio"
  missing.values(fair_reg) <- c(NA)
  annotation(fair_reg)["Source"] <- "European Value Survey 2008"
  annotation(fair_reg)["Remark"] <- "-"
})

codebook(EVS_book)

Write(codebook(EVS_book), file = "EVS_final_cdbk.txt")
#Write(codebook(EVS_book), file = "LaTex/EVS_final_cdbk.tex")

