### In this file, we create analyses for our project

#install.packages(c("tidyverse", "lme4", "lmerTest", "sjPlot"))

library(tidyverse)
library(lme4)
library(sjstats)
library(lmerTest)
library(sjPlot)

#---------------------------------------------------------------------------------------

#Read in dataset from B_Variable Preparation
EVS_final <- read_rds("Data/EVS_final.rds")

#Subsetting data and omitting NAs
ana_dat <- select(EVS_final, nation, reg, sat, happ, job_sat, siops, isei, sup, entre, 
                  inc)

#----------------------------------------------------------------------------------------

##Descriptive analysis

#----------------------------------------------------------------------------------------

##  1. Individual model (Level 1)

m1 <- lm(sat ~ job_sat + siops + isei, data = ana_dat)
summary(m1)

#---------------------------------------------------------------------------------------------

##  2. Regional models (Level 2)

#Null model for regions
m2 <- lmer(sat ~ 1 + (1|reg), data = ana_dat)
summary(m2)
icc(m2)

#Model with Regional and Individual predictors
m3 <- lmer(sat ~ 1 + (1|reg), data = ana_dat)
summary(m3)

#---------------------------------------------------------------------------------------------

##  3. National models (Level 3)

#Null model for nations
m4 <- lmer(sat ~ 1 + (1|nation), data = ana_dat)
summary(m4)
icc(m4)

#Model with National and Individual predictors
m5 <- lmer(sat ~ 1 + (1|nation), data = ana_dat)
summary(m5)

#-----------------------------------------------------------------------------------------

##  4. Intergrated models (Level 2 & 3)

#Null model for both regions and nations
m6 <- lmer(sat ~ 1 + (1|reg) + (1|nation), data = ana_dat)
summary(m6)
icc(m6)

#Model with predictors from all levels
m7 <- lmer(sat ~ 1 + (1|reg) + (1|nation), data = ana_dat)
summary(m7)

#---------------------------------------------------------------------------------------------------------------------------
