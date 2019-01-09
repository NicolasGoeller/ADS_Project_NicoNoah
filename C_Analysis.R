### In this file, we create analyses for our project

library(haven)
library(tidyverse)
library(lme4)
library(sjstats)
library(lmerTest)
library(sjPlot)

#---------------------------------------------------------------------------------------

#Read in dataset from B_Variable Preparation
EVS_final <- read_rds("Data/EVS_final.rds")

#Subsetting data and omitting NAs

#----------------------------------------------------------------------------------------

##Descriptive analysis

#----------------------------------------------------------------------------------------

##  1. Individual model (Level 1)

m1

#---------------------------------------------------------------------------------------------

##  2. Regional models (Level 2)

#Null model for regions
m2 <- lmer(sat ~ 1 + (1|reg), EVS_final)
summary(m2)
icc(m2)

#Model with Regional and Individual predictors
m3

#---------------------------------------------------------------------------------------------

##  3. National models (Level 3)

#Null model for nations
m4 <- lmer(sat ~ 1 + (1|nation), EVS_final)
summary(m4)
icc(m4)

#Model with National and Individual predictors
m5

#-----------------------------------------------------------------------------------------

##  4. Intergrated models (Level 2 & 3)

#Null model for both regions and nations
m6 <- lmer(sat ~ 1 + (1|reg) + (1|nation), EVS_final)
summary(m6)
icc(m6)

#Model with predictors from all levels
m7



#---------------------------------------------------------------------------------------------------------------------------
### Correlation analysis

## 1. Life satisfaction and happiness 
cor(EVS$sat, EVS$happ) # doesn't work 


#-------------------------------------------------------------------------------------------------------------------------------------------------
### Individual-level regressions

## 1. Explaining perception of income equality with own monthly income
reg_1 <- lm(inc_eq ~ incppp_mon, data = EVS)
summary(reg_1)
