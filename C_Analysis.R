### In this file, we create analyses for our project

library(haven)
library(tidyverse)
library(lme4)
library(sjstats)
install.packages("lmerTest")
install.packages("sjPlot")
library(lmerTest)
library(sjPlot)

#---------------------------------------------------------------------------------------

#Read in dataset from B_Variable Preparation
EVS_final <- read_rds("Data/EVS_final.rds")

#----------------------------------------------------------------------------------------

## Null models for later multi-level analysis

#Country-level (level 3)
m01 <- lmer(sat ~ 1 + (1|nation), EVS_final)
summary(m01)
icc(m01)

#Region-level (level 2)
m02 <- lmer(sat ~ 1 + (1|reg), EVS_final)
summary(m02)
icc(m02)

#Intergrated analysis
m03 <- lmer(sat ~ 1 + (1|reg) + (1|nation), EVS_final)
summary(m03)
icc(m03)



#---------------------------------------------------------------------------------------------------------------------------
### Correlation analysis

## 1. Life satisfaction and happiness 
cor(EVS$sat, EVS$happ) # doesn't work 


#-------------------------------------------------------------------------------------------------------------------------------------------------
### Individual-level regressions

## 1. Explaining perception of income equality with own monthly income
reg_1 <- lm(inc_eq ~ incppp_mon, data = EVS)
summary(reg_1)
