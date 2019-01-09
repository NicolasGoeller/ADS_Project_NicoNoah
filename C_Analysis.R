### Nil models for later multi-level analysis 

library(lme4)
library(sjstats)
install.packages("lmertest")
install.packages("sjPlot")
library(lmertest)
library(sjPlot)

#Read in dataset
EVS_final <- read_rds("Data/EVS_final.rds")


m01 <- lmer(sat ~ 1 + (1|cntry), EVS)
summary(m01)
icc(m01)

m02 <- lmer(sat ~ 1 + (1|state), EVS)
summary(m02)
icc(m02)

m03 <- lmer(sat ~ 1 + (1|reg), EVS)
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
