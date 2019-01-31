### In this file, we create analyses for our project

#install.packages(c("tidyverse", "lme4", "lmerTest", "sjPlot"))

library(tidyverse)
library(lme4)
library(sjstats)
library(lmerTest)
library(stargazer)


#---------------------------------------------------------------------------------------

#Read in dataset from B_Variable Preparation
EVS_final <- read_rds("Data/EVS_final.rds")


#----------------------------------------------------------------------------------------

##  1. Individual model (Level 1)

m1 <- lm(sat ~ job_sat, data = EVS_final)
summary(m1)

stargazer(m1,  #regression models 
          type = "latex", # character vector (eg. "text" / "html" / "latex")
          title = "Linear regression model",  # header
          style = "default",  # style (choice see below)
          summary = NULL,  # logical vector: output summary statistics when given data.frame
          out = "LaTex/table1.tex", # path and output of file
          out.header = FALSE, # logical vector: should output file contain code-header?
          column.labels = c("Basic model"), # column labels for mod1/mod2
          column.separate = c(1,1),  # how column labels should be assigned (label over sev. columns possible)
          covariate.labels = c("Job satisfaction",
                               "Constant"),
          dep.var.caption = "Dependent Variable", # Caption (Top) of dependent variable
          star.cutoffs = c(0.05,0.01,0.001),
          dep.var.labels = c("Life satisfaction"))

#---------------------------------------------------------------------------------------------

##  2. Regional models (Level 2)

#m2 <- lme4::lmer(sat ~ job_sat + + (1|reg), data = ana_dat)
#summary(m2)

#---------------------------------------------------------------------------------------------

##  3. National models (Level 3)

m3 <- lme4::lmer(sat ~ job_sat + GDPpc + (1|nation), data = EVS_final)
summary(m3)

stargazer(m3,  #regression models 
          type = "latex", # character vector (eg. "text" / "html" / "latex")
          title = "Multilevel linear regression model",  # header
          style = "default",  # style (choice see below)
          summary = NULL,  # logical vector: output summary statistics when given data.frame
          out = "LaTex/table2.tex", # path and output of file
          out.header = FALSE, # logical vector: should output file contain code-header?
          column.labels = c("Multilevel model"), # column labels for mod1/mod2
          column.separate = c(1,1),  # how column labels should be assigned (label over sev. columns possible)
          covariate.labels = c("Job satisfaction",
                               "GDP per capita",
                               "Constant"),
          dep.var.caption = "Dependent Variable", # Caption (Top) of dependent variable
          star.cutoffs = c(0.05,0.01,0.001),
          dep.var.labels = c("Life satisfaction"))


#-----------------------------------------------------------------------------------------

##  4. Intergrated models (Level 2 & 3)

#Null model for both regions and nations
#m6 <- lmer(sat ~ 1 + (1|reg) + (1|nation), data = ana_dat)
#summary(m6)
#icc(m6)

#Model with predictors from all levels
#m7 <- lmer(sat ~ 1 + (1|reg) + (1|nation), data = ana_dat)
#summary(m7)

#---------------------------------------------------------------------------------------------------------------------------
