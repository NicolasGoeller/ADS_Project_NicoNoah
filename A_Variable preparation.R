library(haven)
library(tidyverse)
library(magrittr)

#EVS_Long <- read_spss("Data/EVS_Long.sav") #Einlesen der EVS 1981-2008 aus .sav-Format
#EVS_2008 <- filter(EVS_Long, S002EVS == 4) #Auswählen der 4.Welle Jahr 2008
#write_rds(EVS_2008, "Data/EVS_2008.rds")

EVS_2008 <- read_rds("Data/EVS_2008.rds")

EVS <- select(EVS_2008, S003, A170, S009, A008, A165, A168_01, A168A, C029,
              C033, X001, X002, X007, X025A) #Pre-selection of variables
EVS %<>% within({
  sat <- A170 #Life satisfaction (10 point)
  sat[A170 %in% c(-5, -4, -3, -2, -1)] <- NA
  sat <- as.numeric(sat)
  
  happ <- A008 #Feeling of happiness (4 point)
  happ[A008 %in% c(-5, -4, -3, -2, -1)] <- NA
  happ <- as.numeric(happ)
  
  cntry <- S003 #Country
  cntry <- as_factor(cntry, ordered = F)
  
  cntry_code <- S009 #Country code abbreviation
  
  trst_d <- A165 #Dummy: Do you think you can trust other people
  trst_d[A165 %in% c(-5, -4, -3, -2, -1)] <- NA
  
  help <- A168_01 #Most people are helpful or look out for themselves (10 point)
  help[A168_01 %in% c(-5, -4, -3, -2, -1)] <- NA
  help <- as.numeric(help)
  
  fair <- A168A #Will people be fair or make their advantage with you (10 point)
  fair[A168A %in% c(-5, -4, -3, -2, -1)] <- NA
  fair <- as.numeric(fair)
  
  work <- C029 #Unemployed/ employed
  work[C029 %in% c(-5, -4, -3, -2, -1)] <- NA
  
  job_sat <- C033 #Job satisfaction (10 point)
  job_sat[C033 %in% c(-5, -4, -3, -2, -1)] <- NA
  job_sat <- as.numeric(job_sat)
  
  sex <- X001 #Sex
  sex[X001 %in% c(-5, -4, -3, -2, -1)] <- NA
  
  age <- X002 #Age recoded from birth year
  age[X002 %in% c(-5, -4, -3, -2, -1)] <- NA
  age <- as.numeric(age)
  age <- 2008 - age
  
  mar_stat <- X007 #Current marital status
  mar_stat[X007 %in% c(-5, -4, -3, -2, -1)] <- NA
  
  edu <- X025A #Education after ISCED code (6 point)
  edu[X025A %in% c(-5, -4, -3, -2, -1)] <- NA
})


