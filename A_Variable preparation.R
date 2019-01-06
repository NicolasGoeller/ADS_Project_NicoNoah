###
# This script provides basic preparation of variables used
# for further analyses. 

library(haven)
library(tidyverse)
library(magrittr)

EVS_new <- read_spss("Data/ZA4800_v4-0-0.sav") # Seems to have the missing countries

#EVS_Long <- read_spss("Data/EVS_Long.sav") #Einlesen der EVS 1981-2008 aus .sav-Format
#EVS_2008 <- filter(EVS_Long, S002EVS == 4) #Auswählen der 4.Welle Jahr 2008
#write_rds(EVS_2008, "Data/EVS_2008.rds")

PN <- read_spss("Data/Democracy Cross-National Data V4.1 09092015.sav")

EVS_2008 <- read_rds("Data/EVS_2008.rds")

EVS <- select(EVS_2008, #Pre-selection of variables
              S003, A170, S009, A008, X048A, X048B, #base
              A165, A168_01, A168A, C029, X036A, #non-pecuniary
              C033, X047B, X047C, X047D, X036B, #pecuniary
              X001, X002, X007, X025A, x049a, X028, X035_2) #demographics

EVS %<>% within({ #base variables
  sat <- A170 #Life satisfaction (10 point)
  sat[A170 %in% c(-5, -4, -3, -2, -1)] <- NA
  sat <- as.numeric(sat)
  
  happ <- A008 #Feeling of happiness (4 point)
  happ[A008 %in% c(-5, -4, -3, -2, -1)] <- NA
  happ <- as.numeric(happ)
  
  cntry <- S003 #Country
  cntry[S003 %in% c(-5, -4, -3, -2, -1)] <- NA
  cntry <- as_factor(cntry, ordered = F)
  
  cntry_code <- S009 #Country code abbreviation
  
  state <- X048A #Region on federal state level
  state <- as_factor(state, ordered = F)
  
  reg <- X048B #region on regional level
  reg <- as_factor(reg, ordered = F)
}) #base variables

EVS %<>% within({ #non-pecuniary factors
  trst_d <- A165 #Dummy: Do you think you can trust other people
  trst_d[A165 %in% c(-5, -4, -3, -2, -1)] <- NA
  
  help <- A168_01 #Most people are helpful or look out for themselves (10 point)
  help[A168_01 %in% c(-5, -4, -3, -2, -1)] <- NA
  help <- as.numeric(help)
  
  fair <- A168A #Will people be fair or make their advantage with you (10 point)
  fair[A168A %in% c(-5, -4, -3, -2, -1)] <- NA
  fair <- as.numeric(fair)
  
  job_sat <- C033 #Job satisfaction (10 point)
  job_sat[C033 %in% c(-5, -4, -3, -2, -1)] <- NA
  job_sat <- as.numeric(job_sat)
  
  siops <- X036A #Standard Index of Occupational Prestige Scala (1-100)
  siops <- as.numeric(siops)
}) #non-pecuniary factors

EVS %<>% within({ #pecuniary factors 
  work <- C029 #Unemployed/ employed
  work[C029 %in% c(-5, -4, -3, -2, -1)] <- NA
  
  inc_an <- X047C #Annual income in euros
  inc_an[C033 %in% c(-5, -4, -3, -2, -1)] <- NA
  inc_an <- as.numeric(inc_an)
  
  inc_mon <- X047B #Monthly income in euros
  inc_mon[C033 %in% c(-5, -4, -3, -2, -1)] <- NA
  inc_mon <- as.numeric(inc_mon)
  
  incppp_mon <- X047D #Monthly income after purchasing power parity
  incppp_mon[X047D %in% c(-5, -4, -3, -2, -1)] <- NA
  incppp_mon <- as.numeric(incppp_mon)
  
  isei <- X036B #International Socio-Economic Index of Occupational Status
  isei <- as.numeric(isei)
}) #pecuniary factors

EVS %<>% within({ #demographics 
  sex <- X001 #Sex
  sex[X001 %in% c(-5, -4, -3, -2, -1)] <- NA
  
  age <- X002 #Age recoded from birth year
  age[X002 %in% c(-5, -4, -3, -2, -1)] <- NA
  age <- as.numeric(age)
  age <- 2008 - age
  
  mar_stat <- X007 #Current marital status
  mar_stat[X007 %in% c(-5, -4, -3, -2, -1)] <- NA
  mar_stat <- as_factor(mar_stat, ordered = F)
  
  edu <- X025A #Education after ISCED code (6 point)
  edu[X025A %in% c(-5, -4, -3, -2, -1)] <- NA
  edu <- as.numeric(edu)
  
  town <- x049a #size of town
  town[x049a %in% c(-5, -4, -3, -2, -1)] <- NA
  town <- as.numeric(town)
  
  employ <- X028
  employ[X028 %in% c(-5, -4, -3, -2, -1)] <- NA
  employ <- as_factor(employ, ordered = F)
  
  prof <- X035_2 #Job/ profession/ industry
  prof[X035_2 %in% c(-5, -4, -3, -2, -1)] <- NA
  prof <- as_factor(prof, ordered = F)
}) #demographics

EVS %<>% na.omit()



#Work in progress on cntry variable
#cntry[S003 %in% c(124, 352, 578, 840)] <- NA

#124 Canada  
#352 Iceland
#578 Norway
#792 Turkey
#840 USA
# Turkey, Iceland & Norway can be found in the online-Catalogue,
#    but not in the data

  #c(8, 40, 51, 56, 70, 100, 112, 124, 191, 196, 197, 203, 208, 233, 246, 250,
   # 268, 276, 300, 348, 352, 372, 380, 428, 440, 442, 470, 498, 499, 528, 578,
   #616, 629, 642, 643, 688, 703, 705, 724, 752, 756, 792, 804, 807, 826, 840,
   #909, 915)
  #c("Albania", "Austria", "Armenia", "Belgium", "Bosnia Herzegovina", 
    #"Bulgaria", "Belarus", "Canada", "Croatia", "Cyprus", "Northern Cyprus",
    #"Czech Republic", "Denmark", "Estonia", "Finland", "France", "Georgia",
    #"Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Latvia", 
    #"Lithuania", "Luxembourg", "Malta", "Moldova", "Montenegro", "Netherlands", 
    #"Norway", "Poland", "Portugal", "Romania", "Russian Federation", "Serbia", 
    #"Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", 
    #"Ukraine", "Macedonia", "Great Britain", "USA", "Northern Ireland", "Kosovo")