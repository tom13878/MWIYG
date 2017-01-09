# -------------------------------------
# community variables wave 1 2010-11
# -------------------------------------

# load packages
library(dplyr)
library(haven)
library(tidyr)

# set working directory
if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/MWI/2013/Data"
} else {
  dataPath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/MWI/2013/Data"
}

# basic population info
comC <- read_dta(file.path(dataPath, "Community/COM_MOD_C.dta")) %>%
  select(ea_id, popEA=com_cc02, HHEA=com_cc03)

# access to services variables
comD <- read_dta(file.path(dataPath, "Community/COM_MOD_D.dta")) %>%
  select(ea_id, road=com_cd01, cost2small_town=com_cd11,
         cost2large_town=com_cd14, bank=com_cd66, micro_finance=com_cd68)

comD$road <- toupper(as_factor(comD$road))
comD$bank <- toupper(as_factor(comD$bank))
comD$micro_finance <- toupper(as_factor(comD$micro_finance))

# agriculture
comF <- read_dta(file.path(dataPath, "Community/COM_MOD_F1.dta")) %>%
  select(ea_id, plant_month=com_cf02, harv_month=com_cf03,
         ext_agent=com_cf07, dist2ext_agent=com_cf08a,
         unit = com_cf08b)

comF$plant_month <- toupper(as_factor(comF$plant_month))
comF$harv_month <- toupper(as_factor(comF$harv_month))
comF$ext_agent <- toupper(as_factor(comF$ext_agent))
comF$unit <- as_factor(comF$unit)
levels(comF$unit) <- c(0.001, 1, 1.609)
comF$unit <- as.numeric(as.character(comF$unit))
comF$dist2ext_agent <- comF$unit * comF$dist2ext_agent
comF$unit <- NULL

# join all community variables
com_2013 <- full_join(comC, comD)
com_2013 <- full_join(com_2013, comF)

# take out trash
rm(comC, comD, comF, dataPath)