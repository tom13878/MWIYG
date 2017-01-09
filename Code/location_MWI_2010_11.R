# -------------------------------------
# Location variables MWI
# -------------------------------------

# load packages
library(dplyr)
library(haven)
library(tidyr)

# set working directory
if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/MWI/2010_11/Data"
} else {
  dataPath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/MWI/2010_11/Data"
}


location_2010_11 <- read_dta(file.path(dataPath, "Household/HH_MOD_A_FILT.dta")) %>%
  transmute(HHID, case_id, ea_id, region=NA, district = hh_a01)

# there are three regions in Malawi:
# 2: Central, 1: Northern and 3: Southern. The first 
# number of the district code tels us which
# is which

location_2010_11$region <- with(location_2010_11,
                             ifelse(district < 200, "NORTHERN",
                                    ifelse(district >=200 & district < 300, "CENTRAL",
                                           "SOUTHERN")))

