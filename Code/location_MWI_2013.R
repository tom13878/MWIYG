# -------------------------------------
# Location variables MWI
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


location_2013 <- read_dta(file.path(dataPath, "Household/HH_MOD_A_FILT.dta")) %>%
  select(y2_hhid, HHID, case_id, ea_id, region, district)
location_2013$region <- with(location_2013,
                          ifelse(region == 1, "NORTHERN",
                                 ifelse(region == 2, "CENTRAL",
                                        "SOUTHERN")))

rm(dataPath)
