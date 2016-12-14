# -------------------------------------
#' Plot areas Malawi wave 2 (2013)
# ------------------------------------- 

# load packages
library(dplyr)
library(haven)
library(tidyr)

# set working directory
if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/MWI/2013/Data"
} else {
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/MWI/2013/Data"
}

# area measurements recorded in Module B_1 of the
# wave 2 questionnaire. Some plots were not measured
# but only 8%.

areas13 <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_B1.dta")) %>%  
  select(y2_hhid, plotnum=ag_b100a,
         area_farmer=ag_b104a, areas_farmer_unit = ag_b104b,
         area_gps=ag_b104c)

# some self reported measurements are in acres and m2
# change to hectares
areas13$area_farmer <- ifelse(areas13$areas_farmer_unit %in% 1,
                            areas13$area_farmer * 0.404685642,
                            ifelse(areas13$areas_farmer_unit %in% 3,
                                   areas13$area_farmer * 0.0001, NA))

areas13$areas_farmer_unit <- NULL

rm(dataPath)