# -------------------------------------
#' Plot areas Malawi wave 2 (2013)
#'
# ------------------------------------- 

# load packages
library(dplyr)
library(haven)
library(tidyr)

# set working directory
if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/Other/Plot_size"
} else {
  dataPath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Other/Plot_size"
}

# Areas for wave 1 have been imputed by the world bank lsms-isa team
# as some plot did not have a gps measurement.

areas <- read_dta(file.path(dataPath, "areas_mwi_y1_imputed.dta")) %>%  
  select(y2_hhid=case_id, plotnum,
         area_farmer=area_sr, area_gps=area_gps_mi_50)

areas$area_gps <- ifelse(areas$area_gps %in% 0, NA, areas$area_gps)

