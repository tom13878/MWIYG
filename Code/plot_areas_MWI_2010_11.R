# -------------------------------------
#' Plot areas Malawi wave 1 (2010 - 11)
#'
# ------------------------------------- 

# -------------------------------------
#' Plot areas Malawi wave 1 (2010_11)
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
# as some plots did not have a gps measurement. Measurements appear
# to be in hectacres

areas10_11 <- read_dta(file.path(dataPath, "areas_mwi_y1_imputed.dta")) %>%  
  select(y2_hhid=case_id, plotnum,
         area_farmer=area_sr, area_gps=area_gps_mi_50)

areas10_11$area_gps <- ifelse(areas10_11$area_gps %in% 0, NA, areas10_11$area_gps)

rm(dataPath)

