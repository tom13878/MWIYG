# -------------------------------------
#' Plot Variables
#' Malawi wave 1 (2013)
#' collection dates: April - November 2013 
#'
#' This file contains plot level information
#' output: dataframe of plot level information
# -------------------------------------

# set working directory
if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/MWI/2013/Data"
} else {
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/MWI/2010/Data"
}

# load packages
library(haven)
library(dplyr)
library(tidyr)

# plot details - Rainy season Agricultural questionnaire section D

plotRS <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_D.dta")) %>%
  select(HHID, case_id, ea_id, plotnum = ag_d00, crop1 = ag_d20a,
         crop2 = ag_d20b, crop3 = ag_d20c, crop4 = ag_d20d, crop5 = ag_d20e,
         soil = )

, soil=ag3a_09, slope_farmer=ag3a_16, irrig=ag3a_17, title=ag3a_27,
manure=ag3a_39, pest=ag3a_58, pest_q=ag3a_60_1, pest_q_unit=ag3a_60_2, fallow_year=ag3a_21, fallow=ag3a_22)