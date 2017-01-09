# -------------------------------------
# asset variables MWI 2010-11, wave1
# -------------------------------------


# load packages
library(dplyr)
library(haven)
library(tidyr)
library(reshape2)

# set working directory
if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/MWI/2010_11/Data"
} else {
  dataPath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/MWI/2010_11/Data"
}


# -------------------------------------
# farm implements and machinery
# -------------------------------------

implmt <- read_dta(file.path(dataPath, "Household/HH_MOD_M.dta")) %>%
  select(HHID, case_id, itemcode=hh_m0a, qty=hh_m01, valu=hh_m03) %>%
  filter(!qty %in% 0, !is.na(qty), !valu %in% 0, !is.na(valu)) %>%
  transmute(HHID, case_id, valu=qty*valu) %>%
  group_by(HHID, case_id) %>%
  summarise(asset=sum(valu,na.rm=TRUE))

# -------------------------------------
# Livestock assets
# -------------------------------------

LR <- c(301:304)
SR <- c(307, 308)
PIGS <- c(309)
POULTRY <- c(310:316)
OTHER <- c(305, 306)

# read in the data
lvstock <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_R1.dta")) %>%
  select(HHID, case_id, animal = ag_r0a, qty=ag_r02, valu=ag_r04)

# count the number of animals of each class a household owns
lvstock_x <- select(lvstock, HHID, case_id, animal, qty) %>%
  melt(id = c("HHID", "case_id", "animal")) %>%
  group_by(HHID, case_id, animal) %>%
  mutate(class = ifelse(animal %in% LR, "LR",
                        ifelse(animal %in% SR, "SR", 
                               ifelse(animal %in% PIGS, "PIGS_",
                                      ifelse(animal %in% POULTRY, "POULTRY",
                                             ifelse(animal %in% OTHER, "OTHER_", "unknown")))))) %>%
  group_by(HHID, case_id, class) %>%
  summarise(n=sum(value, na.rm=TRUE)) %>%
  dcast(HHID + case_id ~ class)

# calculate the value of all livestock assets
# to a household
lvstock_y <- group_by(lvstock, HHID, case_id) %>%
  summarise(lvstock_value = sum(qty*valu, na.rm=TRUE))

# join together
lvstock <- left_join(lvstock_x, lvstock_y)
lvstock$unknown <- NULL

# -------------------------------------
# join livestock assets with machinery
# assets
# -------------------------------------

assets_2010_11 <- left_join(implmt, lvstock)

rm("LR", "SR", "lvstock_x", "lvstock_y",
   "OTHER", "PIGS", "POULTRY", "dataPath",
   "implmt", "lvstock")
