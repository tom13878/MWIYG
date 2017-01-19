# -------------------------------------
# asset variables MWI 2010-11, wave1
# -------------------------------------


# load packages
library(dplyr)
library(haven)
library(tidyr)

# set working directory
if(Sys.info()["user"] == "morle001"){
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/MWI/2013/Data"
} else {
  dataPath <- "C:\\Users\\vandijkm\\OneDrive - IIASA\\SurveyData\\MWI\\2013\\Data"
}


# -------------------------------------
# farm implements and machinery
# -------------------------------------

# value of implments not recorded in 
# the second wave. Unable to create
# machinery asset values

# -------------------------------------
# Livestock assets
# -------------------------------------

LR <- c(301:304, 3305)
SR <- c(307, 308)
PIGS <- c(309)
POULTRY <- c(3310, 311:319)
OTHER <- c(3305)

# read in the data
lvstock <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_R1.dta")) %>%
  select(y2_hhid, animal = ag_r0a, qty=ag_r02, valu=ag_r04)

# count the number of animals of each class a household owns
lvstock_x <- select(lvstock, y2_hhid, animal, qty) %>%
  melt(id = c("y2_hhid", "animal")) %>%
  group_by(y2_hhid, animal) %>%
  mutate(class = ifelse(animal %in% LR, "LR",
                        ifelse(animal %in% SR, "SR", 
                               ifelse(animal %in% PIGS, "PIGS_",
                                      ifelse(animal %in% POULTRY, "POULTRY",
                                             ifelse(animal %in% OTHER, "OTHER_", "unknown")))))) %>%
  group_by(y2_hhid, class) %>%
  summarise(n=sum(value, na.rm=TRUE)) %>%
  dcast(y2_hhid ~ class)

# calculate the value of all livestock assets
# to a household
lvstock_y <- group_by(lvstock, y2_hhid) %>%
  summarise(lvstock_value = sum(qty*valu, na.rm=TRUE))

# join together
lvstock <- left_join(lvstock_x, lvstock_y)
lvstock$unknown <- NULL

# -------------------------------------
# No machinery assets so we have only
# livestock assets for 2013
# -------------------------------------

assets_2013 <- lvstock

rm("LR", "SR", "lvstock_x", "lvstock_y",
   "OTHER", "PIGS", "POULTRY", "dataPath",
   "lvstock")
