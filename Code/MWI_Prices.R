# -------------------------------------
# MWI fertilizer price data
# Malawi is split into 28 districts in
# three regions.
# > A <- read_dta(file.path(dataPath, "/MWI/2010_11/data/Household/HH_MOD_A_FILT.dta"))
# > table(as_factor(A$hh_a01))
# -------------------------------------

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data"
}

# packages
library(dplyr)
library(haven)

# winsor code
source("C:/Users/Tomas/Documents/LEI/functions/winsor.R")

# load pooled data
# source("") panel data when ready

conv <- read.csv(file.path(dataPath, "Other/Fertilizer/Fert_comp.csv")) %>%
  transmute(typ=Fert_type2, n=N_share/100, p=P_share/100) %>%
  filter(typ %in% c("UREA", "DAP"))

# Note that we only know on which field fertilizer is used, not if they are maize plots.
# We decide to calculate the average fertilizer price over maize plots (i.e. plots were among others maize is grown) only as it is possible that because of subsidies or other policies 
# the price of the same type of fertilizer (e.g. UREA) can differ between type of crop, even in the same region
# Inflation is not a problem if we use relative prices (pmaize/pfert), which are assumed to have same inflation.


# read in the fertilizer data and combine in one file
# 2010
fert2010_1 <- read_dta(file.path(dataPath, "/MWI/2010_11/data/Agriculture/AG_MOD_F.dta")) %>%
  transmute(HHID, case_id, typ=toupper(as.character(as_factor(ag_f0c))), qty=ag_f16a, qty_unit = as_factor(ag_f16b), valu=ag_f19) %>%
  do(filter(., complete.cases(.)))
levels(fert2010_1$qty_unit) <- c(0.001, 1, 2, 3, 5, 10, 50, 1, 0.001, 96,97, 98, 99)
fert2010_1$unit2kg <- as.numeric(as.character(fert2010_1$qty_unit)); fert2010_1$qty_unit <- NULL
fert2010_1$unit2kg <- ifelse(fert2010_1$unit2kg > 90, NA, fert2010_1$unit2kg)
fert2010_1$qty <- fert2010_1$qty * fert2010_1$unit2kg
fert2010_1$unit2kg <- NULL


fert2010_2 <- read_dta(file.path(dataPath, "/MWI/2010_11/data/Agriculture/AG_MOD_F.dta")) %>%
  transmute(HHID, case_id, typ=toupper(as.character(as_factor(ag_f0c))), qty=ag_f26a, qty_unit = as_factor(ag_f26b), valu=ag_f29) %>%
  do(filter(., complete.cases(.)))
levels(fert2010_2$qty_unit) <- c(0.001, 1, 2, 3, 5, 10, 50, 1, 0.001, 96,97, 98, 99)
fert2010_2$unit2kg <- as.numeric(as.character(fert2010_2$qty_unit)); fert2010_2$qty_unit <- NULL
fert2010_2$unit2kg <- ifelse(fert2010_2$unit2kg > 90, NA, fert2010_2$unit2kg)
fert2010_2$qty <- fert2010_2$qty * fert2010_2$unit2kg
fert2010_2$unit2kg <- NULL

# next stage is to add zone, region district info

