# -------------------------------------
# Output MWI 2010_11 (wave 1)
# two seasons rainy and dry
# crops and permanent crops
# seed = seed planted in the rainy season for crop (factor)
# -------------------------------------

# set working directory
if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/MWI/2010_11/Data"
} else {
  dataPath <- "C:\\Users\\vandijkm\\OneDrive - IIASA\\SurveyData\\MWI\\2010\\Data"
}

# load packages
library(haven)
library(dplyr)

# crop output
oput_2010_11 <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_G.dta")) %>%
  select(HHID, case_id, ea_id, plotnum=ag_g0b, crop_code=ag_g0d,
         crop_stand=ag_g01, crop_share=ag_g03, harv_start = ag_g12a,
         harv_end = ag_g12b, crop_qty_harv=ag_g13a,
         unit=ag_g13b, condition=ag_g13c)

# change one_crop to crop stand
oput_2010_11$crop_stand <- ifelse(oput_2010_11$crop_stand %in% 1, 1,
                        ifelse(oput_2010_11$crop_stand %in% 2, 0, NA))
oput_2010_11$crop_share <- as_factor(oput_2010_11$crop_share)
oput_2010_11$harv_start <- as_factor(oput_2010_11$harv_start)
oput_2010_11$harv_end <- as_factor(oput_2010_11$harv_end)
oput_2010_11$crop_code <- as.integer(oput_2010_11$crop_code)
oput_2010_11$unit <- as.integer(oput_2010_11$unit)
oput_2010_11$condition <- as.integer(oput_2010_11$condition)

#' crop quantities are recorded in non-standard units.
#' The world bank provided (upon request) a file with
#' the correct conversion units per region, crop, unit,
#' and condition

# get region variable
region <- read_dta(file.path(dataPath, "Household/HH_MOD_A_FILT.dta")) %>%
  transmute(HHID, case_id, ea_id, region=NA, district = hh_a01)
region$region <- with(region,
                             ifelse(district < 200, 1,
                                    ifelse(district >=200 & district < 300, 2,
                                           3)))
region$district <- NULL
region$region <- as.integer(region$region)

# get conversion variables
qty2kg <- read_dta(file.path(dataPath, "../../../Other/Conversion/MWI/IHS.Agricultural.Conversion.Factor.Database.dta"))
qty2kg$crop_code <- as.integer(qty2kg$crop_code)
qty2kg$unit <- as.integer(qty2kg$unit)
qty2kg$condition <- as.integer(qty2kg$condition)
qty2kg$flag <- NULL

# join region variable to the oput variables
# and then join with the conversion factors
oput_2010_11 <- left_join(oput_2010_11, region)
oput_2010_11 <- left_join(oput_2010_11, qty2kg)

# multiply the recorded quantity by conversion
# to kilograms
oput_2010_11$crop_qty_harv <- oput_2010_11$crop_qty_harv * oput_2010_11$conversion
oput_2010_11$unit <- oput_2010_11$shell_unshelled <- oput_2010_11$conversion <-
  oput_2010_11$condition <- NULL

# -------------------------------------
# create dummy variables for crop groups
# (fruit, cash crops (permanent),
# Cereals/Tubers/Roots, cash crops (not permanent),
# vegetables, legumes)
# -------------------------------------

# I was unable to place some foodtsuffs in 
# the correct category. The following are unknown
# 1. ground bean: code 27
# 2. macademia: code 17
# 3. Tanaposi: code 41
# 4. NKHWANI: code 42
# 5. Therere/Okra: code 43
# 6. pea: code 46
# 7. paprika: code 47

# fruit <- c(5, 6, 7, 9, 10, 11, 12, 13, 14)
# cash_crop_perm <- c(36, 39, 16)  # permanent cash crops
# ctr <- c(17, 18, 19, 20, 21, 22, 23,
#          24, 25, 26, 28, 29, 31, 32, 33, 45) # Cereals, Tubers, Roots
# cash_crop_nperm <- c(37) # non permanent cash crops
# vegetables <- c(40, 44)
# legumes <- c(11, 12, 13, 14, 15, 16, 34, 35, 38)
# 
# 
# oput_2010_11_x <- group_by(oput_2010_11, HHID, case_id, plotnum) %>%
#   summarise(crop_count=length(unique(crop_code[!is.na(crop_code)])),
#             fruit=ifelse(any(crop_code %in% fruit), 1, 0),
#             cash_crops_perm=ifelse(any(crop_code %in% cash_crop_perm), 1, 0),
#             ctr=ifelse(any(crop_code %in% ctr), 1, 0),
#             cash_crop_nperm=ifelse(any(crop_code %in% cash_crop_nperm), 1, 0),
#             vegetables=ifelse(any(crop_code %in% vegetables), 1, 0),
#             legume=ifelse(any(crop_code %in% legumes), 1, 0),
#             maize_=ifelse(any(crop_code %in% c(1, 2, 3, 4)), 1, 0), # maize has crop code 1, 2, 3 or 4
#             wheat=ifelse(any(crop_code %in% 30), 1, 0)) # wheat has crop code 30
# 
# oput_2010_11 <- left_join(oput_2010_11, oput_2010_11_x); rm(oput_2010_11_x)

# who responded they produced zero crop, or did not respond (NA)
# and wok out how to get the prices information. Prices will be
# overall values -> probably more accurate.

# crop production from the rainy season of 2010_11
# in order to get unit prices of each crop.
# These need to be matched with region and then
# converted as above

crop_unit_priceRS <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_I.dta")) %>%
  select(HHID, case_id, ea_id, crop_code = ag_i0b,
         qty_harv = ag_i02a, unit = ag_i02b,
         condition = ag_i02c, crop_value = ag_i03) %>% unique()

# Join with region and then conversion factor
crop_unit_priceRS <- left_join(crop_unit_priceRS, region)
crop_unit_priceRS <- left_join(crop_unit_priceRS, qty2kg)

# make conversion and calculate the crop prices
crop_unit_priceRS$qty_harv <- crop_unit_priceRS$qty_harv * crop_unit_priceRS$conversion
crop_unit_priceRS$crop_price <- crop_unit_priceRS$crop_value/crop_unit_priceRS$qty_harv
crop_unit_priceRS <- select(crop_unit_priceRS, HHID, case_id, ea_id, crop_code, crop_value, qty_harv, crop_price)

# join prices with output
oput_2010_11_2 <- left_join(oput_2010_11,
                          crop_unit_priceRS)

# if someone either did not have any output or
# had 0 output remove them from the data frame
oput_2010_11 <- oput_2010_11[! is.na(oput_2010_11$crop_qty_harv) & !oput_2010_11$crop_qty_harv %in% 0, ]

# At this point, in theory, we don't need the units
# anymore as everything is in kg, and we don't care
# about whether the crop was shelled or unshelled as
# this should also be accounted for in the units

oput_2010_11$crop_qty_harv_unit <- oput_2010_11$crop_qty_harvSU <-
  oput_2010_11$region <- NULL

# take out the trash
rm(list=c("legumes", "cash_crop_nperm", "cash_crop_perm",
          "ctr", "fruit", "vegetables", "crop_unit_priceRS",
          "dataPath", "region", "qty2kg"))
