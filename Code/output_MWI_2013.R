# -------------------------------------
# Output MWI 2013 (wave 2)
# two seasons rainy and dry
# crops and permanent crops
# seed = seed planted in the rainy season for crop (factor)
# -------------------------------------

# set working directory
if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/MWI/2013/Data"
} else {
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/MWI/2013/Data"
}

# load packages
library(haven)
library(dplyr)


oput2013 <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_G.dta")) %>%
  select(y2_hhid, plotnum=ag_g00, crop_code=ag_g0b, crop_stand = ag_g01,
         one_crop=ag_g02, crop_share=ag_g03, harv_start = ag_g12a,
         harv_end = ag_g12b, crop_qty_harv = ag_g09a,
         crop_qty_harv_unit = ag_g09b, crop_qty_harvSU = ag_g09c)

oput2013$one_crop <- ifelse(oput2013$one_crop %in% 1, 1,
                        ifelse(oput2013$one_crop %in% 2, 0, NA))
oput2013$crop_stand <- as_factor(oput2013$crop_stand)
oput2013$crop_share <- as_factor(oput2013$crop_share)
oput2013$harv_start <- as_factor(oput2013$harv_start)
oput2013$harv_end <- as_factor(oput2013$harv_end)
oput2013$crop_code <- as.integer(oput2013$crop_code)

# difficulty is with non standard units. 
# 50kg bag does not really weight 50kg
# measurements like Fail Basket etc.
# unit conversions not available in 
# another section of the questionnaire.
# for now only look at records in kilograms
# A further complication is that utput may
# be shelled or unshelled

oput2013$crop_qty_harv <- ifelse(oput2013$crop_qty_harv2 %in% 1,
                             oput2013$crop_qty_harv1, NA)

#' difficulty with non standard units. 
#' There is a market questionnaire that
#' should convert these units - but not
#' available in data, have contacted world bank already
#' to see if that information is available.
#' 
#' A 50kg bag does not really hold 50kg
#' of crop. However, we do know that it
#' holds around 29 kg from a lsms presentation
#' A 90kg bag presumably is the same.
#' A further complication is that output may
#' be shelled or unshelled

oput2013$crop_qty_harv <- ifelse(oput2013$crop_qty_harv_unit %in% 1,oput2013$crop_qty_harv,
                             ifelse(oput2013$crop_qty_harv_unit %in% 2, oput2013$crop_qty_harv * 29,
                                    ifelse(oput2013$crop_qty_harv_unit %in% 3, oput2013$crop_qty_harv * 52.2, NA)))


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

fruit <- c(5, 6, 7, 9, 10, 11, 12, 13, 14)
cash_crop_perm <- c(36, 39, 16)  # permanent cash crops
ctr <- c(17, 18, 19, 20, 21, 22, 23,
         24, 25, 26, 28, 29, 31, 32, 33, 45) # Cereals, Tubers, Roots
cash_crop_nperm <- c(37) # non permanent cash crops
vegetables <- c(40, 44)
legumes <- c(11, 12, 13, 14, 15, 16, 34, 35, 38)


oput2013_x <- group_by(oput2013, y2_hhid, plotnum) %>%
  summarise(crop_count=length(unique(crop_code[!is.na(crop_code)])),
            fruit=ifelse(any(crop_code %in% fruit), 1, 0),
            cash_crops_perm=ifelse(any(crop_code %in% cash_crop_perm), 1, 0),
            ctr=ifelse(any(crop_code %in% ctr), 1, 0),
            cash_crop_nperm=ifelse(any(crop_code %in% cash_crop_nperm), 1, 0),
            vegetables=ifelse(any(crop_code %in% vegetables), 1, 0),
            legume=ifelse(any(crop_code %in% legumes), 1, 0),
            maize_=ifelse(any(crop_code %in% c(1, 2, 3, 4)), 1, 0), # maize has crop code 1, 2, 3 or 4
            wheat=ifelse(any(crop_code %in% 30), 1, 0)) # wheat has crop code 30

oput2013 <- left_join(oput2013, oput2013_x); rm(oput2013_x)

# who responded they produced zero crop, or did not respond (NA)
# and wok out how to get the prices information. Prices will be
# overall values -> probably more accurate.

# crop production from the rainy season of 2013
# in order to get unit prices of each crop.
crop_unit_priceRS <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_I.dta")) %>%
  select(y2_hhid, crop_code = ag_i0b,
         qty_harv = ag_i02a, qty_unit = ag_i02b,
         qty_SU = ag_i02c, crop_value = ag_i03)

# get a unit price, again only looking at the 
# quantities recorded in kilograms
crop_unit_priceRS$qty_harvkg <- ifelse(crop_unit_priceRS$qty_unit %in% 1,crop_unit_priceRS$qty_harv,
                                       ifelse(crop_unit_priceRS$qty_unit %in% 2, crop_unit_priceRS$qty_harv * 29,
                                              ifelse(crop_unit_priceRS$qty_unit %in% 3, crop_unit_priceRS$qty_harv * 52.2, NA)))

crop_unit_priceRS$crop_price <- crop_unit_priceRS$crop_value/crop_unit_priceRS$qty_harvkg
crop_unit_priceRS <- select(crop_unit_priceRS, y2_hhid, crop_code, crop_value, qty_harvkg, crop_price)

# join prices with output
oput2013 <- left_join(oput2013, crop_unit_priceRS)

# if someone either did not have any output or
# had 0 output remove them from the data frame
oput2013 <- oput2013[! is.na(oput2013$crop_qty_harv) & !oput2013$crop_qty_harv %in% 0, ]

# At this point, in theory, we don't need the units
# anymore as everything is in kg, and we don't care
# about whether the crop was shelled or unshelled as
# this should also be accounted for in the units

oput2013$crop_qty_harv_unit <- oput2013$crop_qty_harvSU <- NULL

# take out the trash
rm(list=c("legumes", "cash_crop_nperm", "cash_crop_perm",
          "ctr", "fruit", "vegetables", "crop_unit_priceRS",
          "dataPath"))