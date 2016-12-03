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


oput <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_G.dta")) %>%
  select(y2_hhid, plotnum=ag_g00, crop_code=ag_g0b, crop_stand = ag_g01,
         one_crop=ag_g02, crop_share=ag_g03, harv_start = ag_g12a,
         harv_end = ag_g12b, crop_qty_harv1 = ag_g09a,
         crop_qty_harv2 = ag_g09b, crop_qty_harv3 = ag_g09c)

oput$one_crop <- ifelse(oput$one_crop %in% 1, 1,
                        ifelse(oput$one_crop %in% 2, 0, NA))
oput$crop_stand <- as_factor(oput$crop_stand)
oput$crop_share <- as_factor(oput$crop_share)
oput$harv_start <- as_factor(oput$harv_start)
oput$harv_end <- as_factor(oput$harv_end)
oput$crop_code <- as.integer(oput$crop_code)

# difficulty is with non standard units. 
# 50kg bag does not really weight 50kg
# measurements like Fail Basket etc.
# unit conversions not available in 
# another section of the questionnaire.
# for now only look at records in kilograms
# A further complication is that utput may
# be shelled or unshelled

oput$crop_qty_harv <- ifelse(oput$crop_qty_harv2 %in% 1,
                             oput$crop_qty_harv1, NA)

# remove unecessary variables
oput$crop_qty_harv1 <- oput$crop_qty_harv2 <-
  oput$crop_qty_harv3 <- NULL

# -------------------------------------
# create dummy variables for crop groups
# (fruit, cash crops (permanent),
# Cereals/Tubers/Roots, cash crops (not permanent),
# vegetables, legumes)
# -------------------------------------

# ground bean unknown 27
# macademia unknwn 17
# Tanaposi unknown 41
# NKHWANI unknown 42
# Therere/Okra 43
# pea 46
# paprika 47

fruit <- c(3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14)
cashCropsPerm <- c(36, 39, 1, 2, 3, 16)  # permanent cash crops
CTR <- c(5, 6, 7, 8, 9, 10, 17, 18, 19, 20, 21, 22, 23,
         24, 25, 26, 28, 29, 31, 32, 33, 45) # Cereals, Tubers, Roots
cashCropNPerm <- c(37) # non permanent cash crops
vegetables <- c(40, 44)
legumes <- c(11, 12, 13, 14, 15, 16, 34, 35, 38)


oput_x <- group_by(oput, y2_hhid, plotnum) %>%
  summarise(crop_count=length(unique(crop_code[!is.na(crop_code)])),
            fruit=ifelse(any(crop_code %in% fruit), 1, 0),
            cashCropsPerm=ifelse(any(crop_code %in% cashCropsPerm), 1, 0),
            CTR=ifelse(any(crop_code %in% CTR), 1, 0),
            cashCropNPerm=ifelse(any(crop_code %in% cashCropNPerm), 1, 0),
            vegetables=ifelse(any(crop_code %in% vegetables), 1, 0),
            legume=ifelse(any(crop_code %in% legumes), 1, 0),
            maize_=ifelse(any(crop_code %in% c(1, 2, 3, 4)), 1, 0), # maize has crop code 1, 2, 3 or 4
            wheat=ifelse(any(crop_code %in% 30), 1, 0)) # wheat has crop code 30

oput <- left_join(oput, oput_x); rm(oput_x)

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
crop_unit_priceRS$qty_harvkg <- ifelse(crop_unit_priceRS$qty_unit %in% 1,
                                       crop_unit_priceRS$qty_harv, NA)
crop_unit_priceRS$crop_price <- crop_unit_priceRS$crop_value/crop_unit_priceRS$qty_harvkg
crop_unit_priceRS <- select(crop_unit_priceRS, y2_hhid, crop_code, crop_price)

# join prices with output
oput <- left_join(oput, crop_unit_priceRS)

# if someone either did not have any output or
# had 0 output remove them from the data frame
oput <- oput[! is.na(oput$crop_qty_harv) & !oput$crop_qty_harv %in% 0, ]


rm(list=c("legumes", "cashCropNPerm", "cashCropsPerm",
          "CTR", "fruit", "vegetables", "crop_unit_priceRS",
          "dataPath"))
