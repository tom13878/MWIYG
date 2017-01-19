# -------------------------------------
# Output MWI 2013 (wave 2)
# two seasons rainy and dry
# crops and permanent crops
# seed = seed planted in the rainy season for crop (factor)
# -------------------------------------

# set working directory
if(Sys.info()["user"] == "morle001"){
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/MWI/2013/Data"
} else {
  dataPath <- "C:\\Users\\vandijkm\\OneDrive - IIASA\\SurveyData\\MWI\\2013\\Data"
}

# load packages
library(haven)
library(dplyr)


oput_2013 <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_G.dta")) %>%
  select(y2_hhid, plotnum=ag_g00, crop_code=ag_g0b, crop_stand = ag_g01,
         one_crop=ag_g02, crop_share=ag_g03, harv_start = ag_g12a,
         harv_end = ag_g12b, crop_qty_harv = ag_g09a,
         unit = ag_g09b, condition = ag_g09c)

oput_2013$one_crop <- ifelse(oput_2013$one_crop %in% 1, 1,
                        ifelse(oput_2013$one_crop %in% 2, 0, NA))
oput_2013$crop_stand <- as_factor(oput_2013$crop_stand)
oput_2013$crop_share <- as_factor(oput_2013$crop_share)
oput_2013$harv_start <- as_factor(oput_2013$harv_start)
oput_2013$harv_end <- as_factor(oput_2013$harv_end)
oput_2013$crop_code <- as.integer(oput_2013$crop_code)
oput_2013$unit <- as.integer(oput_2013$unit)
oput_2013$condition <- as.integer(oput_2013$condition)

#' crop quantities are recorded in non-standard units.
#' The world bank provided (upon request) a file with
#' the correct conversion units per region, crop, unit,
#' and condition

# get region variable
region <- read_dta(file.path(dataPath, "Household/HH_MOD_A_FILT.dta")) %>%
  select(y2_hhid, ea_id, region)
region$region <- as.integer(region$region)

# get conversion variables
qty2kg <- read_dta(file.path(dataPath, "../../../Other/Conversion/MWI/IHS.Agricultural.Conversion.Factor.Database.dta"))
qty2kg$crop_code <- as.integer(qty2kg$crop_code)
qty2kg$unit <- as.integer(qty2kg$unit)
qty2kg$condition <- as.integer(qty2kg$condition)
qty2kg$flag <- NULL

# join region variable to the oput variables
# and then join with the conversion factors
oput_2013 <- left_join(oput_2013, region)

# attributes in the qty2kg conversion file
# prevent joining attributes. Strip attributes
# first
stripAttributes <- function(df){
  df[] <- lapply(df, as.vector)
  return(df)
}
qty2kg <- stripAttributes(qty2kg)

# now join works with no errors
oput_2013 <- left_join(oput_2013, qty2kg)

# multiply the recorded quantity by conversion
# to kilograms
oput_2013$crop_qty_harv <- oput_2013$crop_qty_harv * oput_2013$conversion
oput_2013$unit <- oput_2013$shell_unshelled <- oput_2013$conversion <-
  oput_2013$condition <- NULL

# who responded they produced zero crop, or did not respond (NA)
# and wok out how to get the prices information. Prices will be
# overall values -> probably more accurate.

# crop production from the rainy season of 2013
# in order to get unit prices of each crop.
crop_unit_priceRS <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_I.dta")) %>%
  select(y2_hhid, crop_code = ag_i0b,
         qty_harv = ag_i02a, unit = ag_i02b,
         condition = ag_i02c, crop_value = ag_i03)
crop_unit_priceRS$crop_code <- as.integer(crop_unit_priceRS$crop_code)
crop_unit_priceRS$unit <- as.integer(crop_unit_priceRS$unit)
crop_unit_priceRS$condition <- as.integer(crop_unit_priceRS$condition)

# Join with region and then conversion factor
crop_unit_priceRS <- left_join(crop_unit_priceRS, region)
crop_unit_priceRS <- left_join(crop_unit_priceRS, qty2kg)

# make conversion and calculate the crop prices
crop_unit_priceRS$qty_harv <- crop_unit_priceRS$qty_harv * crop_unit_priceRS$conversion
crop_unit_priceRS$crop_price <- crop_unit_priceRS$crop_value/crop_unit_priceRS$qty_harv
crop_unit_priceRS <- select(crop_unit_priceRS, y2_hhid, crop_code, crop_value, qty_harv, crop_price)

# join prices with output
oput_2013 <- left_join(oput_2013, crop_unit_priceRS)

# if someone either did not have any output or
# had 0 output remove them from the data frame
oput_2013 <- oput_2013[! is.na(oput_2013$crop_qty_harv) & !oput_2013$crop_qty_harv %in% 0, ]

# At this point, in theory, we don't need the units
# anymore as everything is in kg, and we don't care
# about whether the crop was shelled or unshelled as
# this should also be accounted for in the units

oput_2013$region <- oput_2013$unit <- oput_2013$condition <- NULL

# take out the trash
rm(list=c("legumes", "cash_crop_nperm", "cash_crop_perm",
          "ctr", "fruit", "vegetables", "crop_unit_priceRS",
          "dataPath", "region", "qty2kg"))

