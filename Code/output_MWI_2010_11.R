# -------------------------------------
# Output MWI 2010_11 (wave 1)
# two seasons rainy and dry
# crops and permanent crops
# seed = seed planted in the rainy season for crop (factor)
# -------------------------------------

# set working directory
if(Sys.info()["user"] == "morle001"){
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/MWI/2010/Data"
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
oput_2010_11$crop_code <- as.integer(oput_2010_11$crop_code)

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
qty2kg$region <- as.integer(qty2kg$region)
qty2kg$flag <- NULL

# join region variable to the oput variables
# and then join with the conversion factors
oput_2010_11 <- left_join(oput_2010_11, region)

# attributes in the qty2kg conversion file
# prevent joining attributes. Strip attributes
# first
stripAttributes <- function(df){
  df[] <- lapply(df, as.vector)
  return(df)
}
qty2kg <- stripAttributes(qty2kg)

# now join works with no errors
oput_2010_11 <- left_join(oput_2010_11, qty2kg)

# multiply the recorded quantity by conversion
# to kilograms
oput_2010_11$crop_qty_harv <- oput_2010_11$crop_qty_harv * oput_2010_11$conversion
oput_2010_11$unit <- oput_2010_11$shell_unshelled <- oput_2010_11$conversion <-
  oput_2010_11$condition <- NULL

# crop production from the rainy season of 2010_11
# in order to get unit prices of each crop.
# These need to be matched with region and then
# converted as above

crop_unit_priceRS <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_I.dta")) %>%
  select(HHID, case_id, ea_id, crop_code = ag_i0b,
         qty_harv = ag_i02a, unit = ag_i02b,
         condition = ag_i02c, crop_value = ag_i03) %>% unique()
crop_unit_priceRS$crop_code <- as.integer(crop_unit_priceRS$crop_code)
crop_unit_priceRS$unit <- as.integer(crop_unit_priceRS$unit)
crop_unit_priceRS$condition <- as.integer(crop_unit_priceRS$condition)

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
