# -------------------------------------
#' Income variables for Uganda wave 1
#' (2010-11). Information on income can be
#' found in the following sections of the
#' questionnaire:
#' 
#' off-farm income (household questionnaire):
#'      1. Module E for off-farm income vars
#'      2. Module N household enterprises
#'      3. Module P other income
#'      
#' on-farm income (agriculture questionnaire):
#'      1. Sections 2a and 2b question 16 for rent income
#'      2. Section 5a for crop production in first cropping season (2009)
#'      3. Section 5b for crop production in second cropping season (2009)
#'      4. Sections 6A, 6B and 6C for livestock sales
#'      5. Section 8 for livestock products
#'      6. Section 9 for fish sold (not used)
#'
#' The output of this file is a dataframe
#' holding all the income variables for each
#' household 
#' 
#' local currency: Malawian Kwacha (mk)
#' 
#' Maize crop codes:
#' 1: maize local
#' 2: maize composite
#' 3: maize hybrid
#' 4: maize hybrid recycled
# -------------------------------------

# load packages
library(dplyr)
library(haven)
library(tidyr)

# set working directory
if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/MWI/2010_11/Data"
} else {
  dataPath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/MWI/2010_11/Data"
}

# -------------------------------------
#' off-farm income
#' Although respondents were asked whether 
#' they had a primary or secondary job 
#' The period in which they were paid was
#' often in days. Unfortunately, the 
#' number of days worked was not recorded
#' and reliable income estimates can
#' therefore not be calculated.
#' 
#' Ganyu labour, short term rural labour,
#' is recorded in a way that makes it
#' possible to calculate and will be
#' recorded as ganyu income.  
# -------------------------------------

# select all income variables
off_farm_income <- read_dta(file.path(dataPath, "Household/HH_MOD_E.dta")) %>%
  select(HHID, case_id, PID,
         mj_industry=hh_e20b, mj_employer=hh_e21,
         mj_months=hh_e22, mj_weekspm=hh_e23,
         mj_hourspw=hh_e24,mj_wage=hh_e25,
         mj_pay_units=hh_e26a, mj_pay_period=hh_e26b, # time units and pay period 
         mj_grat_wage=hh_e27,
         mj_grat_pay_units=hh_e28a, mj_grat_pay_period=hh_e28b, # time units and pay period
         sj_industry=hh_e34b, sj_employer=hh_e35,
         sj_months=hh_e36, sj_weekspm=hh_e37,
         sj_hourspw=hh_e38, sj_wage=hh_e39,
         sj_pay_units=hh_e40a, sj_pay_period=hh_e40b, # time units and pay period 
         sj_grat_wage=hh_e41,
         sj_grat_pay_units=hh_e42a, sj_grat_pay_period=hh_e42b, # time units and pay period
         ganyu=hh_e55, ganyu_months=hh_e56, ganyu_weekspm=hh_e57,
         ganyu_dayspw=hh_e58, ganyu_wagepd=hh_e59)

# as shown in the tables below, the most common
# pay period is "day". We look only an Ganyu labour
table(as_factor(off_farm_income$mj_pay_period)) # mostly day
table(as_factor(off_farm_income$mj_grat_pay_period)) # mostly day
table(as_factor(off_farm_income$sj_pay_period)) # mostly day
table(as_factor(off_farm_income$sj_grat_pay_period)) # mostly day

off_farm_income <- select(off_farm_income, HHID, case_id, PID,
                          ganyu, ganyu_months, ganyu_weekspm,
                          ganyu_dayspw, ganyu_wagepd) %>%
  transmute(HHID, case_id, PID, ganyu,
            ganyu_income = ganyu_months * ganyu_weekspm * ganyu_dayspw * ganyu_wagepd)

off_farm_income$ganyu <- ifelse(off_farm_income$ganyu %in% 1, 1,
                          ifelse(off_farm_income$ganyu %in% 2, 0, NA))

off_farm_income_hh <- group_by(off_farm_income, HHID, case_id) %>%
  summarise(ganyu_income_hh=sum(ganyu_income, na.rm=TRUE))
rm(off_farm_income)

# -------------------------------------
# Module P: Other income
# -------------------------------------

off_farm_income_other <- read_dta(file.path(dataPath, "Household/HH_MOD_P.dta")) %>%
  select(HHID, case_id, code=hh_p0a, income_other=hh_p02)

# summarise to the household level
off_farm_income_other <- group_by(off_farm_income_other, HHID, case_id) %>%
  summarise(income_other=sum(income_other, na.rm=TRUE))

# -------------------------------------
# Module N: household enterprise activities
# Ignore for now for speed - most values
# are at the month level, not clear how to
# scale up to the year level
# -------------------------------------


# -------------------------------------
#' on-farm income
# -------------------------------------

# crop production from the rainy season of 2010_11
cropRS <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_I.dta")) %>%
  select(HHID, case_id, crop_code = ag_i0b,
         qty_harv = ag_i02a, qty_unit = ag_i02b,
         qty_SU = ag_i02c, crop_value = ag_i03)

# crop production from the dry season of 2010_11
cropDS <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_O.dta")) %>%
  select(HHID, case_id, crop_code = ag_o0b,
         qty_harv = ag_o02a, qty_unit = ag_o02b,
         qty_SU = ag_o02c, crop_value = ag_o03)

# join both together
crop <- rbind(cropRS, cropDS)

# local measurements are used - external file in
# order to convrt to kilograms. Includes regions 
# item, unit and conversion - only applies
# to food not raw crops

crop$crop_value_kg <- ifelse(crop$qty_unit %in% 1, crop$qty_harv, NA)

# calculate the full value of crops
# per household

on_farm_income_crop <- group_by(crop, HHID, case_id) %>%
  summarise(crop_value_hh=sum(crop_value_kg, na.rm=TRUE))
rm(crop, cropRS, cropDS)

# rent on land that household member owns
rentRS <- read_dta(file.path(dataPath, "AGSEC2A.dta")) %>%
  select(HHID, parcel_id=a2aq2, rent=a2aq16)

# rent from land that household has rights to
rentDS <- read_dta(file.path(dataPath, "AGSEC2B.dta")) %>%
  select(HHID, parcel_id=a2bq2, rent=a2bq16)

# join the two types of rent together
rent <- rbind(rent1, rent2)

# summarise to the household level
on_farm_income_rent <- group_by(rent, HHID) %>%
  summarise(rent_value_hh=sum(rent, na.rm=TRUE))
rm(rent)

# -------------------------------------
# Livestock
# Respondents were asked if they had sold
# livestock. There are large animals (cows)
# medium animals (goats, sheep etc) and
# small animals like poultry. Income
# is recorded from the sale of each 
# separately
# respondents are asked if they slaughtered
# animals for sale, but the income derived
# is not recorded.
# -------------------------------------

# big animals
big <- read_dta(file.path(dataPath, "AGSEC6A.dta")) %>%
  select(HHID, lvstckcode=a6aq3, lvstk_number_sold=a6aq14,
         lvstk_value=a6aq15, slaughter=a6aq16) %>%
  mutate(type="big")


# medium
med <- read_dta(file.path(dataPath, "AGSEC6B.dta")) %>%
  select(HHID, lvstckcode=a6bq3, lvstk_number_sold=a6bq14,
         lvstk_value=a6bq15, slaughter=a6bq16) %>%
  mutate(type="medium")


# small
small <- read_dta(file.path(dataPath, "AGSEC6C.dta")) %>%
  select(HHID, lvstckcode=a6cq3, lvstk_number_sold=a6cq14,
         lvstk_value=a6cq15, slaughter=a6cq16) %>%
  mutate(type="small")

# combine big, medium and small
on_farm_income_lvstck <- rbind(big, med, small) %>%
  select(HHID, type, lvstk_value)

# group by household and animal
# category (big, medium and small)
# and summarise to teh household level
on_farm_income_lvstck <- group_by(on_farm_income_lvstck, HHID, type) %>%
  summarise(lvstk_value=sum(lvstk_value, na.rm=TRUE)) %>%
  spread(key = type, value = lvstk_value)

names(on_farm_income_lvstck) <- c("HHID", "large_lvstck_inc",
                   "medium_lvstck_inc", "small_lvstck_inc")

# -------------------------------------
# Livestock products sold.
# -------------------------------------

lvstck_products <- read_dta(file.path(dataPath, "AGSEC8.dta")) %>%
  select(HHID, product_code=a8q2, qty=a8q4, qty_unit=a8q5,
         qty_sold=a8q6, lvstck_prod_value=a8q7)

# summarise at the household level
on_farm_income_lvstock_products <- group_by(lvstck_products, HHID) %>%
  summarise(lvstock_prod_value_hh=sum(lvstck_prod_value, na.rm=TRUE))

# -------------------------------------
# fishing - there are questions on
# fish caught and sold, but only how
# much is sold per day and it is no
# recorded how much the sales are per
# year, and therefore not clear how 
# much income was generated from 
# fishing (section 9 of questionnaire)
# -------------------------------------

# -------------------------------------
# Total household income Uganda 2009
# -------------------------------------

income_2009 <- full_join(off_farm_ent, off_farm_income_other)
income_2009 <- full_join(income_2009, on_farm_income_crop)
income_2009 <- full_join(income_2009, on_farm_income_lvstck)
income_2009 <- full_join(income_2009, on_farm_income_lvstock_products)
income_2009 <- full_join(income_2009, on_farm_income_rent)

# take out trash
rm(med, small, big, lvstck_products,
   dataPath, rent1, rent2, off_farm_ent, off_farm_income_other,
   on_farm_income_crop, on_farm_income_lvstck, on_farm_income_lvstock_products,
   on_farm_income_rent)
