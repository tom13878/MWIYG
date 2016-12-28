# -------------------------------------
# MWI fertilizer price data
# Malawi is split into 28 districts in
# three regions.
# -------------------------------------

# packages
library(dplyr)
library(haven)
library(sjmisc)

# winsor code
source("C:/Users/Tomas/Documents/LEI/functions/winsor.R")

# load pooled data
source("c:/users/tomas/documents/lei/MWIYG/Code/panel_MWI.R")
dbP <- panel

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data"
}

# read in nitrogen conversion file
conv <- read.csv(file.path(dataPath, "Other/Fertilizer/Fert_comp.csv")) %>%
  transmute(typ=Fert_type2, n=N_share/100, p=P_share/100) %>%
  filter(typ %in% c("UREA", "DAP"))

# Note that we only know on which field fertilizer is used, not if they are maize plots.
# We decide to calculate the average fertilizer price over maize plots (i.e. plots were among others maize is grown) only as it is possible that because of subsidies or other policies 
# the price of the same type of fertilizer (e.g. UREA) can differ between type of crop, even in the same region
# Inflation is not a problem if we use relative prices (pmaize/pfert), which are assumed to have same inflation.


# read in the fertilizer data and combine in one file
# 2011 - note fertilizer recorded at household level
fert2011_1 <- read_dta(file.path(dataPath, "/MWI/2010_11/data/Agriculture/AG_MOD_F.dta")) %>%
  transmute(HHID, case_id, typ=toupper(as.character(as_factor(ag_f0c))), qty=ag_f16a, qty_unit = as_factor(ag_f16b), valu=ag_f19) %>%
  do(filter(., complete.cases(.)))
levels(fert2011_1$qty_unit) <- c(0.001, 1, 2, 3, 5, 10, 50, 1, 0.001, 96,97, 98, 99)
fert2011_1$unit2kg <- as.numeric(as.character(fert2011_1$qty_unit)); fert2011_1$qty_unit <- NULL
fert2011_1$unit2kg <- ifelse(fert2011_1$unit2kg > 90, NA, fert2011_1$unit2kg)
fert2011_1$qty <- fert2011_1$qty * fert2011_1$unit2kg
fert2011_1$unit2kg <- NULL


fert2011_2 <- read_dta(file.path(dataPath, "/MWI/2010_11/data/Agriculture/AG_MOD_F.dta")) %>%
  transmute(HHID, case_id, typ=toupper(as.character(as_factor(ag_f0c))), qty=ag_f26a, qty_unit = as_factor(ag_f26b), valu=ag_f29) %>%
  do(filter(., complete.cases(.)))
levels(fert2011_2$qty_unit) <- c(0.001, 1, 2, 3, 5, 10, 50, 1, 0.001, 96,97, 98, 99)
fert2011_2$unit2kg <- as.numeric(as.character(fert2011_2$qty_unit)); fert2011_2$qty_unit <- NULL
fert2011_2$unit2kg <- ifelse(fert2011_2$unit2kg > 90, NA, fert2011_2$unit2kg)
fert2011_2$qty <- fert2011_2$qty * fert2011_2$unit2kg
fert2011_2$unit2kg <- NULL

# key prepared to add info on ZONE, REGION and DISTRICT
key_2011 <- filter(dbP, surveyyear == 2011) %>%
  select(HHID, case_id, region, ea_id, district, surveyyear) %>%
  unique() %>% 
  do(filter(., complete.cases(.)))

fert2011 <- rbind(fert2011_1, fert2011_2) %>%
  remove_all_labels() %>%
  left_join(key_2011,.) %>%
  do(filter(., complete.cases(.)))

# 2012
# read in the fertilizer data and combine in one file
# 2013 - note fertilizer recorded at household level
fert2013_1 <- read_dta(file.path(dataPath, "/MWI/2013/data/Agriculture/AG_MOD_F.dta")) %>%
  transmute(y2_hhid, typ=toupper(as.character(as_factor(ag_f0c))), qty=ag_f16a, qty_unit = as_factor(ag_f16b), valu=ag_f19) %>%
  do(filter(., complete.cases(.)))
levels(fert2013_1$qty_unit) <- c(0.001, 1, 2, 3, 5, 10, 50, 1, 0.001, 96,97, 98, 99)
fert2013_1$unit2kg <- as.numeric(as.character(fert2013_1$qty_unit)); fert2013_1$qty_unit <- NULL
fert2013_1$unit2kg <- ifelse(fert2013_1$unit2kg > 90, NA, fert2013_1$unit2kg)
fert2013_1$qty <- fert2013_1$qty * fert2013_1$unit2kg
fert2013_1$unit2kg <- NULL


fert2013_2 <- read_dta(file.path(dataPath, "/MWI/2013/data/Agriculture/AG_MOD_F.dta")) %>%
  transmute(y2_hhid, typ=toupper(as.character(as_factor(ag_f0c))), qty=ag_f26a, qty_unit = as_factor(ag_f26b), valu=ag_f29) %>%
  do(filter(., complete.cases(.)))
levels(fert2013_2$qty_unit) <- c(0.001, 1, 2, 3, 5, 10, 50, 1, 0.001, 96,97, 98, 99)
fert2013_2$unit2kg <- as.numeric(as.character(fert2013_2$qty_unit)); fert2013_2$qty_unit <- NULL
fert2013_2$unit2kg <- ifelse(fert2013_2$unit2kg > 90, NA, fert2013_2$unit2kg)
fert2013_2$qty <- fert2013_2$qty * fert2013_2$unit2kg
fert2013_2$unit2kg <- NULL

# neither of the fert files have the panel hhid, case_id variables
# for 2013
panelHH <- read_dta(file.path(dataPath, "MWI/2013/data/Household/HH_MOD_A_FILT.dta")) %>%
  select(y2_hhid, HHID, case_id)
fert2013_1 <- left_join(fert2013_1, panelHH) %>%
  select(HHID, case_id, everything())
fert2013_2 <- left_join(fert2013_2, panelHH) %>%
  select(HHID, case_id, everything())
fert2013_1$y2_hhid <- fert2013_2$y2_hhid <- NULL


# key prepared to add info on ZONE, REGION and DISTRICT
key_2013 <- filter(dbP, surveyyear %in% 2013) %>%
  select(HHID, case_id, region, ea_id, district, surveyyear) %>%
  unique() %>% 
  do(filter(., complete.cases(.)))

fert2013 <- rbind(fert2013_1, fert2013_2) %>%
  remove_all_labels() %>%
  left_join(key_2013,.) %>%
  do(filter(., complete.cases(.)))

# Combine both surveyyears, remove composite manure and other, and rename NPK
# there seems to be a lot of other chemicals in the
# MWI survey which are not in our conversion file.
# These need to be sorted into something we can work with
fert <- bind_rows(fert2011, fert2013)
fert$typ <- ifelse(fert$typ %in% c("23:21:0+4", "23:21:0+4S", "23:21:0+4S/CHITOWE"),
               "NPK (MWI)", fert$typ)
fert$typ <- ifelse(fert$typ %in% c("D.COMPOUND", "D COMPOUND"), "D compound", fert$typ)
fert <- filter(fert, typ %in% c("D COMPOUND", "DAP", "CAN",
                               "UREA", "NPK (MWI)"))

# provide a nitrogen component value for npk and urea
conv <- read.csv(file.path(dataPath,"Other/Fertilizer/Fert_comp.csv")) %>%
  transmute(typ=Fert_type2, n=N_share/100, p=P_share/100)

# join the fertilizer information with the conversion
fert <- left_join(fert, conv) %>%
  mutate(Vfert=valu/qty,
         Qn=qty*n,
         Qp=qty*p,
         price = Vfert/n)

base <- dbP %>% 
  dplyr::select(region, district, surveyyear) %>%
  unique() %>%
  na.omit

# Values are winsored aggregates are presented for at least 5 values
# market  prices
fertmar <- fert %>%
  filter(price > 0) %>%
  group_by(surveyyear) %>%
  mutate(price = winsor2(price))

medianPrice_f <- function(df, level, group, type){
  prices <- df %>% 
    group_by_(.dots = c(group)) %>%
    dplyr::summarize(
      number = sum(!is.na(price)),
      price = median(price, na.rm=T)) %>%
    filter(number>=5) %>%
    mutate(level = level) %>%
    select(-number) 
  #prices <- setNames(prices, c(group, "price", "level")) 
  out <- left_join(base, prices) %>%
    mutate(type = type)
  return(out)
}

fpCountry <- fertmar %>% 
  group_by(surveyyear) %>%
  dplyr::summarize(price = median(price, na.rm=T)) %>%
  mutate(level = "country") 
fpCountry <- left_join(base, fpCountry) %>%
  mutate(type = "Pn")

fpRegion <- medianPrice_f(fertmar, "region", c("surveyyear", "region"), "Pn")
fpDistrict <- medianPrice_f(fertmar, "district", c("surveyyear", "region", "district"), "Pn")
names(fpRegion)[1:2] <- c("REGNAME", "DISNAME")
names(fpDistrict)[1:2] <- c("REGNAME", "DISNAME")
names(fpCountry)[1:2] <- c("REGNAME", "DISNAME")


fertMarPrice <- bind_rows(fpDistrict, fpRegion, fpCountry) %>%
  na.omit %>%
  spread(level, price) %>%
  mutate(regPrice = ifelse(!is.na(district), district, 
                           ifelse(!is.na(region), region, country)),
         source = ifelse(!is.na(district), "district", 
                         ifelse(!is.na(region), "region", "country")), 
         product = "fertilizer") %>%
  select(-country, -region, -district)

rm(fpCountry, fpRegion, fpDistrict, fertmar)

# Maize prices
maize <- dbP %>% 
  dplyr::select(region, district, surveyyear, price = crop_price) %>%
  mutate(price = winsor2(price))

fpCountry <- maize %>%
  group_by(surveyyear) %>%
  dplyr::summarize(price = median(price, na.rm=T)) %>%
  mutate(level = "country")
fpCountry <- left_join(base, fpCountry) %>%
  mutate(type = "Pc")

fpRegion <- medianPrice_f(maize, "region", c("surveyyear", "region"), "Pc")
fpDistrict <- medianPrice_f(maize, "district", c("surveyyear", "region", "district"), "Pc")
names(fpRegion)[1:2] <- c("REGNAME", "DISNAME")
names(fpDistrict)[1:2] <- c("REGNAME", "DISNAME")
names(fpCountry)[1:2] <- c("REGNAME", "DISNAME")

maizePrice <- bind_rows(fpDistrict, fpRegion, fpCountry) %>%
  na.omit %>%
  spread(level, price) %>%
  mutate(regPrice = ifelse(!is.na(district), district, 
                           ifelse(!is.na(region), country)),
         source = ifelse(!is.na(district), "district", 
                         ifelse(!is.na(region), "region", "country")),
         product = "maize") %>%
  select(-country, -region, -district)

rm(fpCountry, fpRegion, fpDistrict)

# Combine fert price data files - need to finish 
# this part, somehow need to get fert infotmation
# into main database, before panel
regPrice <- bind_rows(fertMarPrice, maizePrice) %>% ungroup

# Create price file at plot level.
# Again, we winsor the prices for each type of price and per surveyyear
plotPrice <- select(dbP, hhid, plotid, ZONE, REGNAME, DISNAME, surveyyear, Pn = WPn, Pc = crop_price) %>%
  gather(type, plotPrice, Pn, Pc) %>%
  mutate(plotPrice = ifelse(plotPrice == 0, NA, plotPrice)) %>% # remove one value with zero price
  group_by(type, surveyyear) %>%
  mutate(plotPrice =winsor2(plotPrice)) %>%
  ungroup() %>% unique
