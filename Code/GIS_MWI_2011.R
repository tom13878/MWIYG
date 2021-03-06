# PROJECT: IPOP/CIMMYT/DFID
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# R code to link spatial data with LSMS-ISA MWI Household data
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 

### INSTALL PACKAGES AND SET WORKING DIRECTORY
BasePackages<- c("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "haven")
lapply(BasePackages, library, character.only = TRUE)
SpatialPackages <- c("rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <- c("GSIF", "SPEI", "haven", "assertive", "countrycode", "sjmisc")
lapply(AdditionalPackages, library, character.only = TRUE)

### OPTIONS
options(scipen = 999) # surpress scientific notation
options("stringsAsFactors" = FALSE) 
options(digits = 4)

### SET WORKING DIRECTORY
dataPath <- "C:\\Users\\vandijkm\\OneDrive - IIASA\\SurveyData\\MWI\\2010\\Data"
GISPath <- "C:\\Users\\vandijkm\\DATA"
setwd(dataPath)


### SET COUNTRY AND YEAR
iso3c <- "MWI"
surveyYear <- 2010

### FUNCTIONS

# Download Basemap
country.map <- readRDS(file.path(dataPath, "/../../../Other/Spatial/MWI/GADM_2.8_MWI_adm2.rds"))
projection(country.map)

### PREPARE LSMS SPATIAL DATAPOINTS
# Get y1_hhid-GIS link
HH.geo <- read_dta(file.path(dataPath, "Geovariables\\HouseholdGeovariables_IHS3_Rerelease.dta")) %>%
            rename(lat = lat_modified, lon = lon_modified, eaid = ea_id) 
  
plot.geo <- read_dta(file.path(dataPath, "Geovariables\\PlotGeovariables_IHS3_Rerelease.dta")) %>%
  dplyr::select(case_id, HHID, dist_hh, plot_id) # NB we do not select ea_id as they seem to be wrong => join excludes many plots

# Create list of coordinates
geo.base <- HH.geo %>% dplyr::select(lat, lon, eaid) %>% 
                        unique() %>%
                        do(filter(., complete.cases(.))) %>%
                        as.data.frame()

                        

# Create list of plot variables
geo.plot <- plot.geo %>%
  remove_all_labels

# create list of HH level variables
geo.hh <- HH.geo %>%
  remove_all_labels

# Create spatial points 
standardproj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
geo.coord <- geo.base %>% 
              dplyr::select(lon, lat) %>%
              SpatialPoints(., proj4string=CRS(projection(country.map)))

# REGION INFORMATION
geo.region  <- over(geo.coord, country.map) %>%
  cbind(geo.base,.) %>%
  dplyr::select(-ID_0, -ISO, -NAME_0, -NL_NAME_2, -VARNAME_2, -TYPE_2, -ENGTYPE_2, - HASC_2, CCN_2, CCA_2)  


# MONTHLY RAINFALL DATA
montlyRainfallPath <- file.path(GISPath, "CRU_TS_3.22\\Processed")
pet <-brick(paste(montlyRainfallPath, "MonthlyEvapotranspiration190101_201312", sep="\\"))
pre <-brick(paste(montlyRainfallPath, "MonthlyPrecipitation190101_201312", sep="\\"))

# extract data
geo.pet <- raster::extract(pet, geo.coord) %>%
  cbind(geo.base,.) %>% 
  gather(date, pet, X1901.01.16:X2013.12.16)

geo.pre <- raster::extract(pre, geo.coord) %>%
  cbind(geo.base,.) %>% 
  gather(date, pre, X1901.01.16:X2013.12.16)

# Calculate SPEI
# CHECK: http://www.fews.net/east-africa/ethiopia
# http://www.fao.org/giews/countrybrief/country.jsp?code=ETH

# Ehtiopa has two rain seasons; short (Belg) and long(Meher). 
# The LSMS only seems to cover the Meher season, which is most relevant for Maize (FAO and FEWS).
# We calculate the SPEI for the five months May-including September.
# 
# # Funtion to calculate SPEI
# spei.f <- function(data, speiperiod, sd, ed){
#   spei.calc <- spei(data$speicalc, speiperiod)
#   spei.calc <- spei.calc$fitted
#   t <- seq(ymd(sd),ymd(ed), by='months')
#   spei.df <- data.frame(speicalc=as.matrix(spei.calc), date= t) %>%
#     mutate(month = month(date, label=TRUE),
#            year = year(date))
#   return(spei.df)
# }
# 
# # Period over which SPEI is calculated corresponding with five month growth period
# speiendmonth <- "Sep"
# speiperiod <- 5
# gsmonths <- c("May", "Jun", "Jul", "Aug", "Sep")
# 
# # Period for which SPEI is estimated. To be increased with more historical data
# sd <- "1901-01-01"
# ed <- "2013-12-01"
# 
# # Calculate SPEI
# geo.spei <- left_join(geo.pre, geo.pet) %>%
#   do(filter(., complete.cases(.)))
# 
# # Add date information. Lubridate used to crash in R because of a problem in dplyr. This is solved by using the development version.
# # I use base code here.
# library(lubridate)
# geo.spei <- geo.spei %>%  mutate(date = gsub("X","", date))
# geo.spei$date <- as.Date(geo.spei$date, "%Y.%m.%d")
# geo.spei$year <- format(geo.spei$date, "%Y")
# geo.spei$month <- format(geo.spei$date, "%b")
# geo.spei$days_in_month = days_in_month(geo.spei$date)
# geo.spei <- geo.spei %>%
#   mutate(petmonth =  pet*days_in_month,
#          speicalc = pre-petmonth) %>%
#   arrange(eaid, year, month) %>%
#   do(filter(., complete.cases(.)))%>%
#   ddply(.,.(eaid, lat, lon), spei.f, speiperiod, sd, ed) %>%
#   filter(month == speiendmonth & year == surveyYear) %>%
#   dplyr::select(-month, - year, -date)
# names(geo.spei)[4] <- "SPEI" 
# 
# # Total rainfall in growing season
# # Add date information. Lubridate used to crash in R because of a problem in dplyr. This is solved by using the development version.
# # I use base code here.
# geo.monthlyrainfall <- geo.pre %>%  mutate(date = gsub("X","", date))
# geo.monthlyrainfall$date <- as.Date(geo.monthlyrainfall$date, "%Y.%m.%d")
# geo.monthlyrainfall$year <- format(geo.monthlyrainfall$date, "%Y")
# geo.monthlyrainfall$month <- format(geo.monthlyrainfall$date, "%b")
# geo.monthlyrainfall <- geo.monthlyrainfall %>%
#   filter(month %in% gsmonths) %>%
#   group_by(eaid, lat, lon, year) %>%
#   summarize(gsRainfall = sum(pre, na.rm=T)) %>%
#   filter(year==surveyYear) %>%
#   dplyr::select(-year)


# SOIL DATA
# Extract soil data and link to GIS coordinates of plots/housholds
soilPath <- file.path(GISPath, paste("AFSIS\\Processed",iso3c, sep="\\"))
setwd(soilPath)
countryFiles1km <- list.files(pattern = "\\__1km.tif$")
soil <-stack(countryFiles1km)

# ROOT DEPTH
RootDepthPath <- soilPath
setwd(RootDepthPath)
RootDepth <-raster(paste(iso3c, "_", "af_agg_ERZD_TAWCpF23mm__M_1km.tif", sep=""))

# extract data
geo.rootdepth <- raster::extract(RootDepth, geo.coord) %>%
  cbind(geo.base,.) %>%
  do(filter(., complete.cases(.))) # NA values for some regions removed
names(geo.rootdepth)[4] <- "RootDepth"

# CARBON STOCK AND PH
# extract data
geo.soil <- raster::extract(soil, geo.coord) %>% 
  cbind(geo.base,.) %>%
  gather(property, value, -eaid, - lat, -lon) %>% 
  separate(property, c("iso3c", "af", "Property", "T", "M1", "M2", "sd", "x", "grid"), sep = "_") %>%
  dplyr::select(-af, -T, -M1, -M2, -x, -grid)

# Prepare final soil variables
# Carbon stock (kg/m2) for a depth of 0-30 cm (sd1-sd3) and 0-100 cm (sd1-sd5)
# http://www.eoearth.org/view/article/156087/ for refs

# Add standard column depths
# Six standard depths following the Global-
# SoilMap.net specifications: sd1 = 2.5 cm (0-5), sd2 = 10 cm (5-15), sd3 = 22.5 cm (15-30), sd4
# = 45 cm (30-60), sd5 = 80 cm (60-100), sd6 = 150 cm (100-200). (p. 34 package documentation)
sds <- get("stsize", envir = GSIF.opts)
soilsize <- data.frame(sd = c("sd1", "sd2", "sd3", "sd4", "sd5", "sd6"), sds = sds)

# SOC sd1-sd3
geo.socsd1_sd3 <- geo.soil %>% 
  filter(Property %in% c("ORCDRC", "CRFVOL", "BLD") & sd %in% c("sd1", "sd2", "sd3")) %>%
  arrange(lat, lon, Property) %>%
  left_join(., soilsize) %>%
  spread(Property, value) %>%
  do(filter(., complete.cases(.))) %>% # NA values for some regions removed
  mutate(SOC = as.numeric(OCSKGM(ORCDRC, BLD, CRFVOL, sds*100))) %>%
  group_by(eaid, lat, lon) %>%
  summarize(SOC_sd1_sd3 = sum(SOC, na.rm=FALSE))

# SOC sd1-sd5
geo.socsd1_sd5 <- geo.soil %>% 
  filter(Property %in% c("ORCDRC", "CRFVOL", "BLD") & sd %in% c("sd1", "sd2", "sd3", "sd4", "sd5")) %>%
  arrange(lat, lon, Property) %>%
  left_join(., soilsize) %>%
  spread(Property, value) %>%
  do(filter(., complete.cases(.))) %>% # NA values for some regions removed
  mutate(SOC = as.numeric(OCSKGM(ORCDRC, BLD, CRFVOL, sds*100))) %>%
  group_by(eaid, lat, lon) %>%
  summarize(SOC_sd1_sd5 = sum(SOC, na.rm=FALSE))

geo.soc <- left_join(geo.socsd1_sd3, geo.socsd1_sd5)

# Ph sd1_sd3
geo.phsd1_sd3 <- geo.soil %>% 
  filter(Property %in% c("PHIHOX") & sd %in% c("sd1", "sd2", "sd3")) %>%
  arrange(lat, lon, Property) %>%
  left_join(., soilsize) %>%
  do(filter(., complete.cases(.))) %>% # NA values for some regions removed
  group_by(eaid, lat, lon)  %>%
  summarize(ph_sd1_sd3 = sum(value*sds)/sum(sds)) # Weighted average
  
# Ph sd1_sd5
geo.phsd1_sd5 <- geo.soil %>% 
  filter(Property %in% c("PHIHOX") & sd %in% c("sd1", "sd2", "sd3", "sd4", "sd5")) %>%
  arrange(lat, lon, Property) %>%
  left_join(., soilsize) %>%
  do(filter(., complete.cases(.))) %>% # NA values for some regions removed
  group_by(eaid, lat, lon)  %>%
  summarize(ph_sd1_sd5 = sum(value*sds)/sum(sds)) # Weighted average

geo.ph <- left_join(geo.phsd1_sd3, geo.phsd1_sd5)

# GYGA DATA is not available, we use GAEZ data to determine yield potential fir maize
GAEZpath <- file.path(GISPath, "GAEZ")

# # Get GYGA map
maize_yld <- raster(file.path(GISPath, "GAEZ/act2000rmze2000yld_package/act2000_r_mze_2000_yld.tif"))
maize_gap <- raster(file.path(GISPath, "GAEZ/gap2000rmze2000qga_package/gap2000_r_mze_2000_qga.tif"))

# Combine to calculate yield potential
GAEZ <- maize_yld + maize_gap

# Show data
MWI.GAEZ <- raster::crop(GAEZ, country.map)
plot(MWI.GAEZ)
plot(country.map, add = T)
plot(geo.coord, add = T, pch = 18, col="red")

# extract data
geo.GAEZ <- raster::extract(GAEZ, geo.coord) %>%
  cbind(geo.base,.) %>%
  do(filter(., complete.cases(.))) # NA values for some regions removed
names(geo.GAEZ)[4] <- "YW"
geo.GAEZ$YW[geo.GAEZ$YW==0] <- NA # set 0 values to NA

# # FARMING SYSTEMS 
# # http://rpackages.ianhowson.com/rforge/raster/man/factor.html on factor values in raster
# # http://oscarperpinan.github.io/rastervis/ for plotting raster
# fsPath <- "D:\\Data\\IPOP\\FarmingSystems\\fs_2012_tx--ssa.tif_5"
# fs <-raster(file.path(fsPath, "FS_2012_TX--SSA.tif"))
#  
#  # plot maps
# p <- crop (fs, country.map)
# levelplot(p) +layer(sp.polygons(country.map, col='black')) 
#  
# # Extract factor values
# geo.fs <- raster::extract(fs, geo.coord)
# fs_val <- factorValues(fs, geo.fs)
#  
# # Extract data, remove numbering and turn into factor
# geo.fs <- raster::extract(fs, geo.coord) %>% 
#  cbind(geo.base,.) %>%
#  cbind(., fs_val) %>%
#  mutate(fs = str_sub(category, 4),
#         fs = factor(trimws(fs))) %>%
#  dplyr::select(-.,-category)
 
# BIND ALL SPATIAL INFORMATION
# rename eaid to eaid2 again
geo.total <-  left_join(geo.region, geo.rootdepth) %>%
#   left_join(., geo.spei) %>%
   left_join(., geo.ph) %>%
   left_join(., geo.soc) %>%
   left_join(., geo.GAEZ) %>%
#   left_join(., geo.fs) %>%
   rename(ea_id = eaid) 

 
# check missing values (not including GYGA data for which we know data is missing)
geo.check <- geo.total %>% dplyr::select(-CCA_2, -CCN_2) %>% do(filter(., !complete.cases(.))) 
geo.check.plot <- geo.check %>% 
   dplyr::select(lon, lat) %>%
   SpatialPoints(., proj4string=CRS(standardproj))

# Data is missing for two locations, which appear to be on the border but due to the offset are now located outside the country.
# Try to see if it is possible to move points back into ETH using shortest distance measurement.
 
# # Plot missing values in GoogleMaps
# # Does NOT work in explorer => set default browser to Firefox
#library(plotGoogleMaps)
#geo.check.google <- spTransform(geo.check.plot, CRS('+init=epsg:28992'))
#m <- plotGoogleMaps(geo.check.google)
 
# Plot missing values on map
#plot(crop(pet[[1356]], country.map))
plot(country.map)
points(geo.check.plot, pch = 18, col="red")
 
# MERGE LSMS AND GEO DATA
 
# Merge plot and household level data
geo.total.plot <- left_join(geo.hh, geo.plot) %>% 
 left_join(., geo.total)

# Rename and recode 
AEZ_code <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other\\Spatial\\Other\\AEZ_code.csv"))

geo.total.plot <- geo.total.plot %>%
 transmute(
   lat,
   lon,
   dist_hh = dist_hh,
   dist_road = dist_road,
   dist_popcenter = dist_popcenter,
   #dist_market = dist_agmrkt,
   dist_admarc = dist_admarc,
   dist_borderpost = dist_borderpost,
   dist_regcap = dist_boma,
   AEZ = ssa_aez09,
   elevation = srtm_eaf,
   slope = afmnslp_pct,
   twi = twi_mwi,
   nutr_av = sq1,
   rain_year = h2010_tot,
   rain_wq = h2010_wetq,
   rain_year_avg = anntot_avg,
   rain_wq_avg = wetq_avg,
   plot_id, 
   case_id,
   HHID,
   region_name=NAME_1, district_name=NAME_2,
   ph=ph_sd1_sd3, ph2=ph_sd1_sd5,
   SOC=SOC_sd1_sd3, SOC2=SOC_sd1_sd5, 
   #rain_CRU=gsRainfall,
   #SPEI,
   #RootDepth,
   #fs, 
   YW
   ) %>%
 mutate(AEZ = droplevels(factor(AEZ,
                          levels = AEZ_code$code,
                                  labels = AEZ_code$AEZ)))
 
 # Write file
 saveRDS(geo.total.plot, file = "H:\\MyDocuments\\Github\\MWIYG\\Cache\\MWI_geo_2010.rds")
 