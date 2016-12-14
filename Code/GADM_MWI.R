# PROJECT: IPOP/CIMMYT/DFID
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# R code to download GADM maps
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
dataPath <- "C:\\Users\\vandijkm\\OneDrive - IIASA\\SurveyData\\Other\\Spatial"
setwd(dataPath)


### SET COUNTRY AND YEAR
iso3c <- "MWI"

### FUNCTIONS

# Download Basemap
basemapPath = paste0(dataPath, "/../../../",  "Other/Spatial/Maps", "/",iso3c)

# Obtain country coordinates for target country
GADM_f <- function(iso3c, lev=0, proj = "+proj=longlat +datum=WGS84"){
  gadm = getData('GADM', country=iso3c, level=lev, path= file.path(dataPath, iso3c))
  # change projection 
  projection <- proj
  country.sp <- spTransform(gadm, CRS(projection))
  return(country.sp)
}

MWI_adm0 <- GADM_f(iso3c, 0)
MWI_adm1 <- GADM_f(iso3c, 1)
MWI_adm2 <- GADM_f(iso3c, 2)
MWI_adm3 <- GADM_f(iso3c, 3)
