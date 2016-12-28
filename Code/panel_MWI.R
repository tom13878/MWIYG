# -------------------------------------
# script for linking the MWI waves
# into a panel. This is a little bit
# tricky because different household
# identifiers are used each year.
#
# IHS3 Third Integrated Household Survey 2010-2011
# IHPS Integrated Household Panel Survey 2013
#
# Output:
#       1. full panel - all crops
#       2. maize panel
# -------------------------------------

# load packages
library(dplyr)
library(haven)
library(stringr)

# set working directory
if(Sys.info()["user"] == "Tomas"){
  filePath <- "C:/Users/Tomas/Documents/LEI/MWIYG/Code"}

# source all the files that will be used to build the panel

# output
source(file.path(filePath, "output_MWI_2010_11.R"))
source(file.path(filePath, "output_MWI_2013.R"))

# plot details
source(file.path(filePath, "plot_variables_MWI_2010_11.R"))
source(file.path(filePath, "plot_variables_MWI_2013.R"))

# areas
source(file.path(filePath, "plot_areas_MWI_2010_11.R"))
source(file.path(filePath, "plot_areas_MWI_2013.R"))

# labour variables
source(file.path(filePath, "labour_MWI_2010_11.R"))
source(file.path(filePath, "labour_MWI_2013.R"))

# household information
source(file.path(filePath, "household_MWI_2010_11.r"))
source(file.path(filePath, "household_MWI_2013.r"))

# location variables
source(file.path(filePath, "location_MWI_2010_11.r"))
source(file.path(filePath, "location_MWI_2013.r"))

# fertilizer price variables
source(file.path(filePath, "fert_prices_MWI_2010_11.r"))
source(file.path(filePath, "fert_prices_MWI_2013.r"))


# -------------------------------------
# join in each year first to create
# cross sections
# -------------------------------------

# wave 1
cross_section10_11 <- left_join(unique(location10_11), unique(HH10_11))
cross_section10_11 <- left_join(cross_section10_11, unique(fert_prices1011))
cross_section10_11 <- left_join(cross_section10_11, unique(oput_2010_11))
cross_section10_11 <- left_join(cross_section10_11, unique(plotRS_2010_11))
cross_section10_11 <- left_join(cross_section10_11, unique(areas10_11))
cross_section10_11 <- left_join(cross_section10_11, unique(lab2010_11))
cross_section10_11$surveyyear <- 2011

# wave 2
cross_section13 <- left_join(unique(location13), unique(HH13))
cross_section13 <- left_join(cross_section13, unique(fert_prices13))
cross_section13 <- left_join(cross_section13, unique(oput_2013))
cross_section13 <- left_join(cross_section13, unique(plotRS_2013))
cross_section13 <- left_join(cross_section13, unique(areas13))
cross_section13 <- left_join(cross_section13, unique(lab2013))
cross_section13$surveyyear <- 2013

# -------------------------------------
# link the panel
# -------------------------------------

keep <- intersect(names(cross_section10_11), names(cross_section13))
panel <- rbind(cross_section10_11[, keep], cross_section13[, keep])
panel <- select(panel, HHID, case_id, ea_id, surveyyear, plotnum, everything())

# -------------------------------------
# maize has four crop codes
#
# LOCAL.............1
# COMPOSITE/OPV.....2
# HYBRID............3
# HYBRID RECYCLED...4
# 
# these three may have different properties
# and they confuse the MWI_prices file
# -------------------------------------

# select maize plots
maize <- filter(panel, crop_code %in% c(1, 2, 3, 4))

# create new variables for maize type
maize$maize_type <- factor(maize$crop_code)
levels(maize$maize_type) <- c("local", "composite", "hybrid", "hybrid recycled")

# order variables for convenience
maize <- select(maize, surveyyear, season, region, district,
                HHID, case_id, ea_id, plotnum,
                maize_type, crop_qty_harv, N, crop_price,
                WPn, everything())

# take out the trash
rm(areas10_11, areas13, cross_section10_11, cross_section13,
   filePath, HH10_11, HH13, keep, lab2010_11, lab2013,
   location10_11, location13, oput_2010_11, oput_2013,
   plot_2010_11, plot_2013, plotDS_2010_11, plotDS_2013,
   plotRS_2010_11, plotRS_2013, fert_prices1011, fert_prices13)
