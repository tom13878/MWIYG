# -------------------------------------
# script for linking the MWI waves
# into a panel. This is a little bit
# tricky because different household
# identifiers are used each year.
#
# IHS3 Third Integrated Household Survey 2010-2011
# IHPS Integrated Household Panel Survey 2013
#
# We need to make the household identifier
# see the code below 
# -------------------------------------


# try to replicate the attrition statistics in section 1.0 of 
# survey



# following file at the individual level
# explains if someone was meant to be
# followed or not
# setwd("C:/Users/Tomas/Documents/LEI/data/MWI/2013/Data")
# test <- read_dta("IHPSMemberDatabase.dta")

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
source(file.path(filePath, "location_MWI_2013R.r"))

# join in each year first
# wave 1
cross_section10_11 <- left_join(unique(location10_11), unique(HH10_11))
cross_section10_11 <- left_join(cross_section10_11, unique(oput_2010_11))
cross_section10_11 <- left_join(cross_section10_11, unique(plotRS_2010_11))
cross_section10_11 <- left_join(cross_section10_11, unique(areas10_11))
cross_section10_11 <- left_join(cross_section10_11, unique(lab2010_11))

cross_section10_11$year <- 2011

# wave 2
cross_section13 <- left_join(unique(location13), unique(HH13))
cross_section13 <- left_join(cross_section13, unique(oput2013))
cross_section13 <- left_join(cross_section13, unique(plotRS_2013))
cross_section13 <- left_join(cross_section13, unique(areas13))
cross_section13 <- left_join(cross_section13, unique(lab2013))
cross_section13$year <- 2013


# 
keep <- intersect(names(cross_section10_11), names(cross_section13))
panel <- rbind(cross_section10_11[, keep], cross_section13[, keep])
panel$plotnum <- NULL
panel <- select(panel, HHID, case_id, ea_id, year, everything())
