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


# # household variables
# source(file.path(filePath, "household_MWI_2010_11.r"))
# source(file.path(filePath, "household_MWI_2013.r"))
# 
# # testing to see if the panel is joined properly
# HH13$y2_hhid <- sapply(strsplit(HH13$y2_hhid, "-"), function(i) i[1])
# HH10_11$y1_hhid <- str_pad(HH10_11$HHID, 4, pad = "0")
# table(HH13$y2_hhid %in% HH10_11$y1_hhid)
# table(HH10_11$y1_hhid %in% HH13$y2_hhid)
# 
# names(HH10_11)
# names(HH13)
# 
# HH10_11 <- rename(HH10_11, hhid = y1_hhid)
# HH13 <- rename(HH13, hhid = y2_hhid)
# 
# HH10_11$indidy1 <- HH10_11$HHID <- NULL
# HH13$indidy2 <- NULL
# 
# keep <- intersect(names(HH10_11), names(HH13))
# 
# HH10_11 <- HH10_11[, keep]
# HH13 <- HH13[, keep]
# names(HH13)[-1] <- paste0(names(HH13)[-1], ".2")
# 
# test <- full_join(HH10_11, HH13)
# 
# table(test$sex == test$sex.2 ) # seems likely the same
# 
# # ages tend to be three apart

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

