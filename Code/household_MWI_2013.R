# -------------------------------------
# Household Information MWI 2013
# -------------------------------------

# load packages
library(dplyr)
library(haven)
library(tidyr)

# set working directory
if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/MWI/2013/Data"
} else {
  dataPath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/MWI/2013/Data"
}


household_2013 <- read_dta(file.path(dataPath, "Household/HH_MOD_B.dta")) %>%
  select(y2_hhid, indidy2 = PID, status=hh_b04, sex=hh_b03,
         yob=hh_b06b, age=hh_b05a, hh_b11, years=hh_b12)

household_2013$years <- as.numeric(household_2013$years)
household_2013$years <- ifelse(household_2013$hh_b11 %in% 1, household_2013$age, household_2013$years)
household_2013$status <- as_factor(household_2013$status)
household_2013$sex <- toupper(as_factor(household_2013$sex))
household_2013$yob <- as.integer(household_2013$yob)
household_2013$hh_b11 <- NULL

# make a new variable cage (cut age = cage) which splits
# individuals according to their age group with
# breaks at 15, 55 and the max age

household_2013$cage <- cut(household_2013$age, breaks = c(0, 15, 55, max(household_2013$age, na.rm=TRUE)),
                    labels=c("0-15", "16-55", "56+"), include.lowest = TRUE, right = TRUE)

# education of household members and sum
# of education of all household members
# between the ages of 15 and 55
# no question 10 on education start date

education <- read_dta(file.path(dataPath, "Household/HH_MOD_C.dta")) %>%
  select(y2_hhid, indidy2=PID, education_any=hh_c06)

education$education_any <- as_factor(education$education_any) # ever went to school

# join with household_2013 dataframe
household_2013 <- left_join(household_2013, education)
household_2013 <- select(household_2013, -yob)

# summarise the data: get sum of education
# of household members 15-55 and number of
# household members 15:55

household_2013_x <- group_by(household_2013, y2_hhid) %>%
  summarise(N1555=sum(cage %in% "16-55"))
household_2013 <- left_join(household_2013, household_2013_x); rm(household_2013_x)

# select only the head of the household
household_2013 <- filter(household_2013, status == "HEAD") %>% select(-status, -cage)

# -------------------------------------
# death in the family No section W in 
# wave 2 questionnaire
# -------------------------------------

# take out trash
rm(dataPath, education)
