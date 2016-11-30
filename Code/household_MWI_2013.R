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


HH13 <- read_dta(file.path(dataPath, "Household/HH_MOD_B.dta")) %>%
  select(y2_hhid, indidy2 = PID, status=hh_b04, sex=hh_b03,
         yob=hh_b06b, age=hh_b05a, hh_b11, years=hh_b12)

HH13$years <- as.numeric(HH13$years)
HH13$years <- ifelse(HH13$hh_b11 %in% 1, HH13$age, HH13$years)
HH13$status <- as_factor(HH13$status)
HH13$sex <- toupper(as_factor(HH13$sex))
HH13$yob <- as.integer(HH13$yob)
HH13$hh_b11 <- NULL

# make a new variable cage (cut age = cage) which splits
# individuals according to their age group with
# breaks at 15, 55 and the max age

HH13$cage <- cut(HH13$age, breaks = c(0, 15, 55, max(HH13$age, na.rm=TRUE)),
                    labels=c("0-15", "16-55", "56+"), include.lowest = TRUE, right = TRUE)

# education of household members and sum
# of education of all household members
# between the ages of 15 and 55
# no question 10 on education start date

education <- read_dta(file.path(dataPath, "Household/HH_MOD_C.dta")) %>%
  select(y2_hhid, indidy2=PID, education_any=hh_c06)

education$education_any <- as_factor(education$education_any) # ever went to school

# join with HH13 dataframe
HH13 <- left_join(HH13, education)
HH13 <- select(HH13, -yob)

# summarise the data: get sum of education
# of household members 15-55 and number of
# household members 15:55

HH13_x <- group_by(HH13, y2_hhid) %>%
  summarise(N1555=sum(cage %in% "16-55"))
HH13 <- left_join(HH13, HH13_x); rm(HH13_x)

# select only the head of the household
HH13 <- filter(HH13, status == "HEAD") %>% select(-status, -cage)

# -------------------------------------
# death in the family No section W in 
# wave 2 questionnaire
# -------------------------------------

# take out trash
rm(dataPath, education)
