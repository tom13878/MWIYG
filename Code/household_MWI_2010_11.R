# -------------------------------------
# Household Information MWI 2010 - 11
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


HH10_11 <- read_dta(file.path(dataPath, "Household/HH_MOD_B.dta")) %>%
  select(HHID, case_id, indidy1 = PID, status=hh_b04, sex=hh_b03,
         yob=hh_b06b, age=hh_b05a, hh_b11, years=hh_b12)

HH10_11$years <- as.numeric(HH10_11$years)
HH10_11$years <- ifelse(HH10_11$hh_b11 %in% 1, HH10_11$age, HH10_11$years)
HH10_11$status <- as_factor(HH10_11$status)
HH10_11$sex <- toupper(as_factor(HH10_11$sex))
HH10_11$yob <- as.integer(HH10_11$yob)
HH10_11$hh_b11 <- NULL

# make a new variable cage (cut age = cage) which splits
# individuals according to their age group with
# breaks at 15, 55 and the max age

HH10_11$cage <- cut(HH10_11$age, breaks = c(0, 15, 55, max(HH10_11$age, na.rm=TRUE)),
                 labels=c("0-15", "16-55", "56+"), include.lowest = TRUE, right = TRUE)

# education of household members and sum
# of education of all household members
# between the ages of 15 and 55

education <- read_dta(file.path(dataPath, "Household/HH_MOD_C.dta")) %>%
  select(HHID, case_id, indidy1=PID, education_any=hh_c06, start=hh_c10,
         end=hh_c15, atSchool=hh_c13)

education$education_any <- as_factor(education$education_any) # ever went to school
education$end <- as.integer(as.character(education$end))
education$end <- ifelse(education$atSchool == 1, 2010, education$end)

# join with HH10_11 dataframe
HH10_11 <- left_join(HH10_11, education)
HH10_11$education <- HH10_11$end - (HH10_11$yob + HH10_11$start)
HH10_11$education <- ifelse(HH10_11$education_any %in% "No", 0, HH10_11$education)
HH10_11 <- select(HH10_11, -start, -end, -yob, -atSchool)

# remove negative years of education (56 obs)
HH10_11$education <- ifelse(HH10_11$education < 0, NA, HH10_11$education)

# summarise the data: get sum of education
# of household members 15-55 and number of
# household members 15:55

HH10_11_x <- group_by(HH10_11, HHID, case_id) %>%
  summarise(education1555=sum(education[cage %in% "16-55"], na.rm=T),
            N1555=sum(cage %in% "16-55"))
HH10_11 <- left_join(HH10_11, HH10_11_x); rm(HH10_11_x)

# select only the head of the household
HH10_11 <- filter(HH10_11, status == "Head") %>% select(-status, -cage)

# -------------------------------------
# death in the family
# -------------------------------------

death <- read_dta(file.path(dataPath, "Household/HH_MOD_W.dta")) %>%
  select(HHID, case_id, hh_w01) %>% group_by(HHID, case_id) %>%
  summarise(death = ifelse(any(hh_w01 %in% 1), 1, 0))

HH10_11 <- left_join(HH10_11, death)

# take out trash
rm(dataPath, education, death)
