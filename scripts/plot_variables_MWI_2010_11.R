# -------------------------------------
#' Plot Variables
#' Malawi wave 1 (2010 - 11)
#' collection dates: March 2010 - March 2011 
#'
#' output: Three dataframes with plot level details
#'  1. plotRS: plot details during rainy season (RS)
#'  2. plotDS: plot details during dry season (DS)
#'  3. plot: combined plot details over RS and DS
#'  
#' External files required:
#'  1. fertilizer composition
# -------------------------------------

# set working directory
if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/MWI/2010_11/Data"
} else {
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/MWI/2010/Data"
}

# load packages
library(haven)
library(dplyr)

# -------------------------------------
#' Rainy season plot details
#' Agricultural questionnaire section D
#' Notes:
#'  1. five possible crops labelled
#'  crop1 to crop5 (maize has crop code = 1)
#'  2.
# -------------------------------------

plotRS <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_D.dta")) %>%
  select(HHID, case_id, ea_id, plotnum = ag_d00,
         crop1 = ag_d20a, crop2 = ag_d20b, crop3 = ag_d20c,
         crop4 = ag_d20d, crop5 = ag_d20e, soil = ag_d21,
         slope_farmer = ag_d26, irrig = ag_d28a,
         fallow = ag_d33, manure = ag_d36,
         pest = ag_d40, pest_type1 = ag_d41a, pest_q1 = ag_d41b,
         pest_unit1 = ag_d41c, pest_type2 = ag_d41d,
         pest_q2 = ag_d41e, pest_unit2 = ag_d41f)

# re-factor soil and slope_farmer variables
plotRS$soil <- factor(plotRS$soil, levels=c(1,2,3,4),
                      labels=c("Sandy", "Loam", "Clay", "Other"))
plotRS$slope_farmer <- factor(plotRS$slope_farmer, levels=c(1,2,3,4),
                            labels=c("Flat", "Slightly sloped",
                                     "Moderately sloped", "Very steep"))

# remove labels from certain variables
plotRS$crop1 <- as.integer(plotRS$crop1)
plotRS$crop2 <- as.integer(plotRS$crop2)
plotRS$crop3 <- as.integer(plotRS$crop3)
plotRS$crop4 <- as.integer(plotRS$crop4)
plotRS$crop5 <- as.integer(plotRS$crop5)
plotRS$irrig <- ifelse(plotRS$irrig %in% 7, 0,
                       ifelse(plotRS$irrig %in% c(1:6), 1, NA))
plotRS$manure <- ifelse(plotRS$manure %in% 1, 1,
                       ifelse(plotRS$manure %in% 2, 0, NA))
plotRS$pest <- ifelse(plotRS$pest %in% 1, 1,
                        ifelse(plotRS$pest %in% 2, 0, NA))

# convert units for pesticide to kg/litres
plotRS$pest_q1 <- ifelse(plotRS$pest_unit1 %in% c(2, 8), plotRS$pest_q1,
                         ifelse(plotRS$pest_unit1 %in% c(1, 9), plotRS$pest_q1*0.001, NA))
plotRS$pest_q2 <- ifelse(plotRS$pest_unit2 %in% c(2, 8), plotRS$pest_q2,
                         ifelse(plotRS$pest_unit2 %in% c(1, 9), plotRS$pest_q2*0.001, NA))



# we no longer need pesticide units.
# also remove the pesticide type as
plotRS$pest_unit1 <- plotRS$pest_unit2 <-
  plotRS$pest_type1 <- plotRS$pest_type2 <-  NULL

# sum pesticide quantity

# inorganic fertilizer
#' there is an option to have used more than one
#' inorganic fertilizer.
fertRS1 <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_D.dta")) %>%
  select(HHID, case_id, ea_id, plotnum = ag_d00, typ = ag_d39a,
         qty = ag_d39d, month = ag_d39e)
fertRS2 <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_D.dta")) %>%
  select(HHID, case_id, ea_id, plotnum = ag_d00, typ=ag_d39f,
         qty=ag_d39i, month = ag_d39j )

fertRS1$typ <- as_factor(fertRS1$typ)
fertRS2$typ <- as_factor(fertRS2$typ)

levels(fertRS1$typ) <- levels(fertRS2$typ) <-
  c("NPK (MWI)", "DAP", "CAN", "UREA", "D compound", "OTHER")

# -------------------------------------
# reorganize data so that observations
# on fertilizer type occupy a single row
# fertilizer is unit of observation
# Data on NPK composition from Sheahan
# et al (2014), Food Policy
# -------------------------------------

conv <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other/Fertilizer/Fert_comp.csv")) %>%
  transmute(typ=Fert_type2, n=N_share/100, p=P_share/100) %>%
  filter(typ %in% levels(fertRS1$typ))

fertRS1 <- left_join(fertRS1, conv)
fertRS2 <- left_join(fertRS2, conv)

# -------------------------------------
# organize fertilizer data for analysis

fertRS <- rbind(fertRS1, fertRS2)

fertRS <- mutate(fertRS,
               Qn=qty*n,
               Qp=qty*p)

fertRS <- group_by(fertRS, case_id, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            P=sum(Qp, na.rm=TRUE))

# -------------------------------------
#' Dry season plot details
# -------------------------------------

plotDS <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_K.dta")) %>%
  select(HHID, case_id, ea_id, plotnum = ag_k0a, title = ag_k04,
         crop1 = ag_k21a, crop2 = ag_k21b, crop3 = ag_k21c,
         crop4 = ag_k21d, crop5 = ag_k21e, soil = ag_k22,
         slope_farmer = ag_k27, irrig = ag_k29a,
         fallow_year = ag_k33, fallow = ag_k34, manure = ag_k37,
         pest = ag_k41, pest_type1 = ag_k42a, pest_q1 = ag_k42b,
         pest_unit1 = ag_k42c, pest_type2 = ag_k42d,
         pest_q2 = ag_k42e, pest_unit2 = ag_k42f)

# inorganic fertilizer
#' there is an option to have used more than one
#' inorganic fertilizer.
fertDS1 <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_K.dta")) %>%
  select(HHID, case_id, ea_id, plotnum = ag_k0a, typ = ag_k40a,
         qty = ag_k40d, month = ag_k40e)
fertDS2 <- read_dta(file.path(dataPath, "Agriculture/AG_MOD_K.dta")) %>%
  select(HHID, case_id, ea_id, plotnum = ag_k0a, typ=ag_k40f,
         qty=ag_k40i, month = ag_k40j )

fertDS1$typ <- as_factor(fertDS1$typ)
fertDS2$typ <- as_factor(fertDS2$typ)

levels(fertDS1$typ) <- levels(fertDS2$typ) <-
  c("NPK (MWI)", "DAP", "CAN", "UREA", "D compound", "OTHER")

fertDS1 <- left_join(fertDS1, conv)
fertDS2 <- left_join(fertDS2, conv)

# -------------------------------------
# organize fertilizer data for analysis

fertDS <- rbind(fertDS1, fertDS2)

fertDS <- mutate(fertDS,
                 Qn=qty*n,
                 Qp=qty*p)

fertDS <- group_by(fertDS, case_id, plotnum) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            P=sum(Qp, na.rm=TRUE))


# -------------------------------------
# join all the plot details information
# together and return a list for the 
# raing season and dry season.
# -------------------------------------

plotRS <- left_join(plotRS, fertRS) %>% mutate(season="RS")
plotDS <- left_join(plotDS, fertDS) %>% mutate(season="DS")
plot <- rbind(plotRS, plotDS)

# take out the trash
rm(dataPath, conv, fertDS, fertDS1, fertDS2,
   fertRS, fertRS1, fertRS2)