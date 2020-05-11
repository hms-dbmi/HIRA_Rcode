##########################
# Libraries installation #
##########################
packages <- c("readxl","dplyr","tidyverse","tidyr","ggplot2", "plyr", "lubridate")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
library(readxl)

library(tidyverse)
library(lubridate)


######  LOADING DATA + DATA MANAGEMENT
##########
#corona claim data
# Corona claim data
co19_t200_trans_dn = read_excel("./HIRA COVID-19 Sample Data_20200325.xlsx", sheet=2)
co19_t300_trans_dn = read_excel("./HIRA COVID-19 Sample Data_20200325.xlsx", sheet=3)
co19_t400_trans_dn = read_excel("./HIRA COVID-19 Sample Data_20200325.xlsx", sheet=4)
co19_t530_trans_dn = read_excel("./HIRA COVID-19 Sample Data_20200325.xlsx", sheet=5)

##### ONLY FOR SAMPLE DATA: adding severe ICD code in the data
co19_t200_trans_dn[co19_t200_trans_dn$MAIN_SICK == "J029", "MAIN_SICK"] <- "J80"
co19_t200_trans_dn[co19_t200_trans_dn$SUB_SICK == "J029", "SUB_SICK"] <- "J80"


# medical use history data
co19_t200_twjhe_dn = read_excel("HIRA COVID-19 Sample Data_20200325.xlsx", sheet=6)
co19_t530_twjhe_dn = read_excel("HIRA COVID-19 Sample Data_20200325.xlsx", sheet=9)

## ONLY FOR SAMPLE DATA: data management for sample medical history, to have overlapping MID
co19_t200_twjhe_dn$MID <- sample(co19_t200_trans_dn$MID,
                                 length(co19_t200_twjhe_dn$MID),
                                 replace=T)
co19_t530_twjhe_dn$MID <- sample(co19_t200_trans_dn$MID,
                                 length(co19_t530_twjhe_dn$MID),
                                 replace=T)

# Convert dates variables to date type
co19_t200_trans_dn[c("RECU_FR_DD", "RECU_TO_DD")] <-
  lapply(co19_t200_trans_dn[c("RECU_FR_DD", "RECU_TO_DD")], as_date)

## List of tracked drugs 4CE + mapping to Korean codes
drugs_severe_mapping <- read.csv("severe_mapping_fake.csv", row.names=NULL, stringsAsFactors = F)[c("Name", "ATC.code", "GNL_CD")]
drugs_mapping <- read.csv("drugs_mapping_fake.csv", row.names=NULL, stringsAsFactors = F)[c("Name", "ATC.code", "GNL_CD")]

## ICD codes mapping
korean_icd <- read_excel("Korean_Codes/MAIN_SICK_KCD-7.xlsx", sheet=1)
severe_icd <- read_excel("./4CE Phase 1.1 _Tracked and Severe Meds - Tracked Meds_Labs_Codes Severe_DM.xlsx", col_names=T, sheet=2)
names(severe_icd) <- c("MAIN_SICK", "Description_4CE")
severe_icd <- severe_icd[-1,]
korean_severe_icd <- left_join(severe_icd, korean_icd, by="MAIN_SICK")



######### 4CE POPULATION SELECTION
#############
# Looking fot the 4CE aligment date (for a given MID, earliest entry date for corona related claim)
first_visit_MID <- co19_t200_trans_dn[c("MID", "RECU_FR_DD")] %>%
  group_by(MID) %>%
  slice(which.min(RECU_FR_DD))
names(first_visit_MID)[which(names(first_visit_MID) == "RECU_FR_DD")] <- "alignment_date"

## Here is the base population, with a column "aligment date" added
population <- left_join(co19_t200_trans_dn, first_visit_MID, by="MID")

######## 4CE SEVERITY ASSESSMENT
############
long_icd_corona <- population %>% pivot_longer(cols = c("MAIN_SICK", "SUB_SICK"),
                                                  values_to = "ICD_KOREA",
                                                  names_to = "CODE_TYPE")
MID_severe_icd <- long_icd_corona[long_icd_corona$ICD_KOREA %in% korean_severe_icd$MAIN_SICK, "MID"] %>%
  unique() %>% unlist()
MID_severe_drugs <- co19_t530_trans_dn[co19_t530_trans_dn$GNL_CD %in% drugs_severe_mapping$GNL_CD, "MID"] %>%
  unique() %>% unlist()
MID_severe <- union(MID_severe_icd, MID_severe_drugs)
severe_patients <- co19_t200_trans_dn[co19_t200_trans_dn$MID %in% MID_severe, c("MID", "RECU_FR_DD")] %>%
  group_by(MID) %>%
  summarize(min(RECU_FR_DD))
names(severe_patients)[which(names(severe_patients) == "min(RECU_FR_DD)")] <- "date_severe"
severe_patients$is_severe <- TRUE

# Adding information about patient severity
population <- left_join(population, severe_patients, by="MID")
population$is_severe <- replace_na(population$is_severe, FALSE)

######### 1. DailyCounts
## DailyCounts
population$los_visit_day <- population$RECU_TO_DD - population$RECU_FR_DD

# Prevent absurd dates
first_calendar_day <- if_else(min(population$RECU_FR_DD) < as_date("2019-12-31"),
                              as_date("2019-12-31"),
                              min(population$RECU_FR_DD, na.rm = T))
last_calendar_day <- if_else(max(population$RECU_TO_DD) > lubridate::today(tz="UTC"),
                             lubridate::today(tz="UTC"),
                             max(population$RECU_TO_DD, na.rm = T))
max_los_visit <- as.numeric(max(population$los_visit_day, na.rm = T))

### Number patient on a given calendar date
calendar_days_range <- as_date(first_calendar_day:last_calendar_day)
calendar_day_count <- calendar_day_count_cumulative <- vector(mode="list", length=length(calendar_days_range))
names(calendar_day_count_cumulative) <- names(calendar_day_count) <- as.character(calendar_days_range)
for (n in 1:length(calendar_days_range)) {
  day <- calendar_days_range[[n]]
  population$is_severe_that_day <- population$date_severe < day
  population$is_severe_that_day <- tidyr::replace_na(population$is_severe_that_day, FALSE)
  count_present <- population[(day >= population$RECU_FR_DD) & (day <= population$RECU_TO_DD), c("MID", "is_severe_that_day")] %>%
    unique() %>%
    select(is_severe_that_day) %>%
    table(useNA="no")
  calendar_day_count[[n]] <- as.data.frame(matrix(count_present, nrow=1, ncol=length(count_present), dimnames=list(c(day), names(count_present))))
  count_cumulative <- population[day >= population$RECU_FR_DD, c("MID", "RECU_FR_DD", "is_severe_that_day")] %>%
    unique() %>%
    select(is_severe_that_day) %>%
    table(useNA="no")
  calendar_day_count_cumulative[[n]] <- as.data.frame(matrix(count_cumulative,
                                                             nrow=1, ncol=length(count_cumulative), dimnames=list(c(day), names(count_cumulative))))

}
df_calendar_count <- bind_rows(calendar_day_count, .id="day")
df_calendar_count$num_patients_in_hospital_on_this_date <- apply(df_calendar_count[c("TRUE", "FALSE")],
                                                            1,
                                                            sum,
                                                            na.rm=TRUE)
names(df_calendar_count)[which(names(df_calendar_count)=="TRUE")] <- "num_patients_in_hospital_and_severe_on_this_date"
df_calendar_count[["FALSE"]] <- NULL

df_calendar_count_cumulative <- bind_rows(calendar_day_count_cumulative, .id="day")
df_calendar_count_cumulative$cumulative_patient_all <- apply(df_calendar_count_cumulative[c("TRUE", "FALSE")],
                                                  1,
                                                  sum,
                                                  na.rm=TRUE)
names(df_calendar_count_cumulative)[which(names(df_calendar_count_cumulative)=="TRUE")] <- "cumulative_patient_severe"
df_calendar_count_cumulative[["FALSE"]] <- NULL


######## 2. Clinical Course
#############
# Number of patient relative to admission date
relative_days_range <- 0:max_los_visit
relative_day_count <- vector(mode = "list", length = length(relative_days_range))
names(relative_day_count) <- as.character(relative_days_range)
for (n in 1:length(relative_days_range)) {
  day <- relative_days_range[[n]]
  count <- population[day <= population$los_visit_day, c("MID", "is_severe")] %>%
    unique() %>%
    select(is_severe) %>%
    table(useNA="no")
  relative_day_count[[n]] <- as.data.frame(matrix(count, nrow=1, ncol=length(count), dimnames=list(c(day), names(count))))
}
df_relative_count <- bind_rows(relative_day_count, .id="day")
df_relative_count$num_patients_all_still_in_hospital <- apply(df_relative_count[c("TRUE", "FALSE")],
                                                            1,
                                                            sum,
                                                            na.rm=TRUE)
names(df_relative_count)[which(names(df_relative_count)=="TRUE")] <- "num_patients_ever_severe_still_in_hospital"
df_relative_count[["FALSE"]] <- NULL


####### 3. Demographics
###########
population$age_cat <- cut(population$PAT_AGE,
                          breaks <- c(0,3,6,12,18,26,50,70,80,150),
                          labels <- c("0-2","3-5","6-11","12-17","18-25","26-49","50-69","70-79","80+")
                          )
Demographics <- group_by(population, age_cat, SEX_TP_CD, is_severe) %>%
  count() %>%
  pivot_wider(values_from = n, names_from = is_severe, names_prefix = "is_severe_")



####### 4. Diagnoses
############
## Merge current and historic
long_icd_corona <- left_join(long_icd_corona, population[c("MID", "alignment_date", "is_severe")], by="MID")

long_medics_history <- co19_t200_twjhe_dn %>%
  pivot_longer(cols = c("MAIN_SICK", "SUB_SICK"),
               values_to="ICD_KOREA",
               names_to = "CODE_TYPE") %>%
  left_join(population[c("MID", "alignment_date", "is_severe")], by="MID") %>%
  mutate(RECU_FR_DD = as_date(RECU_FR_DD),
         RECU_TO_DD = as_date(RECU_TO_DD))
long_medics <- bind_rows(long_icd_corona[c("RECU_FR_DD", "RECU_TO_DD", "ICD_KOREA", "alignment_date", "is_severe")],
                         long_medics_history[c("RECU_FR_DD", "RECU_TO_DD", "ICD_KOREA", "alignment_date", "is_severe")]) %>%
  mutate(ICD_KOREA = sub(pattern = "(?<=\\w{3}).*", replacement = "", x = ICD_KOREA, perl=T))

long_medics$before_since <- if_else(long_medics$RECU_FR_DD < long_medics$alignment_date,
                                    "before",
                                    "since")
long_medics$not_within_15d_bf <- (long_medics$RECU_FR_DD < long_medics$alignment_date - 15) |
                                     (long_medics$RECU_FR_DD >= long_medics$alignment_date)
icd_counts <- long_medics %>% subset(subset = long_medics$not_within_15d_bf == TRUE) %>%
  group_by(before_since, is_severe, ICD_KOREA) %>%
  count() %>%
  pivot_wider(names_from = c(before_since, is_severe),
              values_from = n,
              values_fill = list(n = 0)) %>%
  rename(num_patients_all_before_admission = before_FALSE,
         num_patients_all_since_admission = since_FALSE,
         num_patients_ever_severe_before_admission = before_TRUE,
         num_patients_ever_severe_since_admission = since_TRUE,
         icd_code_3chars = ICD_KOREA)

######## 6. Drugs
############
drugs_long <- bind_rows(co19_t530_trans_dn, co19_t530_twjhe_dn) %>%
  left_join(population[c("MID", "is_severe", "alignment_date")], by="MID") %>%
  select(MID, GNL_CD, PRSCP_GRANT_NO) %>%
  subset(GNL_CD %in% drugs_mapping$GNL_CD) %>%
  mutate(prescription_date = as_date(substr(PRSCP_GRANT_NO, 1, 8)),
         prescription_occurence = as.integer(substr(PRSCP_GRANT_NO, 9, 13))) %>%
  left_join(population[c("MID", "alignment_date", "is_severe")], by="MID")
drugs_long$before_since <- if_else(drugs_long$prescription_date < drugs_long$alignment_date,
                                   "before",
                                   "since")
drugs_long$not_within_15d_bf <- (drugs_long$prescription_date < (drugs_long$alignment_date - 15)) |
  (drugs_long$prescription_date >= drugs_long$alignment_date)
drugs_count <- drugs_long %>% subset(not_within_15d_bf) %>%
  group_by(before_since, is_severe, GNL_CD) %>%
  count() %>%
  pivot_wider(names_from = c(before_since, is_severe),
              values_from = n,
              values_fill = list(n = 0)) %>%
  rename(num_patients_all_before_admission = before_FALSE,
         # num_patients_all_since_admission = since_FALSE,
         num_patients_ever_severe_before_admission = before_TRUE,
         #num_patients_ever_severe_since_admission = since_TRUE,
         icd_code_3chars = GNL_CD)



