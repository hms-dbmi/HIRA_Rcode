##########################
# Libraries installation #
##########################
packages <- c("readxl","dplyr","tidyverse","tidyr","plyr", "lubridate")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
library(readxl)
library(plyr)
library(dplyr)
library(tidyverse)
library(tidyr)
library( lubridate )
library(data.table)

#################
### FUNCTIONS ###
#################

##### when creating the data set we have to consider that the same patient can have multiple admissions
##### and that the same patient can be administered multiple drugs
##### and check that the merge is correct or look for alternatives to create the file, or create several files
createDataSet <- function( since = co19_t200_trans_dn,
                           before = co19_t200_twjhe_dn,
                           medication_before = co19_t530_twjhe_dn,
                           medication_since = co19_t530_trans_dn,
                           hospitalize = FALSE,
                           days_before_offset = 15,
                           days_before = 365){

  # Creating alignment date
  first_visit_PATIENTID <- since[ c("MID", "RECU_FR_DD", "PAT_AGE", "SEX_TP_CD")] %>%
    group_by(MID) %>%
    slice(which.min(RECU_FR_DD)) %>%
    rename(ALIGNMENT_DATE = RECU_FR_DD)

  #variables of interest
  col_t200 <- c( "MID", "MAIN_SICK", "SUB_SICK",
                 "FOM_TP_CD", "RECU_FR_DD", "RECU_TO_DD","DGRSLT_TP_CD",
                 "BEFORE_SINCE")
  since[["BEFORE_SINCE"]] <- "since"
  before[["BEFORE_SINCE"]] <- "before"
  dataAnalysis <- dplyr::bind_rows(since[col_t200], before[col_t200])

  # Adding alignment date + unique patient personal information (Age)
  dataAnalysis <- merge(dataAnalysis, first_visit_PATIENTID, by = "MID")

  #SEX_TP_CD to sex
  sex_tp_cd_map = read_excel("./SEX_TP_CD.xlsx", sheet=1)

  #Map the number for SEX_TP_CD to meaningful text
  dataAnalysis = merge(dataAnalysis, sex_tp_cd_map, by="SEX_TP_CD", all.x=T)

  #Map death info for DGRSLT_TP_CD to meaningful
  dataAnalysis$DEATH <- ifelse( dataAnalysis$DGRSLT_TP_CD==4, "yes", "no")

  #Create a data.frame with meaningful column names and joining main and sub sick under the same column
  dataAnalysisSelection <- data.frame( PATIENT_ID  = dataAnalysis$MID,
                                       SEX = dataAnalysis$SEX,
                                       DEATH = dataAnalysis$DEATH,
                                       AGE = dataAnalysis$PAT_AGE,
                                       DIAGNOSTIC_CODE = c( dataAnalysis$MAIN_SICK, dataAnalysis$SUB_SICK),
                                       CARE_RELEASE_DATE = dataAnalysis$RECU_FR_DD,
                                       CARE_ENDS = dataAnalysis$RECU_TO_DD,
                                       HOSPITALIZATION = dataAnalysis$FOM_TP_CD,
                                       BEFORE_SINCE = dataAnalysis$BEFORE_SINCE,
                                       ALIGNMENT_DATE = dataAnalysis$ALIGNMENT_DATE,
                                       stringsAsFactors = F
  )
  dataAnalysisSelection[c("CARE_RELEASE_DATE", "CARE_ENDS", "ALIGNMENT_DATE")] <-
    lapply(dataAnalysisSelection[c("CARE_RELEASE_DATE", "CARE_ENDS", "ALIGNMENT_DATE")], as_date)

  #Create a new column with the diagnostic code at 3 digits level
  dataAnalysisSelection$DIAGNOSTIC_3D <- substr(dataAnalysisSelection$DIAGNOSTIC_CODE, 1, 3)

  #Select hospitalize patients ONLY
  if( hospitalize == TRUE){
    dataAnalysisSelection <- dataAnalysisSelection[ dataAnalysisSelection$HOSPITALIZATION == "021", ]
    if(nrow( dataAnalysisSelection) == 0){
      message("There is not hospitalize patients in this dataset.")
      stop()
    }

  }

  col_medications <- c("MID", "GNL_CD", "PRSCP_GRANT_NO")
  medication <- dplyr::bind_rows(medication_since[col_medications], medication_since[col_medications])
  #Create a data.frame with the medication data and select only hospitalize patients
  dataAnalysisMedication =  data.frame( PATIENT_ID  = medication$MID,
                                        MEDICATION  = medication$GNL_CD,
                                        MEDICATION_DATE = medication$PRSCP_GRANT_NO)

  #drug mapping,
  gnl_to_4ce = read.delim("./4CE_in_GNL_drug_overlap.tsv")

  #merge both to do selection an analysis based on the ACT medication code
  dataAnalysisMedicationComplete <- merge( dataAnalysisMedication, gnl_to_4ce[c("GNL_CD",
                                                                                "ATC_Code",
                                                                                "Type_for_4CE_Analysis",
                                                                                "Class_Name",
                                                                                "Med_Name_x")],
                                           by.x = "MEDICATION",
                                           by.y = "GNL_CD",
                                           all.x = TRUE )

  # Merging everything into one dataframe
  dataAnalysisSelection <- merge( dataAnalysisSelection,
                                  dataAnalysisMedicationComplete,
                                  by="PATIENT_ID",
                                  all.x = T)

  severePatients <- dataAnalysisSelection[ (dataAnalysisSelection$DIAGNOSTIC_3D %in% c("J80", "J95")) |
                                             (dataAnalysisSelection$Type_for_4CE_Analysis == "Severe Illness Medication"),
                                           c("PATIENT_ID", "CARE_RELEASE_DATE")] %>%
    group_by(PATIENT_ID) %>%
    slice(which.min(CARE_RELEASE_DATE)) %>%
    rename(SEVERE_PATIENT_DATE = CARE_RELEASE_DATE)

  if( nrow(severePatients)!= 0){
    severePatients[["SEVERE_PATIENT"]] <- T
    dataAnalysisSelection <- left_join(dataAnalysisSelection,
                                       severePatients,
                                       by = "PATIENT_ID")
    dataAnalysisSelection[["SEVERE_PATIENT"]] <- replace_na(dataAnalysisSelection[["SEVERE_PATIENT"]], F)
  } else {
    dataAnalysisSelection[["SEVERE_PATIENT"]] <- FALSE
    dataAnalysisSelection[["SEVERE_PATIENT_DATE"]] <- NA
  }

  #estimate the days of difference between care release and end date
  dataAnalysisSelection$DATE_DIFF_IN_DAYS <- as.numeric(dataAnalysisSelection$CARE_ENDS -dataAnalysisSelection$CARE_RELEASE_DATE)

  # 4CE Timeframe window
  dataAnalysisSelection$within_timeframe <-
    (
      (dataAnalysisSelection$CARE_RELEASE_DATE <= dataAnalysisSelection$ALIGNMENT_DATE - days_before_offset) &
        (dataAnalysisSelection$CARE_RELEASE_DATE >= dataAnalysisSelection$ALIGNMENT_DATE - days_before)
    ) |
    (dataAnalysisSelection$CARE_RELEASE_DATE >= dataAnalysisSelection$ALIGNMENT_DATE)
  #  dataAnalysisSelection <- dataAnalysisSelection[dataAnalysisSelection$within_timeframe == TRUE,]

  #add the age breaks as a column
  agebreaks <- c(0,3,6,12,18,26,50,70,80,150)
  agelabels <- c("0-2","3-5","6-11","12-17","18-25","26-49","50-69","70-79","80+")

  data.table::setDT(dataAnalysisSelection)[, AGE_RANGE := cut(AGE,
                                                              breaks = agebreaks,
                                                              right = FALSE,
                                                              labels = agelabels)]
  return( dataAnalysisSelection )
}

demographicsFile <- function( input, by.sex, by.age, severe ){

  selection <- unique( input[ ,c("PATIENT_ID", "AGE_RANGE", "SEX", "SEVERE_PATIENT") ] )

  if( by.sex == TRUE & by.age == TRUE & severe == TRUE ){
    demographicsCount <- rbind(plyr::ddply(selection,
                                           .(AGE_RANGE,SEX, SEVERE_PATIENT),
                                           summarise,COUNT = length(PATIENT_ID)))
  }else if( by.sex == TRUE & by.age == TRUE & severe == FALSE ){
    demographicsCount <- rbind(plyr::ddply(selection,
                                           .(AGE_RANGE,SEX),
                                           summarise,COUNT = length(PATIENT_ID)))
  }else if( by.sex == FALSE & by.age == TRUE & severe == TRUE){

    demographicsCount <- rbind(plyr::ddply(selection,
                                           .(AGE_RANGE, SEVERE_PATIENT),
                                           summarise,COUNT = length(PATIENT_ID)))
  }else if(by.sex == FALSE & by.age == TRUE & severe == FALSE){
    demographicsCount <- rbind(plyr::ddply(selection,
                                           .(AGE_RANGE),
                                           summarise,COUNT = length(PATIENT_ID)))
  }else if( by.sex == TRUE & by.age == FALSE & severe == TRUE){
    demographicsCount <- rbind(plyr::ddply(selection,
                                           .(SEX, SEVERE_PATIENT),
                                           summarise,COUNT = length(PATIENT_ID)))

  }else if( by.sex == TRUE & by.age == FALSE & severe == FALSE){

    demographicsCount <- rbind(plyr::ddply(selection,
                                           .(SEX),
                                           summarise,COUNT = length(PATIENT_ID)))

  }
  demographicsCount$RACE <- "Other"

  return( demographicsCount )
}

diagnosesFile <- function( input, threeDigits, by.sex, by.age, severe ){

  if( threeDigits == TRUE ){
    selection <- input[, c("PATIENT_ID", "DIAGNOSTIC_3D", "SEX", "AGE_RANGE","SEVERE_PATIENT")]
    selection <- unique( selection )

    if( by.sex == TRUE & by.age == FALSE & severe == FALSE ){
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_3D,SEX),
                                          summarise,COUNT = length(DIAGNOSTIC_3D)))
    }else if( by.sex == TRUE & by.age == FALSE & severe == TRUE ){
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_3D,SEX, SEVERE_PATIENT),
                                          summarise,COUNT = length(DIAGNOSTIC_3D)))
    }else if( by.sex == FALSE & by.age == TRUE & severe == FALSE ) {
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_3D, AGE_RANGE),
                                          summarise,COUNT = length(DIAGNOSTIC_3D)))
    }else if( by.sex == FALSE & by.age == TRUE & severe == TRUE ) {
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_3D, AGE_RANGE, SEVERE_PATIENT),
                                          summarise,COUNT = length(DIAGNOSTIC_3D)))
    }else if( by.sex == TRUE & by.age == TRUE & severe == FALSE ) {
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_3D, SEX, AGE_RANGE),
                                          summarise,COUNT = length(DIAGNOSTIC_3D)))
    }else if( by.sex == TRUE & by.age == TRUE & severe == TRUE ) {
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_3D, SEX, AGE_RANGE, SEVERE_PATIENT),
                                          summarise,COUNT = length(DIAGNOSTIC_3D)))
    }else if( by.sex == FALSE & by.age == FALSE & severe == FALSE ) {
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_3D),
                                          summarise,COUNT = length(DIAGNOSTIC_3D)))
    }else if( by.sex == FALSE & by.age == FALSE & severe == TRUE ) {
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_3D, SEVERE_PATIENT),
                                          summarise,COUNT = length(DIAGNOSTIC_3D)))
    }

  }else{
    selection <- input[, c("PATIENT_ID", "DIAGNOSTIC_CODE", "SEX", "AGE_RANGE", "SEVERE_PATIENT")]
    selection <- unique( selection )

    if( by.sex == TRUE & by.age == FALSE  & severe == FALSE){
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_CODE,SEX),
                                          summarise,COUNT = length(DIAGNOSTIC_CODE)))
    }else if( by.sex == TRUE & by.age == FALSE  & severe == TRUE){
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_CODE,SEX, SEVERE_PATIENT),
                                          summarise,COUNT = length(DIAGNOSTIC_CODE)))
    }else if( by.sex == FALSE & by.age == TRUE & severe == FALSE ){
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_CODE,AGE_RANGE),
                                          summarise,COUNT = length(DIAGNOSTIC_CODE)))
    }else if( by.sex == FALSE & by.age == TRUE & severe == TRUE ){
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_CODE,AGE_RANGE, SEVERE_PATIENT),
                                          summarise,COUNT = length(DIAGNOSTIC_CODE)))
    }else if( by.sex == TRUE & by.age == TRUE  & severe == FALSE){
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_CODE,SEX, AGE_RANGE),
                                          summarise,COUNT = length(DIAGNOSTIC_CODE)))
    }else if( by.sex == TRUE & by.age == TRUE  & severe == TRUE){
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_CODE,SEX, AGE_RANGE, SEVERE_PATIENT),
                                          summarise,COUNT = length(DIAGNOSTIC_CODE)))
    }else  if( by.sex == FALSE & by.age == FALSE  & severe == FALSE){
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_CODE),
                                          summarise,COUNT = length(DIAGNOSTIC_CODE)))
    }else  if( by.sex == FALSE & by.age == FALSE  & severe == TRUE){
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_CODE, SEVERE_PATIENT),
                                          summarise,COUNT = length(DIAGNOSTIC_CODE)))
    }
  }
  return( selectionCount )
}

medicationFile <- function( input, by.sex, by.age, aggregationLevel, severe ){

  if( aggregationLevel == "GNL"){
    input <- unique( input[ , c("PATIENT_ID", "MEDICATION", "SEX", "AGE_RANGE", "SEVERE_PATIENT")] )
    input <- input[!is.na( input$MEDICATION ), ]
  }else if( aggregationLevel == "ATC"){
    input <- unique( input[ , c("PATIENT_ID", "ATC_Code", "SEX", "AGE_RANGE", "SEVERE_PATIENT")] )
    colnames(input)[2] <- "MEDICATION"
    input <- input[!is.na( input$MEDICATION ), ]

  }else if( aggregationLevel == "MEDICATION_NAME"){
    input <- unique( input[ , c("PATIENT_ID", "Med_Name_x", "SEX", "AGE_RANGE", "SEVERE_PATIENT")] )
    colnames(input)[2] <- "MEDICATION"
    input <- input[!is.na( input$MEDICATION ), ]
  }else if( aggregationLevel == "MEDICATION_CLASS"){
    input <- unique( input[ , c("PATIENT_ID", "Class_Name", "SEX", "AGE_RANGE", "SEVERE_PATIENT")] )
    colnames(input)[2] <- "MEDICATION"
    input <- input[!is.na( input$MEDICATION ), ]
  }


  if( by.sex == FALSE & by.age == FALSE & severe == FALSE){
    medicationCount <- rbind(plyr::ddply(input,
                                         .(MEDICATION),
                                         summarise,COUNT = length(PATIENT_ID)))
  }else if( by.sex == FALSE & by.age == FALSE & severe == TRUE){
    medicationCount <- rbind(plyr::ddply(input,
                                         .(MEDICATION, SEVERE_PATIENT),
                                         summarise,COUNT = length(PATIENT_ID)))
  }else if( by.sex == TRUE & by.age == TRUE & severe == FALSE){
    medicationCount <- rbind(plyr::ddply(input,
                                         .(MEDICATION, SEX, AGE_RANGE),
                                         summarise,COUNT = length(PATIENT_ID)))
  }else if( by.sex == TRUE & by.age == TRUE & severe == TRUE){
    medicationCount <- rbind(plyr::ddply(input,
                                         .(MEDICATION, SEX, AGE_RANGE, SEVERE_PATIENT),
                                         summarise,COUNT = length(PATIENT_ID)))
  }else if( by.sex == TRUE & by.age == FALSE & severe == FALSE){
    medicationCount <- rbind(plyr::ddply(input,
                                         .(MEDICATION, SEX),
                                         summarise,COUNT = length(PATIENT_ID)))
  }else if( by.sex == TRUE & by.age == FALSE & severe == TRUE){
    medicationCount <- rbind(plyr::ddply(input,
                                         .(MEDICATION, SEX, SEVERE_PATIENT),
                                         summarise,COUNT = length(PATIENT_ID)))
  }else if( by.sex == FALSE & by.age == TRUE & severe == FALSE){
    medicationCount <- rbind(plyr::ddply(input,
                                         .(MEDICATION, AGE_RANGE),
                                         summarise,COUNT = length(PATIENT_ID)))
  }else if( by.sex == FALSE & by.age == TRUE & severe == TRUE){
    medicationCount <- rbind(plyr::ddply(input,
                                         .(MEDICATION, AGE_RANGE, SEVERE_PATIENT),
                                         summarise,COUNT = length(PATIENT_ID)))
  }

  return( medicationCount)
}


DailyCounts <- function(input=sinceAdmission) {
  population <- unique(input[, c("PATIENT_ID", "SEX", "DEATH", "SEVERE_PATIENT", "CARE_RELEASE_DATE", "CARE_ENDS", "DATE_DIFF_IN_DAYS", "SEVERE_PATIENT_DATE")])
  population$los_visit_day <- population$CARE_RELEASE_DATE - population$CARE_ENDS

  # Prevent absurd dates
  first_calendar_day <- if_else(min(population$CARE_RELEASE_DATE) < as_date("2019-12-31"),
                                as_date("2019-12-31"),
                                min(population$CARE_RELEASE_DATE, na.rm = T))
  last_calendar_day <- if_else(max(population$CARE_ENDS) > lubridate::today(tz="UTC"),
                               lubridate::today(tz="UTC"),
                               max(population$CARE_ENDS, na.rm = T))
  max_los_visit <- as.numeric(max(population$DATE_DIFF_IN_DAYS, na.rm = T))

  ### Number patient on a given calendar date
  calendar_days_range <- as_date(first_calendar_day:last_calendar_day)
  calendar_day_count <- vector(mode="list", length=length(calendar_days_range))
  names(calendar_day_count) <- as.character(calendar_days_range)
  calendar_day_count_cumulative <- calendar_day_count
  calendar_cumulative_death <- calendar_day_count
  for (n in 1:length(calendar_days_range)) {
    day <- calendar_days_range[[n]]
    population$is_severe_that_day <- population$SEVERE_PATIENT_DATE < day
    population$is_severe_that_day <- tidyr::replace_na(population$is_severe_that_day, FALSE)
    count_present <- population[(day >= population$CARE_RELEASE_DATE) & (day <= population$CARE_ENDS), c("PATIENT_ID", "is_severe_that_day")] %>%
      unique() %>%
      select(is_severe_that_day) %>%
      table(useNA="no")
    calendar_day_count[[n]] <- as.data.frame(matrix(count_present, nrow=1, ncol=length(count_present), dimnames=list(c(day), names(count_present))))
    count_cumulative <- population[day >= population$DATE_DIFF_IN_DAYS, c("PATIENT_ID", "CARE_RELEASE_DATE", "is_severe_that_day")] %>%
      unique() %>%
      select(is_severe_that_day) %>%
      table(useNA="no")

    count_cumulative_death <- population[day >= population$DATE_DIFF_IN_DAYS, c("PATIENT_ID", "CARE_RELEASE_DATE", "DEATH")] %>%
      unique() %>%
      mutate(count_death = if_else(DEATH == "yes", 1, 0)) %>%
      select(count_death) %>%
      sum()
    calendar_day_count_cumulative[[n]] <- as.data.frame(matrix(count_cumulative,
                                                               nrow=1,
                                                               ncol=length(count_cumulative),
                                                               dimnames=list(c(day), names(count_cumulative))))
    calendar_cumulative_death[[n]] <- as.data.frame(matrix(count_cumulative_death,
                                                           nrow=1,
                                                           ncol=length(count_cumulative_death),
                                                           dimnames=list(c(day), names(count_cumulative_death))))

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

  df_calendar_cumulative_death <- bind_rows(calendar_cumulative_death, .id="day") %>%
    rename(cumulative_patients_dead = V1)

  df_calendar_count_cumulative <- left_join(df_calendar_count_cumulative, df_calendar_cumulative_death, by="day")

  return(list(df_calendar_count = df_calendar_count, df_calendar_count_cumulative = df_calendar_count_cumulative))
}

ClinicalCourse <- function(long_df = sinceAdmission) {
  population <- unique(long_df[, c("PATIENT_ID", "SEX", "DEATH", "SEVERE_PATIENT", "CARE_RELEASE_DATE", "CARE_ENDS", "DATE_DIFF_IN_DAYS", "SEVERE_PATIENT_DATE")])
  max_los_visit <- as.numeric(max(population$DATE_DIFF_IN_DAYS, na.rm = T))
  relative_days_range <- 0:max_los_visit
  relative_day_count <- vector(mode = "list", length = length(relative_days_range))
  names(relative_day_count) <- as.character(relative_days_range)
  for (n in 1:length(relative_days_range)) {
    day <- relative_days_range[[n]]
    count <- population[day <= population$DATE_DIFF_IN_DAYS, c("PATIENT_ID", "SEVERE_PATIENT")] %>%
      unique() %>%
      select(SEVERE_PATIENT) %>%
      table(useNA="no")
    relative_day_count[[n]] <- as.data.frame(matrix(count, nrow=1, ncol=length(count), dimnames=list(c(day), names(count))))
  }
  df_relative_count <- bind_rows(relative_day_count, .id="day")
  df_relative_count$num_patients_all_still_in_hospital <- apply(df_relative_count[-which(names(df_relative_count) == "day")],
                                                                1,
                                                                sum,
                                                                na.rm=TRUE)
  names(df_relative_count)[which(names(df_relative_count)=="TRUE")] <- "num_patients_ever_severe_still_in_hospital"
  df_relative_count[["FALSE"]] <- NULL
  return(df_relative_count)
}

#sink("mylogs.txt")

#################################################################################
# Read the files from the GitHub Repo https://github.com/gp2u/opendata4covid19/ #
#################################################################################
# this code is extracted from the GitHub repo file extract.R

#corona claim data
co19_t200_trans_dn = read_excel("./HIRA COVID-19 Sample Data_20200325.xlsx",
                                sheet=2)
#medication for claim data
co19_t530_trans_dn = read_excel("./HIRA COVID-19 Sample Data_20200325.xlsx", sheet=5)

#medical use history data
co19_t200_twjhe_dn = read_excel("./HIRA COVID-19 Sample Data_20200325.xlsx", sheet=6)

#medication for medical use history data
co19_t530_twjhe_dn = read_excel("./HIRA COVID-19 Sample Data_20200325.xlsx", sheet=9)
#
# ## ONLY FOR SAMPLE DATA: to have severe ICD codes in sample data
# co19_t200_trans_dn[co19_t200_trans_dn$MAIN_SICK == "J029", "MAIN_SICK"] <- "J80"
# co19_t200_trans_dn[co19_t200_trans_dn$SUB_SICK == "J029", "SUB_SICK"] <- "J80"
# co19_t530_twjhe_dn[is.na(co19_t200_twjhe_dn$MAIN_SICK), "MAIN_SICK"] <- "J80"
# co19_t530_twjhe_dn[is.na(co19_t200_twjhe_dn$SUB_SICK), "SUB_SICK"] <- "J80"
# co19_t530_trans_dn[co19_t530_trans_dn$GNL_CD %in% c("430101ATB", "179303ATE"), "GNL_CD"] <- "450200BIJ"
# co19_t530_twjhe_dn[co19_t530_twjhe_dn$GNL_CD %in% c("248902ATB", "179303ATE"), "GNL_CD"] <- "450200BIJ"
#
# # ONLY FOR SAMPLE DATA: data management for sample medical history, to have overlapping MID
# co19_t200_twjhe_dn$MID <- sample(co19_t200_trans_dn$MID,
#                                  length(co19_t200_twjhe_dn$MID),
#                                  replace=T)
# co19_t530_twjhe_dn$MID <- sample(co19_t200_trans_dn$MID,
#                                  length(co19_t530_twjhe_dn$MID),
#                                  replace=T)
# co19_t530_trans_dn$MID <- sample(co19_t200_trans_dn$MID,
#                                  length(co19_t530_trans_dn$MID),
#                                  replace=T)
# co19_t200_twjhe_dn[co19_t200_twjhe_dn$RECU_FR_DD %in% c(20190101, 20150404),'RECU_FR_DD'] <- "20200101"
#
# #ONLY FOR SAMPLE DATA: hospitalization patients
co19_t200_trans_dn[co19_t200_trans_dn$MAIN_SICK == "J80", "FOM_TP_CD"] <- "021"

#################################################
## CREATE THE DATASETS: HOSPITALIZED PATIENTS ##
##################@#############################
DataSet <- createDataSet( since              = co19_t200_trans_dn,
                          before             = co19_t200_twjhe_dn,
                          medication_since   = co19_t530_trans_dn,
                          medication_before  = co19_t530_twjhe_dn,
                          hospitalize = F)
sinceAdmission <- DataSet[DataSet$BEFORE_SINCE == "since",]
beforeAdmission <- DataSet[DataSet$BEFORE_SINCE == "before",]


###################
## DAILY COUNTS ##
##################
list_daily_counts <- DailyCounts(sinceAdmission)
df_calendar_count <- list_daily_counts[["df_calendar_count"]]
df_calendar_count_cumulative <- list_daily_counts[["df_calendar_count_cumulative"]]
df_clinical_course <- ClinicalCourse(sinceAdmission)

#create output folder
dir.create("./Results", showWarning = FALSE)


save( list_daily_counts, file="./Results/list_daily_counts.RData")
save( df_calendar_count, file="./Results/df_calendar_count.RData")
save( df_calendar_count_cumulative, file="./Results/df_calendar_count_cumulative.RData")
save( df_clinical_course, file="./Results/df_clinical_course.RData")


#########################
## DEMOGRAPHIC COUNTS ##
########################
demogSinceAdmissionBySex <- demographicsFile( input = sinceAdmission, by.sex = TRUE, by.age = FALSE, severe = TRUE )
demogSinceAdmissionByAge <- demographicsFile( input = sinceAdmission, by.sex = FALSE, by.age = TRUE, severe = TRUE )
demogSinceAdmissionBySexAndAge <- demographicsFile( input = sinceAdmission, by.sex = TRUE, by.age = TRUE, severe = TRUE )
save( demogSinceAdmissionBySex, file="./Results/demogSinceAdmissionBySex.RData")
save( demogSinceAdmissionByAge, file="./Results/demogSinceAdmissionByAge.RData")
save( demogSinceAdmissionBySexAndAge, file="./Results/demogSinceAdmissionBySexAndAge.RData")

demogBeforeAdmissionBySex <- demographicsFile( input = beforeAdmission, by.sex = TRUE, by.age = FALSE, severe = TRUE )
demogBeforeAdmissionByAge <- demographicsFile( input = beforeAdmission, by.sex = FALSE, by.age = TRUE, severe = TRUE )
demogBeforeAdmissionBySexAndAge <- demographicsFile( input = beforeAdmission, by.sex = TRUE, by.age = TRUE, severe = TRUE )
save( demogBeforeAdmissionBySex, file="./Results/demogBeforeAdmissionBySex.RData")
save( demogBeforeAdmissionByAge, file="./Results/demogBeforeAdmissionByAge.RData")
save( demogBeforeAdmissionBySexAndAge, file="./Results/demogBeforeAdmissionBySexAndAge.RData")


######################
## DIAGNOSES COUNTS ##
#########@############
diagnSinceAdmissionThreeD <- diagnosesFile( input = sinceAdmission, threeDigits = TRUE, by.sex=FALSE, by.age = FALSE, severe = TRUE )
diagnSinceAdmissionThreeD_bySex <- diagnosesFile( input = sinceAdmission, threeDigits = TRUE, by.sex=TRUE, by.age = FALSE, severe = TRUE )
diagnSinceAdmissionThreeD_byAge <- diagnosesFile( input = sinceAdmission, threeDigits = TRUE, by.sex=FALSE, by.age = TRUE, severe = TRUE )
diagnSinceAdmissionThreeD_bySexAndAge <- diagnosesFile( input = sinceAdmission, threeDigits = TRUE, by.sex=TRUE, by.age = TRUE, severe = TRUE )
save( diagnSinceAdmissionThreeD, file="./Results/diagnSinceAdmissionThreeD.RData")
save( diagnSinceAdmissionThreeD_bySex, file="./Results/diagnSinceAdmissionThreeD_bySex.RData")
save( diagnSinceAdmissionThreeD_byAge, file="./Results/diagnSinceAdmissionThreeD_byAge.RData")
save( diagnSinceAdmissionThreeD_bySexAndAge,  file="./Results/diagnSinceAdmissionThreeD_bySexAndAge.RData")

diagnBeforeAdmissionThreeD <- diagnosesFile( input = beforeAdmission, threeDigits = TRUE, by.sex=FALSE, by.age = FALSE, severe = TRUE )
diagnBeforeAdmissionThreeD_bySex <- diagnosesFile( input = beforeAdmission, threeDigits = TRUE, by.sex=TRUE, by.age = FALSE, severe = TRUE )
diagnBeforeAdmissionThreeD_byAge <- diagnosesFile( input = beforeAdmission, threeDigits = TRUE, by.sex=FALSE, by.age = TRUE, severe = TRUE )
diagnBeforeAdmissionThreeD_bySexAndAge <- diagnosesFile( input = beforeAdmission, threeDigits = TRUE, by.sex=TRUE, by.age = TRUE, severe = TRUE )
save( diagnBeforeAdmissionThreeD,  file="./Results/diagnBeforeAdmissionThreeD.RData")
save( diagnBeforeAdmissionThreeD_bySex, file="./Results/diagnBeforeAdmissionThreeD_bySex.RData")
save( diagnBeforeAdmissionThreeD_byAge,  file="./Results/diagnBeforeAdmissionThreeD_byAge.RData")
save( diagnBeforeAdmissionThreeD_bySexAndAge,  file="./Results/diagnBeforeAdmissionThreeD_bySexAndAge.RData")

diagnSinceAdmissionAllD <- diagnosesFile( input = sinceAdmission, threeDigits = FALSE, by.sex=FALSE, by.age = FALSE, severe = TRUE )
diagnSinceAdmissionAllD_bySex <- diagnosesFile( input = sinceAdmission, threeDigits = FALSE, by.sex=TRUE, by.age = FALSE, severe = TRUE )
diagnSinceAdmissionAllD_byAge <- diagnosesFile( input = sinceAdmission, threeDigits = FALSE, by.sex=FALSE, by.age = TRUE, severe = TRUE )
diagnSinceAdmissionAllD_bySexAndAge  <- diagnosesFile( input = sinceAdmission, threeDigits = FALSE, by.sex=TRUE, by.age = TRUE, severe = TRUE )
save( diagnSinceAdmissionAllD,  file="./Results/diagnSinceAdmissionAllD.RData")
save( diagnSinceAdmissionAllD_bySex, file="./Results/diagnSinceAdmissionAllD_bySex.RData")
save( diagnSinceAdmissionAllD_byAge,  file="./Results/diagnSinceAdmissionAllD_byAge.RData")
save( diagnSinceAdmissionAllD_bySexAndAge,  file="./Results/diagnSinceAdmissionAllD_bySexAndAge.RData")

diagnBeforeAdmissionAll <- diagnosesFile( input = beforeAdmission, threeDigits = FALSE, by.sex=FALSE, by.age = FALSE, severe = TRUE )
diagnBeforeAdmissionAllDD_bySex <- diagnosesFile( input = beforeAdmission, threeDigits = FALSE, by.sex=TRUE, by.age = FALSE, severe = TRUE )
diagnBeforeAdmissionAllD_byAge <- diagnosesFile( input = beforeAdmission, threeDigits = FALSE, by.sex=FALSE, by.age = TRUE, severe = TRUE )
diagnBeforeAdmissionAllD_bySexAndAge <- diagnosesFile( input = beforeAdmission, threeDigits = FALSE, by.sex=TRUE, by.age = TRUE, severe = TRUE )
save( diagnBeforeAdmissionAll,  file="./Results/diagnBeforeAdmissionAll.RData")
save( diagnBeforeAdmissionAllDD_bySex,  file="./Results/diagnBeforeAdmissionAllDD_bySex.RData")
save( diagnBeforeAdmissionAllD_byAge,  file="./Results/diagnBeforeAdmissionAllD_byAge.RData")
save( diagnBeforeAdmissionAllD_bySexAndAge, file="./Results/diagnBeforeAdmissionAllD_bySexAndAge.RData")

########################
## MEDICATION COUNTS ##
#######################
#levels for aggregation: GNL, ATC, MEDICATION_NAME, MEDICATION_CLASS
medicationSinceAdmission_GNL <- medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = FALSE, aggregationLevel = "GNL", severe = TRUE )
medicationSinceAdmission_ATC <- medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = FALSE, aggregationLevel = "ATC" , severe = TRUE)
medicationSinceAdmission_MEDICATION_NAME <- medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = FALSE, aggregationLevel = "MEDICATION_NAME", severe = TRUE )
medicationSinceAdmission_MEDICATION_CLASS <- medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = FALSE, aggregationLevel = "MEDICATION_CLASS", severe = TRUE )
save( medicationSinceAdmission_GNL,  file="./Results/medicationSinceAdmission_GNL.RData")
save( medicationSinceAdmission_ATC,  file="./Results/medicationSinceAdmission_ATC.RData")
save( medicationSinceAdmission_MEDICATION_NAME,  file="./Results/medicationSinceAdmission_MEDICATION_NAME.RData")
save( medicationSinceAdmission_MEDICATION_CLASS,  file="./Results/medicationSinceAdmission_MEDICATION_CLASS.RData")

medicationSinceAdmission_bySex_GNL <-medicationFile( input = sinceAdmission, by.sex = TRUE,  by.age = FALSE, aggregationLevel = "GNL", severe = TRUE )
medicationSinceAdmission_bySex_ATC <- medicationFile( input = sinceAdmission, by.sex = TRUE,  by.age = FALSE, aggregationLevel = "ATC", severe = TRUE  )
medicationSinceAdmission_bySex_MEDICATION_NAME <-  medicationFile( input = sinceAdmission, by.sex = TRUE,  by.age = FALSE, aggregationLevel = "MEDICATION_NAME", severe = TRUE )
medicationSinceAdmission_bySex_MEDICATION_CLASS <- medicationFile( input = sinceAdmission, by.sex = TRUE,  by.age = FALSE, aggregationLevel = "MEDICATION_CLASS", severe = TRUE )
save( medicationSinceAdmission_bySex_GNL,  file="./Results/medicationSinceAdmission_bySex_GNL.RData")
save( medicationSinceAdmission_bySex_ATC,  file="./Results/medicationSinceAdmission_bySex_ATC.RData")
save( medicationSinceAdmission_bySex_MEDICATION_NAME,  file="./Results/medicationSinceAdmission_bySex_MEDICATION_NAME.RData")
save( medicationSinceAdmission_bySex_MEDICATION_CLASS,  file="./Results/medicationSinceAdmission_bySex_MEDICATION_CLASS.RData")

medicationSinceAdmission_byAge_GNL <- medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = TRUE, aggregationLevel = "GNL", severe = TRUE   )
medicationSinceAdmission_byAge_ATC <- medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = TRUE, aggregationLevel = "ATC", severe = TRUE )
medicationSinceAdmission_byAge_MEDICATION_NAME <- medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = TRUE, aggregationLevel = "MEDICATION_NAME", severe = TRUE )
medicationSinceAdmission_byAge_MEDICATION_CLASS <- medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = TRUE, aggregationLevel = "MEDICATION_CLASS", severe = TRUE)
save( medicationSinceAdmission_byAge_GNL,  file="./Results/medicationSinceAdmission_byAge_GNL.RData")
save( medicationSinceAdmission_byAge_ATC,  file="./Results/medicationSinceAdmission_byAge_ATC.RData")
save( medicationSinceAdmission_byAge_MEDICATION_NAME,  file="./Results/medicationSinceAdmission_byAge_MEDICATION_NAME.RData")
save( medicationSinceAdmission_byAge_MEDICATION_CLASS,  file="./Results/medicationSinceAdmission_byAge_MEDICATION_CLASS.RData")

medicationSinceAdmission_bySexAndAge_GNL <- medicationFile( input = sinceAdmission, by.sex = TRUE,  by.age = TRUE, aggregationLevel = "GNL", severe = TRUE)
medicationSinceAdmission_bySexAndAge_ATC <- medicationFile( input = sinceAdmission, by.sex = TRUE,  by.age = TRUE, aggregationLevel = "ATC", severe = TRUE)
medicationSinceAdmission_bySexAndAge_MEDICATION_NAME <- medicationFile( input = sinceAdmission, by.sex = TRUE,  by.age = TRUE, aggregationLevel = "MEDICATION_NAME", severe = TRUE)
medicationSinceAdmission_bySexAndAge_MEDICATION_CLASS <- medicationFile( input = sinceAdmission, by.sex = TRUE,  by.age = TRUE, aggregationLevel = "MEDICATION_CLASS", severe = TRUE)
save( medicationSinceAdmission_bySexAndAge_GNL,  file="./Results/medicationSinceAdmission_bySexAndAge_GNL.RData")
save( medicationSinceAdmission_bySexAndAge_ATC,  file="./Results/medicationSinceAdmission_bySexAndAge_ATC.RData")
save( medicationSinceAdmission_bySexAndAge_MEDICATION_NAME,  file="./Results/medicationSinceAdmission_bySexAndAge_MEDICATION_NAME.RData")
save( medicationSinceAdmission_bySexAndAge_MEDICATION_CLASS,  file="./Results/medicationSinceAdmission_bySexAndAge_MEDICATION_CLASS.RData")

medicationBeforeAdmission_GNL <- medicationFile( input = beforeAdmission, by.sex = FALSE, by.age = FALSE, aggregationLevel = "GNL", severe = TRUE)
medicationBeforeAdmission_ATC <-medicationFile( input = beforeAdmission, by.sex = FALSE, by.age = FALSE, aggregationLevel = "ATC", severe = TRUE)
medicationBeforeAdmission_MEDICATION_NAME <-medicationFile( input = beforeAdmission, by.sex = FALSE, by.age = FALSE, aggregationLevel = "MEDICATION_NAME", severe = TRUE)
medicationBeforeAdmission_MEDICATION_CLASS <-medicationFile( input = beforeAdmission, by.sex = FALSE, by.age = FALSE, aggregationLevel = "MEDICATION_CLASS", severe = TRUE)
save( medicationBeforeAdmission_GNL,  file="./Results/medicationBeforeAdmission_GNL.RData")
save( medicationBeforeAdmission_ATC,  file="./Results/medicationBeforeAdmission_ATC.RData")
save( medicationBeforeAdmission_MEDICATION_NAME,  file="./Results/medicationBeforeAdmission_MEDICATION_NAME.RData")
save( medicationBeforeAdmission_MEDICATION_CLASS,  file="./Results/medicationBeforeAdmission_MEDICATION_CLASS.RData")

medicationBeforeAdmission_bySex_GNL <- medicationFile( input = beforeAdmission, by.sex = TRUE,  by.age = FALSE, aggregationLevel = "GNL", severe = TRUE)
medicationBeforeAdmission_bySex_ATC <- medicationFile( input = beforeAdmission, by.sex = TRUE,  by.age = FALSE, aggregationLevel = "ATC", severe = TRUE)
medicationBeforeAdmission_bySex_MEDICATION_NAME <- medicationFile( input = beforeAdmission, by.sex = TRUE,  by.age = FALSE, aggregationLevel = "MEDICATION_NAME", severe = TRUE)
medicationBeforeAdmission_bySex_MEDICATION_CLASS <- medicationFile( input = beforeAdmission, by.sex = TRUE,  by.age = FALSE, aggregationLevel = "MEDICATION_CLASS", severe = TRUE)
save( medicationBeforeAdmission_bySex_GNL,  file="./Results/medicationBeforeAdmission_bySex_GNL.RData")
save( medicationBeforeAdmission_bySex_ATC,  file="./Results/medicationBeforeAdmission_bySex_ATC.RData")
save( medicationBeforeAdmission_bySex_MEDICATION_NAME,  file="./Results/medicationBeforeAdmission_bySex_MEDICATION_NAME.RData")
save( medicationBeforeAdmission_bySex_MEDICATION_CLASS,  file="./Results/medicationBeforeAdmission_bySex_MEDICATION_CLASS.RData")

medicationBeforeAdmission_byAge_GNL <- medicationFile( input = beforeAdmission, by.sex = FALSE, by.age = TRUE, aggregationLevel = "GNL", severe = TRUE)
medicationBeforeAdmission_byAge_ATC <- medicationFile( input = beforeAdmission, by.sex = FALSE, by.age = TRUE, aggregationLevel = "ATC", severe = TRUE)
medicationBeforeAdmission_byAge_MEDICATION_NAME <- medicationFile( input = beforeAdmission, by.sex = FALSE, by.age = TRUE, aggregationLevel = "MEDICATION_NAME", severe = TRUE)
medicationBeforeAdmission_byAge_MEDICATION_CLASS <- medicationFile( input = beforeAdmission, by.sex = FALSE, by.age = TRUE, aggregationLevel = "MEDICATION_CLASS", severe = TRUE)
save( medicationBeforeAdmission_byAge_GNL,  file="./Results/medicationBeforeAdmission_byAge_GNL.RData")
save( medicationBeforeAdmission_byAge_ATC,  file="./Results/medicationBeforeAdmission_byAge_ATC.RData")
save( medicationBeforeAdmission_byAge_MEDICATION_NAME,  file="./Results/medicationBeforeAdmission_byAge_MEDICATION_NAME.RData")
save( medicationBeforeAdmission_byAge_MEDICATION_CLASS,  file="./Results/medicationBeforeAdmission_byAge_MEDICATION_CLASS.RData")

medicationBeforeAdmission_bySexAndAge_GNL <- medicationFile( input = beforeAdmission, by.sex = TRUE,  by.age = TRUE, aggregationLevel = "GNL", severe = TRUE)
medicationBeforeAdmission_bySexAndAge_ATC <- medicationFile( input = beforeAdmission, by.sex = TRUE,  by.age = TRUE, aggregationLevel = "ATC", severe = TRUE)
medicationBeforeAdmission_bySexAndAge_MEDICATION_NAME <- medicationFile( input = beforeAdmission, by.sex = TRUE,  by.age = TRUE, aggregationLevel = "MEDICATION_NAME", severe = TRUE)
medicationBeforeAdmission_bySexAndAge_MEDICATION_CLASS <- medicationFile( input = beforeAdmission, by.sex = TRUE,  by.age = TRUE, aggregationLevel = "MEDICATION_CLASS", severe = TRUE)
save( medicationBeforeAdmission_bySexAndAge_GNL,  file="./Results/medicationBeforeAdmission_bySexAndAge_GNL.RData")
save( medicationBeforeAdmission_bySexAndAge_ATC, file ="medicationBeforeAdmission_bySexAndAge_ATC.RData")
save( medicationBeforeAdmission_bySexAndAge_MEDICATION_NAME,  file="./Results/medicationBeforeAdmission_bySexAndAge_MEDICATION_NAME.RData")
save( medicationBeforeAdmission_bySexAndAge_MEDICATION_CLASS,  file="./Results/medicationBeforeAdmission_bySexAndAge_MEDICATION_CLASS.RData")

rm(DataSet)
rm(sinceAdmission)
rm(beforeAdmission)

########################################
## CREATE THE DATASETS: ALL PATIENTS ##
##################@####################
DataSet <- createDataSet( since              = co19_t200_trans_dn,
                          before             = co19_t200_twjhe_dn,
                          medication_since   = co19_t530_trans_dn,
                          medication_before  = co19_t530_twjhe_dn,
                          hospitalize = FALSE,
                          days_before_offset = 15,
                          days_before = 365)
sinceAdmission <- DataSet[DataSet$BEFORE_SINCE == "since",]
beforeAdmission <- DataSet[DataSet$BEFORE_SINCE == "before",]

###################
## DAILY COUNTS ##
##################
list_daily_counts <- DailyCounts(sinceAdmission)
df_calendar_count <- list_daily_counts[["df_calendar_count"]]
df_calendar_count_cumulative <- list_daily_counts[["df_calendar_count_cumulative"]]
df_clinical_course <- ClinicalCourse(sinceAdmission)

save( list_daily_counts, file="./Results/list_daily_counts_All.RData")
save( df_calendar_count, file="./Results/df_calendar_count_All.RData")
save( df_calendar_count_cumulative, file="./Results/df_calendar_count_cumulative_All.RData")
save( df_clinical_course, file="./Results/df_clinical_course_All.RData")


#########################
## DEMOGRAPHIC COUNTS ##
########################
demogSinceAdmissionBySex <- demographicsFile( input = sinceAdmission, by.sex = TRUE, by.age = FALSE, severe = FALSE )
demogSinceAdmissionByAge <- demographicsFile( input = sinceAdmission, by.sex = FALSE, by.age = TRUE, severe = FALSE )
demogSinceAdmissionBySexAndAge <- demographicsFile( input = sinceAdmission, by.sex = TRUE, by.age = TRUE, severe = FALSE )
save( demogSinceAdmissionBySex, file="./Results/demogSinceAdmissionBySex_All.RData")
save( demogSinceAdmissionByAge, file="./Results/demogSinceAdmissionByAge_All.RData")
save( demogSinceAdmissionBySexAndAge, file="./Results/demogSinceAdmissionBySexAndAge_All.RData")

demogBeforeAdmissionBySex <- demographicsFile( input = beforeAdmission, by.sex = TRUE, by.age = FALSE, severe = FALSE )
demogBeforeAdmissionByAge <- demographicsFile( input = beforeAdmission, by.sex = FALSE, by.age = TRUE, severe = FALSE )
demogBeforeAdmissionBySexAndAge <- demographicsFile( input = beforeAdmission, by.sex = TRUE, by.age = TRUE, severe = FALSE )
save( demogBeforeAdmissionBySex, file="./Results/demogBeforeAdmissionBySex_All.RData")
save( demogBeforeAdmissionByAge, file="./Results/demogBeforeAdmissionByAge_All.RData")
save( demogBeforeAdmissionBySexAndAge, file="./Results/demogBeforeAdmissionBySexAndAge_All.RData")


######################
## DIAGNOSES COUNTS ##
#########@############
diagnSinceAdmissionThreeD <- diagnosesFile( input = sinceAdmission, threeDigits = TRUE, by.sex=FALSE, by.age = FALSE, severe = FALSE  )
diagnSinceAdmissionThreeD_bySex <- diagnosesFile( input = sinceAdmission, threeDigits = TRUE, by.sex=TRUE, by.age = FALSE, severe = FALSE  )
diagnSinceAdmissionThreeD_byAge <- diagnosesFile( input = sinceAdmission, threeDigits = TRUE, by.sex=FALSE, by.age = TRUE, severe = FALSE  )
diagnSinceAdmissionThreeD_bySexAndAge <- diagnosesFile( input = sinceAdmission, threeDigits = TRUE, by.sex=TRUE, by.age = TRUE, severe = FALSE  )
save( diagnSinceAdmissionThreeD, file="./Results/diagnSinceAdmissionThreeD_All.RData")
save( diagnSinceAdmissionThreeD_bySex, file="./Results/diagnSinceAdmissionThreeD_bySex_All.RData")
save( diagnSinceAdmissionThreeD_byAge, file="./Results/diagnSinceAdmissionThreeD_byAge_All.RData")
save( diagnSinceAdmissionThreeD_bySexAndAge, file="./Results/diagnSinceAdmissionThreeD_bySexAndAge_All.RData")

diagnBeforeAdmissionThreeD <- diagnosesFile( input = beforeAdmission, threeDigits = TRUE, by.sex=FALSE, by.age = FALSE, severe = FALSE  )
diagnBeforeAdmissionThreeD_bySex <- diagnosesFile( input = beforeAdmission, threeDigits = TRUE, by.sex=TRUE, by.age = FALSE, severe = FALSE  )
diagnBeforeAdmissionThreeD_byAge <- diagnosesFile( input = beforeAdmission, threeDigits = TRUE, by.sex=FALSE, by.age = TRUE, severe = FALSE  )
diagnBeforeAdmissionThreeD_bySexAndAge <- diagnosesFile( input = beforeAdmission, threeDigits = TRUE, by.sex=TRUE, by.age = TRUE, severe = FALSE  )
save( diagnBeforeAdmissionThreeD, file="./Results/diagnBeforeAdmissionThreeD_All.RData")
save( diagnBeforeAdmissionThreeD_bySex, file="./Results/diagnBeforeAdmissionThreeD_bySex_All.RData")
save( diagnBeforeAdmissionThreeD_byAge, file="./Results/diagnBeforeAdmissionThreeD_byAge_All.RData")
save( diagnBeforeAdmissionThreeD_bySexAndAge, file="./Results/diagnBeforeAdmissionThreeD_bySexAndAge_All.RData")

diagnSinceAdmissionAllD <- diagnosesFile( input = sinceAdmission, threeDigits = FALSE, by.sex=FALSE, by.age = FALSE, severe = FALSE  )
diagnSinceAdmissionAllD_bySex <- diagnosesFile( input = sinceAdmission, threeDigits = FALSE, by.sex=TRUE, by.age = FALSE, severe = FALSE  )
diagnSinceAdmissionAllD_byAge <- diagnosesFile( input = sinceAdmission, threeDigits = FALSE, by.sex=FALSE, by.age = TRUE, severe = FALSE  )
diagnSinceAdmissionAllD_bySexAndAge  <- diagnosesFile( input = sinceAdmission, threeDigits = FALSE, by.sex=TRUE, by.age = TRUE, severe = FALSE  )
save( diagnSinceAdmissionAllD, file="./Results/diagnSinceAdmissionAllD_All.RData")
save( diagnSinceAdmissionAllD_bySex, file="./Results/diagnSinceAdmissionAllD_bySex_All.RData")
save( diagnSinceAdmissionAllD_byAge, file="./Results/diagnSinceAdmissionAllD_byAge_All.RData")
save( diagnSinceAdmissionAllD_bySexAndAge, file="./Results/diagnSinceAdmissionAllD_bySexAndAge_All.RData")

diagnBeforeAdmissionAll <- diagnosesFile( input = beforeAdmission, threeDigits = FALSE, by.sex=FALSE, by.age = FALSE, severe = FALSE  )
diagnBeforeAdmissionAllDD_bySex <- diagnosesFile( input = beforeAdmission, threeDigits = FALSE, by.sex=TRUE, by.age = FALSE, severe = FALSE  )
diagnBeforeAdmissionAllD_byAge <- diagnosesFile( input = beforeAdmission, threeDigits = FALSE, by.sex=FALSE, by.age = TRUE, severe = FALSE  )
diagnBeforeAdmissionAllD_bySexAndAge <- diagnosesFile( input = beforeAdmission, threeDigits = FALSE, by.sex=TRUE, by.age = TRUE, severe = FALSE  )
save( diagnBeforeAdmissionAll, file="./Results/diagnBeforeAdmissionAll_All.RData")
save( diagnBeforeAdmissionAllDD_bySex, file="./Results/diagnBeforeAdmissionAllDD_bySex_All.RData")
save( diagnBeforeAdmissionAllD_byAge, file="./Results/diagnBeforeAdmissionAllD_byAge_All.RData")
save( diagnBeforeAdmissionAllD_bySexAndAge, file="./Results/diagnBeforeAdmissionAllD_bySexAndAge_All.RData")

########################
## MEDICATION COUNTS ##
#######################
#levels for aggregation: GNL, ATC, MEDICATION_NAME, MEDICATION_CLASS
medicationSinceAdmission_GNL <- medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = FALSE, aggregationLevel = "GNL", severe = FALSE  )
medicationSinceAdmission_ATC <- medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = FALSE, aggregationLevel = "ATC", severe = FALSE  )
medicationSinceAdmission_MEDICATION_NAME <- medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = FALSE, aggregationLevel = "MEDICATION_NAME", severe = FALSE  )
medicationSinceAdmission_MEDICATION_CLASS <- medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = FALSE, aggregationLevel = "MEDICATION_CLASS", severe = FALSE  )
save( medicationSinceAdmission_GNL, file="./Results/medicationSinceAdmission_GNL_All.RData")
save( medicationSinceAdmission_ATC, file="./Results/medicationSinceAdmission_ATC_All.RData")
save( medicationSinceAdmission_MEDICATION_NAME, file="./Results/medicationSinceAdmission_MEDICATION_NAME_All.RData")
save( medicationSinceAdmission_MEDICATION_CLASS, file="./Results/medicationSinceAdmission_MEDICATION_CLASS_All.RData")

medicationSinceAdmission_bySex_GNL <-medicationFile( input = sinceAdmission, by.sex = TRUE,  by.age = FALSE, aggregationLevel = "GNL", severe = FALSE  )
medicationSinceAdmission_bySex_ATC <- medicationFile( input = sinceAdmission, by.sex = TRUE,  by.age = FALSE, aggregationLevel = "ATC" , severe = FALSE  )
medicationSinceAdmission_bySex_MEDICATION_NAME <-  medicationFile( input = sinceAdmission, by.sex = TRUE,  by.age = FALSE, aggregationLevel = "MEDICATION_NAME" , severe = FALSE )
medicationSinceAdmission_bySex_MEDICATION_CLASS <- medicationFile( input = sinceAdmission, by.sex = TRUE,  by.age = FALSE, aggregationLevel = "MEDICATION_CLASS", severe = FALSE  )
save( medicationSinceAdmission_bySex_GNL, file="./Results/medicationSinceAdmission_bySex_GNL_All.RData")
save( medicationSinceAdmission_bySex_ATC, file="./Results/medicationSinceAdmission_bySex_ATC_All.RData")
save( medicationSinceAdmission_bySex_MEDICATION_NAME, file="./Results/medicationSinceAdmission_bySex_MEDICATION_NAME_All.RData")
save( medicationSinceAdmission_bySex_MEDICATION_CLASS, file="./Results/medicationSinceAdmission_bySex_MEDICATION_CLASS_All.RData")

medicationSinceAdmission_byAge_GNL <- medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = TRUE, aggregationLevel = "GNL", severe = FALSE )
medicationSinceAdmission_byAge_ATC <- medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = TRUE, aggregationLevel = "ATC", severe = FALSE )
medicationSinceAdmission_byAge_MEDICATION_NAME <- medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = TRUE, aggregationLevel = "MEDICATION_NAME", severe = FALSE )
medicationSinceAdmission_byAge_MEDICATION_CLASS <- medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = TRUE, aggregationLevel = "MEDICATION_CLASS", severe = FALSE )
save( medicationSinceAdmission_byAge_GNL, file="./Results/medicationSinceAdmission_byAge_GNL_All.RData")
save( medicationSinceAdmission_byAge_ATC, file="./Results/medicationSinceAdmission_byAge_ATC_All.RData")
save( medicationSinceAdmission_byAge_MEDICATION_NAME, file="./Results/medicationSinceAdmission_byAge_MEDICATION_NAME_All.RData")
save( medicationSinceAdmission_byAge_MEDICATION_CLASS, file="./Results/medicationSinceAdmission_byAge_MEDICATION_CLASS_All.RData")

medicationSinceAdmission_bySexAndAge_GNL <- medicationFile( input = sinceAdmission, by.sex = TRUE,  by.age = TRUE, aggregationLevel = "GNL", severe = FALSE )
medicationSinceAdmission_bySexAndAge_ATC <- medicationFile( input = sinceAdmission, by.sex = TRUE,  by.age = TRUE, aggregationLevel = "ATC", severe = FALSE )
medicationSinceAdmission_bySexAndAge_MEDICATION_NAME <- medicationFile( input = sinceAdmission, by.sex = TRUE,  by.age = TRUE, aggregationLevel = "MEDICATION_NAME", severe = FALSE )
medicationSinceAdmission_bySexAndAge_MEDICATION_CLASS <- medicationFile( input = sinceAdmission, by.sex = TRUE,  by.age = TRUE, aggregationLevel = "MEDICATION_CLASS", severe = FALSE )
save( medicationSinceAdmission_bySexAndAge_GNL, file="./Results/medicationSinceAdmission_bySexAndAge_GNL_All.RData")
save( medicationSinceAdmission_bySexAndAge_ATC, file="./Results/medicationSinceAdmission_bySexAndAge_ATC_All.RData")
save( medicationSinceAdmission_bySexAndAge_MEDICATION_NAME, file="./Results/medicationSinceAdmission_bySexAndAge_MEDICATION_NAME_All.RData")
save( medicationSinceAdmission_bySexAndAge_MEDICATION_CLASS, file="./Results/medicationSinceAdmission_bySexAndAge_MEDICATION_CLASS_All.RData")

medicationBeforeAdmission_GNL <- medicationFile( input = beforeAdmission, by.sex = FALSE, by.age = FALSE, aggregationLevel = "GNL" , severe = FALSE )
medicationBeforeAdmission_ATC <-medicationFile( input = beforeAdmission, by.sex = FALSE, by.age = FALSE, aggregationLevel = "ATC" , severe = FALSE )
medicationBeforeAdmission_MEDICATION_NAME <-medicationFile( input = beforeAdmission, by.sex = FALSE, by.age = FALSE, aggregationLevel = "MEDICATION_NAME" , severe = FALSE )
medicationBeforeAdmission_MEDICATION_CLASS <-medicationFile( input = beforeAdmission, by.sex = FALSE, by.age = FALSE, aggregationLevel = "MEDICATION_CLASS" , severe = FALSE )
save( medicationBeforeAdmission_GNL, file="./Results/medicationBeforeAdmission_GNL_All.RData")
save( medicationBeforeAdmission_ATC, file="./Results/medicationBeforeAdmission_ATC_All.RData")
save( medicationBeforeAdmission_MEDICATION_NAME, file="./Results/medicationBeforeAdmission_MEDICATION_NAME_All.RData")
save( medicationBeforeAdmission_MEDICATION_CLASS, file="./Results/medicationBeforeAdmission_MEDICATION_CLASS_All.RData")

medicationBeforeAdmission_bySex_GNL <- medicationFile( input = beforeAdmission, by.sex = TRUE,  by.age = FALSE, aggregationLevel = "GNL" , severe = FALSE )
medicationBeforeAdmission_bySex_ATC <- medicationFile( input = beforeAdmission, by.sex = TRUE,  by.age = FALSE, aggregationLevel = "ATC" , severe = FALSE )
medicationBeforeAdmission_bySex_MEDICATION_NAME <- medicationFile( input = beforeAdmission, by.sex = TRUE,  by.age = FALSE, aggregationLevel = "MEDICATION_NAME" , severe = FALSE )
medicationBeforeAdmission_bySex_MEDICATION_CLASS <- medicationFile( input = beforeAdmission, by.sex = TRUE,  by.age = FALSE, aggregationLevel = "MEDICATION_CLASS" , severe = FALSE )
save( medicationBeforeAdmission_bySex_GNL, file="./Results/medicationBeforeAdmission_bySex_GNL_All.RData")
save( medicationBeforeAdmission_bySex_ATC, file="./Results/medicationBeforeAdmission_bySex_ATC_All.RData")
save( medicationBeforeAdmission_bySex_MEDICATION_NAME,file="./Results/medicationBeforeAdmission_bySex_MEDICATION_NAME_All.RData")
save( medicationBeforeAdmission_bySex_MEDICATION_CLASS, file="./Results/medicationBeforeAdmission_bySex_MEDICATION_CLASS_All.RData")

medicationBeforeAdmission_byAge_GNL <- medicationFile( input = beforeAdmission, by.sex = FALSE, by.age = TRUE, aggregationLevel = "GNL", severe = FALSE )
medicationBeforeAdmission_byAge_ATC <- medicationFile( input = beforeAdmission, by.sex = FALSE, by.age = TRUE, aggregationLevel = "ATC", severe = FALSE )
medicationBeforeAdmission_byAge_MEDICATION_NAME <- medicationFile( input = beforeAdmission, by.sex = FALSE, by.age = TRUE, aggregationLevel = "MEDICATION_NAME", severe = FALSE )
medicationBeforeAdmission_byAge_MEDICATION_CLASS <- medicationFile( input = beforeAdmission, by.sex = FALSE, by.age = TRUE, aggregationLevel = "MEDICATION_CLASS", severe = FALSE )
save( medicationBeforeAdmission_byAge_GNL, file="./Results/medicationBeforeAdmission_byAge_GNL_All.RData")
save( medicationBeforeAdmission_byAge_ATC, file="./Results/medicationBeforeAdmission_byAge_ATC_All.RData")
save( medicationBeforeAdmission_byAge_MEDICATION_NAME, file="./Results/medicationBeforeAdmission_byAge_MEDICATION_NAME_All.RData")
save( medicationBeforeAdmission_byAge_MEDICATION_CLASS, file="./Results/medicationBeforeAdmission_byAge_MEDICATION_CLASS_All.RData")

medicationBeforeAdmission_bySexAndAge_GNL <- medicationFile( input = beforeAdmission, by.sex = TRUE,  by.age = TRUE, aggregationLevel = "GNL", severe = FALSE )
medicationBeforeAdmission_bySexAndAge_ATC <- medicationFile( input = beforeAdmission, by.sex = TRUE,  by.age = TRUE, aggregationLevel = "ATC", severe = FALSE )
medicationBeforeAdmission_bySexAndAge_MEDICATION_NAME <- medicationFile( input = beforeAdmission, by.sex = TRUE,  by.age = TRUE, aggregationLevel = "MEDICATION_NAME", severe = FALSE )
medicationBeforeAdmission_bySexAndAge_MEDICATION_CLASS <- medicationFile( input = beforeAdmission, by.sex = TRUE,  by.age = TRUE, aggregationLevel = "MEDICATION_CLASS", severe = FALSE )
save( medicationBeforeAdmission_bySexAndAge_GNL, file="./Results/medicationBeforeAdmission_bySexAndAge_GNL_All.RData")
save( medicationBeforeAdmission_bySexAndAge_ATC, file="./Results/medicationBeforeAdmission_bySexAndAge_ATC_All.RData")
save( medicationBeforeAdmission_bySexAndAge_MEDICATION_NAME, file="./Results/medicationBeforeAdmission_bySexAndAge_MEDICATION_NAME_All.RData")
save( medicationBeforeAdmission_bySexAndAge_MEDICATION_CLASS, file="./Results/medicationBeforeAdmission_bySexAndAge_MEDICATION_CLASS_All.RData")

