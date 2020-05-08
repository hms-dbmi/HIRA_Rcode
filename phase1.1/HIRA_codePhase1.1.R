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

#################
### FUNCTIONS ###
#################

##### when creating the data set we have to consider that the same patient can have multiple admissions
##### and that the same patient can be administered multiple drugs
##### and check that the merge is correct or look for alternatives to create the file, or create several files
createDataSet <- function( input, medication, hospitalize, severe ){
  
  #variables of our interest
  dataAnalysis = input[,c( "MID", "SEX_TP_CD", "PAT_AGE", "MAIN_SICK", "SUB_SICK", 
                           "FOM_TP_CD", "RECU_FR_DD", "RECU_TO_DD","DGRSLT_TP_CD" )]
  
  #SEX_TP_CD to sex
  sex_tp_cd_map = read_excel("SEX_TP_CD.xlsx", sheet=1)
  
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
                                       HOSPITALIZATION = dataAnalysis$FOM_TP_CD
  )
  
  #Create a new column with the diagnostic code at 3 digits level
  dataAnalysisSelection$DIAGNOSTIC_3D <- substr(dataAnalysisSelection$DIAGNOSTIC_CODE, 1, 3)
  
  #Select hospitalize patients ONLY
  if( hospitalize == TRUE){
    dataAnalysisSelection <- dataAnalysisSelection[ dataAnalysisSelection$HOSPITALIZATION == "021", ]
  }
  
  #Create a data.frame with the medication data and select only hospitalize patients
  dataAnalysisMedication =  data.frame( PATIENT_ID  = medication$MID, 
                                        MEDICATION  = medication$GNL_CD)
  
  #drug mapping
  gnl_to_4ce = read.delim("4CE_in_GNL_drug_overlap.tsv")
  
  #merge both to do selection an analysis based on the ACT medication code
  dataAnalysisMedicationComplete <- merge( dataAnalysisMedication, gnl_to_4ce, 
                                           by.x = "MEDICATION", 
                                           by.y = "GNL_CD", 
                                           all.x = TRUE )
  
  #merge everything in a unique data.frame
  dataAnalysisSelection <- merge( dataAnalysisSelection, dataAnalysisMedicationComplete, by="PATIENT_ID", 
                                  all = TRUE)
  
  #Select severe patients only
  if( severe == TRUE ){
    severePatientsIcd <- dataAnalysisSelection[ dataAnalysisSelection$DIAGNOSTIC_3D %in% c("J80", "J95"), "PATIENT_ID"]
    severePatientsMed <- dataAnalysisSelection[ dataAnalysisSelection$Type_for_4CE_Analysis == "Severe Illness Medication", "PATIENT_ID" ]
    severePatients <- unique( c( severePatientsIcd, severePatientsMed ))
    dataAnalysisSelection <- dataAnalysisSelection[ dataAnalysisSelection$PATIENT_ID %in% severePatients, ]
  }
  
  #transform dates columns as date format 
  dataAnalysisSelection$CARE_RELEASE_DATE <- ymd(as.character(dataAnalysisSelection$CARE_RELEASE_DATE))
  dataAnalysisSelection$CARE_ENDS <- ymd(as.character(dataAnalysisSelection$CARE_ENDS))
  
  #estimate the days off difference between care release and end date
  dataAnalysisSelection$DATE_DIFF_IN_DAYS <- as.numeric(as.Date( dataAnalysisSelection$CARE_ENDS) -
                                                          as.Date( dataAnalysisSelection$CARE_RELEASE_DATE))

  #add the age breaks as a column
  agebreaks <- c(0,3,6,12,18,26,50,70,80,150)
  agelabels <- c("0-2","3-5","6-11","12-17","18-25","26-49","50-69","70-79","80+")
  
  setDT(dataAnalysisSelection)[, AGE_RANGE := cut(AGE,
                                                  breaks = agebreaks, 
                                                  right = FALSE, 
                                                  labels = agelabels)]
  
  return( dataAnalysisSelection )
}

demographicsFile <- function( input, by.sex, by.age ){
  
  selection <- unique( input[, c("PATIENT_ID", "AGE_RANGE", "SEX") ] )
  
  if( by.sex == TRUE & by.age == TRUE){
    demographicsCount <- rbind(plyr::ddply(selection,
                                           .(AGE_RANGE,SEX),
                                           summarise,COUNT = length(PATIENT_ID)))
  }else if( by.sex == FALSE & by.age == TRUE){
    demographicsCount <- rbind(plyr::ddply(selection,
                                           .(AGE_RANGE),
                                           summarise,COUNT = length(PATIENT_ID)))
  }else if( by.sex == TRUE & by.age == FALSE){
    demographicsCount <- rbind(plyr::ddply(selection,
                                           .(SEX),
                                           summarise,COUNT = length(PATIENT_ID)))
  }
  
  demographicsCount$RACE <- "Other"
  
  return( demographicsCount )
}

diagnosesFile <- function( input, threeDigits, by.sex, by.age ){
  
  if( threeDigits == TRUE ){
    selection <- input[, c("PATIENT_ID", "DIAGNOSTIC_3D", "SEX", "AGE_RANGE")]
    selection <- unique( selection )
    
    if( by.sex == TRUE & by.age == FALSE ){
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_3D,SEX),
                                          summarise,COUNT = length(DIAGNOSTIC_3D)))
    }else if( by.sex == FALSE & by.age == TRUE ) {
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_3D, AGE_RANGE),
                                          summarise,COUNT = length(DIAGNOSTIC_3D)))
    }else if( by.sex == TRUE & by.age == TRUE ) {
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_3D, SEX, AGE_RANGE),
                                          summarise,COUNT = length(DIAGNOSTIC_3D)))
    }else if( by.sex == FALSE & by.age == FALSE ) {
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_3D),
                                          summarise,COUNT = length(DIAGNOSTIC_3D)))
    }

  }else{
    selection <- input[, c("PATIENT_ID", "DIAGNOSTIC_CODE", "SEX", "AGE_RANGE")]
    selection <- unique( selection )
    
    if( by.sex == TRUE & by.age == FALSE ){
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_CODE,SEX),
                                          summarise,COUNT = length(DIAGNOSTIC_CODE)))
    }else if( by.sex == FALSE & by.age == TRUE ){
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_CODE,AGE_RANGE),
                                          summarise,COUNT = length(DIAGNOSTIC_CODE)))
    }else if( by.sex == TRUE & by.age == TRUE ){
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_CODE,SEX, AGE_RANGE),
                                          summarise,COUNT = length(DIAGNOSTIC_CODE)))
    }else  if( by.sex == FALSE & by.age == FALSE ){
      selectionCount <- rbind(plyr::ddply(selection,
                                          .(DIAGNOSTIC_CODE),
                                          summarise,COUNT = length(DIAGNOSTIC_CODE)))
      }
 }
  return( selectionCount )
}

medicationFile <- function( input, by.sex, by.age, aggregationLevel ){
  
  if( aggregationLevel == "GNL"){
    input <- unique( input[ , c("PATIENT_ID", "MEDICATION", "SEX", "AGE_RANGE")] )
    input <- input[!is.na( input$MEDICATION ), ]
  }else if( aggregationLevel == "ATC"){
    input <- unique( input[ , c("PATIENT_ID", "ATC_Code", "SEX", "AGE_RANGE")] )
    colnames(input)[2] <- "MEDICATION"
    input <- input[!is.na( input$MEDICATION ), ]
  }else if( aggregationLevel == "MEDICATION_NAME"){
    input <- unique( input[ , c("PATIENT_ID", "Med_Name_x", "SEX", "AGE_RANGE")] )
    colnames(input)[2] <- "MEDICATION"
    input <- input[!is.na( input$MEDICATION ), ]
  }else if( aggregationLevel == "MEDICATION_CLASS"){
    input <- unique( input[ , c("PATIENT_ID", "Class_Name", "SEX", "AGE_RANGE")] )
    colnames(input)[2] <- "MEDICATION"
    input <- input[!is.na( input$MEDICATION ), ]
  }

  
  if( by.sex == FALSE & by.age == FALSE){
    medicationCount <- rbind(plyr::ddply(input,
                                         .(MEDICATION),
                                         summarise,COUNT = length(PATIENT_ID)))
  }else if( by.sex == TRUE & by.age == TRUE){
    medicationCount <- rbind(plyr::ddply(input,
                                         .(MEDICATION, SEX, AGE_RANGE),
                                         summarise,COUNT = length(PATIENT_ID)))
  }else if( by.sex == TRUE & by.age == FALSE){
    medicationCount <- rbind(plyr::ddply(input,
                                         .(MEDICATION, SEX),
                                         summarise,COUNT = length(PATIENT_ID)))
  }else if( by.sex == FALSE & by.age == TRUE){
    medicationCount <- rbind(plyr::ddply(input,
                                         .(MEDICATION, AGE_RANGE),
                                         summarise,COUNT = length(PATIENT_ID)))
  }

  return( medicationCount)
}

dailyCountsFile <- function( input ){
  
  dailyCounts <- data.frame( CALENDAR_DATE = unique( sort(c( input$CARE_RELEASE_DATE, 
                                                             input$CARE_ENDS))), 
                             CUMULATIVE_PATIENTS_ALL = NA,
                             CUMULATIVE_PATIENTS_SEVERE = NA, 
                             CUMULATIVE_PATIENTS_DEAD = NA,
                             NUM_PATIENTS_IN_HOSPITAL_ON_THIS_DATE = NA, 
                             NUM_PATIENTS_IN_HOSPITAL_AND_SEVERE_ON_THIS_DATE = NA )
  
  for( i in 1:nrow(dailyCounts)){
    
    dailyCounts$CUMULATIVE_PATIENTS_ALL[i] <- length( unique( 
      input[ as.Date( input$CARE_RELEASE_DATE) <= dailyCounts$CALENDAR_DATE[i] &
               as.Date( input$CARE_ENDS) >= dailyCounts$CALENDAR_DATE[i], "PATIENT_ID" ] ))
    
    dailyCounts$NUM_PATIENTS_IN_HOSPITAL_ON_THIS_DATE[i] <- length( unique( 
      input[ as.Date( input$CARE_RELEASE_DATE) <= dailyCounts$CALENDAR_DATE[i] &
               as.Date( input$CARE_ENDS) >= dailyCounts$CALENDAR_DATE[i] &
               input$DEATH == "no", "PATIENT_ID" ] ))
    
    dailyCounts$CUMULATIVE_PATIENTS_DEAD[i] <-  length( unique( 
      input[ as.Date( input$CARE_RELEASE_DATE) <= dailyCounts$CALENDAR_DATE[i] &
               as.Date( input$CARE_ENDS) >= dailyCounts$CALENDAR_DATE[i] &
               input$DEATH == "yes", "PATIENT_ID" ] ))                                                                                     
  }
  return( dailyCounts )
}


#sink("mylogs.txt")

#################################################################################
# Read the files from the GitHub Repo https://github.com/gp2u/opendata4covid19/ #
#################################################################################
# this code is extracted from the GitHub repo file extract.R

#corona claim data
co19_t200_trans_dn = read_excel("HIRA COVID-19 Sample Data_20200325.xlsx", sheet=2)

#medication for claim data
co19_t530_trans_dn = read_excel("HIRA COVID-19 Sample Data_20200325.xlsx", sheet=5)

#medical use history data
co19_t200_twjhe_dn = read_excel("HIRA COVID-19 Sample Data_20200325.xlsx", sheet=6)

#medication for medical use history data
co19_t530_twjhe_dn = read_excel("HIRA COVID-19 Sample Data_20200325.xlsx", sheet=9)


#########################
## CREATE THE DATASETS ##
##################@######
sinceAdmission <- createDataSet( input       = co19_t200_trans_dn,
                                 medication  = co19_t530_trans_dn,
                                 hospitalize = FALSE,
                                 severe      = FALSE )

beforeAdmission <- createDataSet( input       = co19_t200_twjhe_dn,
                                  medication  = co19_t530_twjhe_dn,
                                  hospitalize = FALSE,
                                  severe      = FALSE )


###################
## DAILY COUNTS ##
##################
dailyCountsFile( input = sinceAdmission )
dailyCountsFile( input = beforeAdmission )

#########################
## DEMOGRAPHIC COUNTS ##
########################
demographicsFile( input = sinceAdmission, by.sex = TRUE, by.age = FALSE )
demographicsFile( input = sinceAdmission, by.sex = TRUE, by.age = TRUE )
demographicsFile( input = sinceAdmission, by.sex = FALSE, by.age = TRUE )

demographicsFile( input = beforeAdmission, by.sex = TRUE, by.age = FALSE )
demographicsFile( input = beforeAdmission, by.sex = TRUE, by.age = TRUE )
demographicsFile( input = beforeAdmission, by.sex = FALSE, by.age = TRUE )


######################
## DIAGNOSES COUNTS ##
#########@############
diagnosesFile( input = sinceAdmission, threeDigits = TRUE, by.sex=FALSE, by.age = FALSE )
diagnosesFile( input = sinceAdmission, threeDigits = TRUE, by.sex=TRUE, by.age = FALSE )
diagnosesFile( input = sinceAdmission, threeDigits = TRUE, by.sex=FALSE, by.age = TRUE )
diagnosesFile( input = sinceAdmission, threeDigits = TRUE, by.sex=TRUE, by.age = TRUE )

diagnosesFile( input = beforeAdmission, threeDigits = TRUE, by.sex=FALSE, by.age = FALSE )
diagnosesFile( input = beforeAdmission, threeDigits = TRUE, by.sex=TRUE, by.age = FALSE )
diagnosesFile( input = beforeAdmission, threeDigits = TRUE, by.sex=FALSE, by.age = TRUE )
diagnosesFile( input = beforeAdmission, threeDigits = TRUE, by.sex=TRUE, by.age = TRUE )

########################
## MEDICATION COUNTS ##
#######################
#levels for aggregation: GNL, ATC, MEDICATION_NAME, MEDICATION_CLASS
medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = FALSE, aggregationLevel = "GNL" )
medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = FALSE, aggregationLevel = "ATC" )
medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = FALSE, aggregationLevel = "MEDICATION_NAME" )
medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = FALSE, aggregationLevel = "MEDICATION_CLASS" )

medicationFile( input = sinceAdmission, by.sex = TRUE,  by.age = FALSE )
medicationFile( input = sinceAdmission, by.sex = FALSE, by.age = TRUE  )
medicationFile( input = sinceAdmission, by.sex = TRUE,  by.age = TRUE  )

medicationFile( input = beforeAdmission, by.sex = FALSE, by.age = FALSE )
medicationFile( input = beforeAdmission, by.sex = TRUE,  by.age = FALSE )
medicationFile( input = beforeAdmission, by.sex = FALSE, by.age = TRUE  )
medicationFile( input = beforeAdmission, by.sex = TRUE,  by.age = TRUE  )