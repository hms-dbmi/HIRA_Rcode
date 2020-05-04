##########################
# Libraries installation #
##########################
packages <- c("readxl","dplyr","tidyverse","tidyr","ggplot2", "plyr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
library(readxl)
library(plyr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

###############
## Functions ##
###############
#sink("mylogs.txt")
#### Summary diagnotic count
phenotypeCount <- function( data ){
  diagnosticSummary <- data[, c("PATIENT_ID", "DIAGNOSTIC_3D")]
  diagnosticSummary <- unique( diagnosticSummary )
  return( as.data.frame( table( diagnosticSummary$DIAGNOSTIC_3D )) )
}

#################################################################################
# Read the files from the GitHub Repo https://github.com/gp2u/opendata4covid19/ #
#################################################################################
# this code is extracted from the GitHub repo file extract.R

#corona claim data
co19_t200_trans_dn = read_excel("HIRA COVID-19 Sample Data_20200325.xlsx", sheet=2)

#medical use history data
co19_t200_twjhe_dn = read_excel("HIRA COVID-19 Sample Data_20200325.xlsx", sheet=6)

#SEX_TP_CD to sex
sex_tp_cd_map = read_excel("SEX_TP_CD.xlsx", sheet=1)


#####################
# Corona Claim Data #
#####################

#Select the variables of Interest
dataAnalysis = co19_t200_trans_dn[,c( "MID", "SEX_TP_CD", "PAT_AGE", "MAIN_SICK", "SUB_SICK" )]

#Map the number for SEX_TP_CD to meaningful text
dataAnalysis = merge(dataAnalysis, sex_tp_cd_map, by="SEX_TP_CD", all.x=T)

#Create a data.frame with meaningful column names and joining main and sub sick under the same column
dataAnalysisSelection <- data.frame( PATIENT_ID  = dataAnalysis$MID, 
                                     SEX = dataAnalysis$SEX, 
                                     AGE = dataAnalysis$PAT_AGE,
                                     DIAGNOSTIC_CODE = c( dataAnalysis$MAIN_SICK, dataAnalysis$SUB_SICK) 
                                     )

#Create a new column with the diagnostic code at 3 digits level
dataAnalysisSelection$DIAGNOSTIC_3D <- substr(dataAnalysisSelection$DIAGNOSTIC_CODE, 1, 3)


#### Demographics by sex summary
agebreaks <- c(0,3,6,12,18,26,50,70,80,150)
agelabels <- c("0-2","3-5","6-11","12-17","18-25","26-49","50-69","70-79","80+")

setDT(dataAnalysisSelection)[, AGE_RANGE := cut(AGE,
                               breaks = agebreaks, 
                               right = FALSE, 
                               labels = agelabels)]

dataAnalysisSelection <- dataAnalysisSelection[! is.na (dataAnalysisSelection$DIAGNOSTIC_3D),]
dataAnalysisSelection <- unique( dataAnalysisSelection )

demographicsBySex <- dataAnalysisSelection[, c("PATIENT_ID", "AGE_RANGE", "SEX")]
demographicsBySex <- unique( demographicsBySex )

demographicsBySexCounts <- rbind(plyr::ddply(demographicsBySex,
                                       .(AGE_RANGE,SEX),
                                       summarise,COUNT = length(SEX)))

demographicsBySexCounts
#save(demographicsBySexCounts, file="demographicsBySexCountsCoronaClaim.RData")

#Diagnostic count
diagnosticCount <- phenotypeCount( data = dataAnalysisSelection )
diagnosticCount
#save(diagnosticCount, file="diagnosticCountCoronaClaim.RData")

#Diagnostic count by sex
diagnosticBySex <- dataAnalysisSelection[, c("PATIENT_ID", "DIAGNOSTIC_3D", "SEX")]
diagnosticBySex <- unique( diagnosticBySex )

diagnosticBySexCounts <- rbind(plyr::ddply(diagnosticBySex,
                                             .(DIAGNOSTIC_3D,SEX),
                                             summarise,COUNT = length(DIAGNOSTIC_3D)))

diagnosticBySexCounts
#save(diagnosticBySexCounts, file="diagnosticBySexCountsCoronaClaim.RData")


#######################
# Medical Use History #
#######################

dataAnalysis = co19_t200_twjhe_dn[,c(
  "MID", "SEX_TP_CD", "PAT_AGE", "MAIN_SICK", "SUB_SICK", "DGRSLT_TP_CD"
)]


#Map the number for SEX_TP_CD to meaningful text
dataAnalysis = merge(dataAnalysis, sex_tp_cd_map, by="SEX_TP_CD", all.x=T)

#Create a data.frame with meaningful column names and joining main and sub sick under the same column
dataAnalysisSelection <- data.frame( PATIENT_ID  = dataAnalysis$MID, 
                                     SEX = dataAnalysis$SEX, 
                                     AGE = dataAnalysis$PAT_AGE,
                                     DIAGNOSTIC_CODE = c( dataAnalysis$MAIN_SICK, dataAnalysis$SUB_SICK) 
)

#Create a new column with the diagnostic code at 3 digits level
dataAnalysisSelection$DIAGNOSTIC_3D <- substr(dataAnalysisSelection$DIAGNOSTIC_CODE, 1, 3)


#### Demographics by sex summary
agebreaks <- c(0,3,6,12,18,26,50,70,80,150)
agelabels <- c("0-2","3-5","6-11","12-17","18-25","26-49","50-69","70-79","80+")

setDT(dataAnalysisSelection)[, AGE_RANGE := cut(AGE,
                                                breaks = agebreaks, 
                                                right = FALSE, 
                                                labels = agelabels)]

dataAnalysisSelection <- dataAnalysisSelection[! is.na (dataAnalysisSelection$DIAGNOSTIC_3D),]
dataAnalysisSelection <- unique( dataAnalysisSelection )

demographicsBySex <- dataAnalysisSelection[, c("PATIENT_ID", "AGE_RANGE", "SEX")]
demographicsBySex <- unique( demographicsBySex )

demographicsBySexCounts <- rbind(plyr::ddply(demographicsBySex,
                                             .(AGE_RANGE,SEX),
                                             summarise,COUNT = length(SEX)))

demographicsBySexCounts
#save(demographicsBySexCounts, file="demographicsBySexCounts_MedicalHistory.RData")

#Diagnostic count
diagnosticCount <- phenotypeCount( data = dataAnalysisSelection )
#save(diagnosticCount, file="diagnosticCount_MedicalHistory.RData")

#Diagnostic count by sex
diagnosticBySex <- dataAnalysisSelection[, c("PATIENT_ID", "DIAGNOSTIC_3D", "SEX")]
diagnosticBySex <- unique( diagnosticBySex )

diagnosticBySexCounts <- rbind(plyr::ddply(diagnosticBySex,
                                           .(DIAGNOSTIC_3D,SEX),
                                           summarise,COUNT = length(DIAGNOSTIC_3D)))

diagnosticBySexCounts
#save(diagnosticBySexCounts, file="diagnosticBySexCounts_MedicalHistory.RData")

