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
sink("mylogs.txt")
#### Summary diagnotic count
phenotypeCount <- function( data ){
  diagnosticSummary <- data[, c("PATIENT_ID", "DIAGNOSTIC_3D")]
  diagnosticSummary <- unique( diagnosticSummary )
  return( as.data.frame( table( diagnosticSummary$DIAGNOSTIC_3D )) )
}

###PheWAS Analysis
myPhewasAnalysis <- function( data, ageMin, ageMax, caco, cases, control, correctionMethod ){
  
  #select subset for data analysis based on the age of the patient
  subset <- data[ data$AGE >= ageMin & data$AGE <= ageMax, 
                  c("PATIENT_ID", "SEX", "DIAGNOSTIC_3D", "AGE_RANGE")]
  subset <- subset[!duplicated( subset)]
  
  #select the distinct diagnostic codes (3 digits level)
  phenotypes <- unique( subset$DIAGNOSTIC_3D )
  
  print("Generating a PheWAS matrix ...")
  subset$COUNT <- 1
  phenotypedf <- as.data.frame(spread(subset, DIAGNOSTIC_3D, COUNT, fill = 0))
  
  phenotypedf$casecontrol <- as.character(ifelse( phenotypedf[caco]==cases, 1, 
                                                  ifelse( phenotypedf[caco] == control, 0, NA))
  )
  
  # Logistic regression analysis #
  print("Running the logistic regression analysis for each diagnostic ...")
  phenotypedf$casecontrol <- as.character(phenotypedf$casecontrol)
  
  phewasOutput <- as.data.frame( matrix(ncol = 7 ) )
  
  for( i in 1:length( phenotypes ) ){
    
    if( i %in% c(1, 10, 100, 500, 1000, 1500, 2000 )){
      print( paste0( "Diagnostic code ", i, " out of ", length( phenotypes )))
    }
    
    selection <- glm( formula= phenotypedf[, as.character(phenotypes[i])]~casecontrol, family = binomial(), data=phenotypedf, na.action = na.omit)
    ci   <- exp(summary(selection)$coefficients["casecontrol1",1]+qnorm(c(0.025, 0.975)) * summary(selection)$coefficients["casecontrol1",2])
    
    cacodf <-  phenotypedf[, c(as.character(phenotypes[i]), "PATIENT_ID", "casecontrol")]
    cacodf <- na.omit( cacodf )
    caseDisease <- length(unique(cacodf[ cacodf[, 1] == 1 & cacodf$casecontrol == "1", "PATIENT_ID"]))
    caseNoDisease <-  length(unique(cacodf[ cacodf[, 1] == 0 & cacodf$casecontrol == "1", "PATIENT_ID"]))
    controlDisease <-  length(unique(cacodf[ cacodf[, 1] == 1 & cacodf$casecontrol == "0", "PATIENT_ID"]))
    controlNoDisease <-  length(unique(cacodf[ cacodf[, 1] == 0 & cacodf$casecontrol == "0", "PATIENT_ID"]))
    newRow <- c ( as.character(phenotypes[i]), summary(selection)$coefficients[2], exp(summary(selection)$coefficients[2]), paste0("(", ci[1][1], ",", ci[2][1], ")"),
                  summary(selection)$coefficients[2,4], paste0(caseDisease+controlDisease,"(",caseDisease,"/",controlDisease,")"), 
                  paste0(caseNoDisease+controlNoDisease,"(",caseNoDisease,"/",controlNoDisease,")")
    )
    phewasOutput <- rbind( newRow, phewasOutput )
  }
  
  colnames( phewasOutput ) <- c("DIAGNOSTIC", "Coefficient", "OddsRatio", "Confidence_interval", "pvalue", "Phenotype_present", "Phenotype_absent")
  phewasOutput$adjustPvalue <- p.adjust( as.numeric(phewasOutput$pvalue), method = correctionMethod)
  print("Done! Congratulations! Your PheWAS analysis has been finished!")
  return( phewasOutput)
}

###Fisher Test Analysis
fisherAnalysis <- function( data, ageMin, ageMax, caco, cases, control, correctionMethod, aggregationLevel ){
  
  subset <- data[ data$AGE >= ageMin & data$AGE <= ageMax, ]
  phenotypes <- as.character(unique( subset$DIAGNOSTIC_3D ))
  
  subset <- as.data.frame( subset )
  subset$casecontrol <- as.character(ifelse( subset[caco]==cases, 1, 
                                             ifelse( subset[caco] == control, 0, NA)))
  
  
  results <- parallel::mclapply( phenotypes, tableData, mc.preschedule = TRUE, 
                                 mc.cores = 1, 
                                 data = subset,
                                 cases = 1, 
                                 control = 0 )
  
  resultsdf <- do.call("rbind", results )
  resultsdf <- as.data.frame( resultsdf, stringsAsFactors=FALSE )
  colnames(resultsdf) <- c( "Phenotype", "CasesWithPhenotye", "CasesWithoutPhenotype", 
                            "ControlsWithPhenotype", "ControlsWithoutPhenotype", 
                            "pValue", "95%confidenceInterval_min", "95%confidenceInterval_max","OddsRatio" )
  resultsdf$adjustPvalue <- p.adjust( as.numeric( resultsdf$pValue ), method = correctionMethod, n = nrow( resultsdf ) )
  return( resultsdf)
  
}

tableData <- function ( phenoCode, data, cases, control ) {
  
  casesPresent <- length( unique ( data[ data$DIAGNOSTIC_3D == phenoCode & data$casecontrol == cases, "PATIENT_ID" ] ) )
  controlPresent <-  length( unique ( data[ data$DIAGNOSTIC_3D == phenoCode & data$casecontrol == control, "PATIENT_ID" ] ) )
  casesNotPresent <- length(unique( data[ data$casecontrol == cases, "PATIENT_ID"])) - casesPresent
  controlsNotPresent <- length(unique(data[ data$casecontrol == control, "PATIENT_ID"])) - controlPresent
  
  mat <- matrix( c( casesPresent, casesNotPresent, controlPresent, controlsNotPresent), ncol = 2 )
  f <- fisher.test(as.table(mat), alt="two.sided")
  c(phenoCode, casesPresent, casesNotPresent, controlPresent, controlsNotPresent, f$p.value, f$conf.int, f$estimate)
  
}


#################################################################################
# Read the files from the GitHub Repo https://github.com/gp2u/opendata4covid19/ #
#################################################################################
# this code is extracted from the GitHub repo file extract.R

#corona claim data
co19_t200_trans_dn = read_excel("./Data/HIRA COVID-19 Sample Data_20200325.xlsx", sheet=2)

#medical use history data
co19_t200_twjhe_dn = read_excel("./Data/HIRA COVID-19 Sample Data_20200325.xlsx", sheet=6)

###############################
# Create a map table for SEX #
##############################
sex_tp_cd_map <- data.table( SEX_TP_CD = c(1,2,9,"$"), 
            SEX = c("male", "female", "other", "$"))

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
agelabels <- c("0-2","3-5","6-11","12-17","18-25","26-49","50-69","70-79","80-150")

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

dir.create("./Results", showWarning = FALSE)
save(demographicsBySexCounts, file="./Results/demographicsBySexCountsCoronaClaim.RData")

#Diagnostic count
diagnosticCount <- phenotypeCount( data = dataAnalysisSelection )
diagnosticCount
save(diagnosticCount, file="./Results/diagnosticCountCoronaClaim.RData")

#Diagnostic count by sex
diagnosticBySex <- dataAnalysisSelection[, c("PATIENT_ID", "DIAGNOSTIC_3D", "SEX")]
diagnosticBySex <- unique( diagnosticBySex )

diagnosticBySexCounts <- rbind(plyr::ddply(diagnosticBySex,
                                             .(DIAGNOSTIC_3D,SEX),
                                             summarise,COUNT = length(DIAGNOSTIC_3D)))

diagnosticBySexCounts
save(diagnosticBySexCounts, file="./Results/diagnosticBySexCountsCoronaClaim.RData")


##### Sex PheWAS 
sexPheWAS_0to150 <- myPhewasAnalysis(data    = dataAnalysisSelection, 
                       ageMin  = 0,
                       ageMax  = 150,
                       caco    = "SEX",
                       cases   = "female",
                       control = "male",
                       correctionMethod = "bonferroni" )

save(sexPheWAS_0to150, file="./Results/sexPheWAS_0to150_CoronaClaim.RData")

#Sex PheWAS by age range
ageRanges <- unique(dataAnalysisSelection$AGE_RANGE)

for( i in 1:length(ageRanges)){
  print(paste0("Age Range: ",as.character(ageRanges[i])))
  range <- as.character(ageRanges[i])
  
  if("female" %in% demographicsBySexCounts[ demographicsBySexCounts$AGE_RANGE == range, "SEX"]){
    
    if("male" %in% demographicsBySexCounts[ demographicsBySexCounts$AGE_RANGE == range, "SEX"]){
      min  <- as.numeric(unlist(strsplit(range, "-"))[1])
      max <- as.numeric(unlist(strsplit(range, "-"))[2])
      
      sexPheWAS <- myPhewasAnalysis(data = dataAnalysisSelection, 
                                    ageMin  = min,
                                    ageMax  = max,
                                    caco    = "SEX",
                                    cases   = "female",
                                    control = "male",
                                    correctionMethod = "bonferroni" )
      
      save(sexPheWAS, file=paste0("./Results/sexPheWAS_", min, "to", max, "_CoronaClaim.RData"))
      
      
    }else{
      next()
    }
  }
  
   
}

#### Sex Fisher
fisher_0to150 <- fisherAnalysis( data = dataAnalysisSelection, 
                   ageMin = 0, 
                   ageMax = 150, 
                   caco = "SEX",
                   cases = "female",
                   control = "male",
                   correctionMethod = "bonferroni")

save(fisher_0to150, file="./Results/fisher_0to150_CoronaClaim.RData")

#Sex Fisher by age range
ageRanges <- unique(dataAnalysisSelection$AGE_RANGE)
for( i in 1:length(ageRanges)){
  
  print(paste0("Age Range: ",as.character(ageRanges[i])))
  range <- as.character(ageRanges[i])
  min  <- as.numeric(unlist(strsplit(range, "-"))[1])
  max <- as.numeric(unlist(strsplit(range, "-"))[2])
  
  sexFisher <- fisherAnalysis( data = dataAnalysisSelection, 
                                   ageMin = min, 
                                   ageMax = max, 
                                   caco = "SEX",
                                   cases = "female",
                                   control = "male",
                                   correctionMethod = "bonferroni")
  
  save(sexFisher, file=paste0("./Results/sexFisher_", min, "to", max, "_CoronaClaim.RData"))
  
}

rm(dataAnalysis)
rm(dataAnalysisSelection)

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
agelabels <- c("0-2","3-5","6-11","12-17","18-25","26-49","50-69","70-79","80-150")

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
save(demographicsBySexCounts, file="./Results/demographicsBySexCounts_MedicalHistory.RData")

#Diagnostic count
diagnosticCount <- phenotypeCount( data = dataAnalysisSelection )
save(diagnosticCount, file="./Results/diagnosticCount_MedicalHistory.RData")

#Diagnostic count by sex
diagnosticBySex <- dataAnalysisSelection[, c("PATIENT_ID", "DIAGNOSTIC_3D", "SEX")]
diagnosticBySex <- unique( diagnosticBySex )

diagnosticBySexCounts <- rbind(plyr::ddply(diagnosticBySex,
                                           .(DIAGNOSTIC_3D,SEX),
                                           summarise,COUNT = length(DIAGNOSTIC_3D)))

diagnosticBySexCounts
save(diagnosticBySexCounts, file="./Results/diagnosticBySexCounts_MedicalHistory.RData")



##### Sex PheWAS 
sexPheWAS_0to150 <- myPhewasAnalysis(data    = dataAnalysisSelection, 
                                     ageMin  = 0,
                                     ageMax  = 150,
                                     caco    = "SEX",
                                     cases   = "female",
                                     control = "male",
                                     correctionMethod = "bonferroni" )

save(sexPheWAS_0to150, file="./Results/sexPheWAS_0to150_MedicalHistory.RData")

#Sex PheWAS by age range
ageRanges <- unique(dataAnalysisSelection$AGE_RANGE)

for( i in 1:length(ageRanges)){
  print(paste0("Age Range: ",as.character(ageRanges[i])))
  range <- as.character(ageRanges[i])
  
  if("female" %in% demographicsBySexCounts[ demographicsBySexCounts$AGE_RANGE == range, "SEX"]){
    
    if("male" %in% demographicsBySexCounts[ demographicsBySexCounts$AGE_RANGE == range, "SEX"]){
      min  <- as.numeric(unlist(strsplit(range, "-"))[1])
      max <- as.numeric(unlist(strsplit(range, "-"))[2])
      
      sexPheWAS <- myPhewasAnalysis(data = dataAnalysisSelection, 
                                    ageMin  = min,
                                    ageMax  = max,
                                    caco    = "SEX",
                                    cases   = "female",
                                    control = "male",
                                    correctionMethod = "bonferroni" )
      
      save(sexPheWAS, file=paste0("./Results/sexPheWAS_", min, "to", max, "_MedicalHistory.RData"))
      
      
    }else{
      next()
    }
  }
  
  
}


#### Sex Fisher
fisher_0to150 <- fisherAnalysis( data = dataAnalysisSelection, 
                                 ageMin = 0, 
                                 ageMax = 150, 
                                 caco = "SEX",
                                 cases = "female",
                                 control = "male",
                                 correctionMethod = "bonferroni")

save(fisher_0to150, file="./Results/fisher_0to150_MedicalHistory.RData")

#Sex Fisher by age range
ageRanges <- unique(dataAnalysisSelection$AGE_RANGE)
for( i in 1:length(ageRanges)){
  
  print(paste0("Age Range: ",as.character(ageRanges[i])))
  range <- as.character(ageRanges[i])
  min  <- as.numeric(unlist(strsplit(range, "-"))[1])
  max <- as.numeric(unlist(strsplit(range, "-"))[2])
  
  sexFisher <- fisherAnalysis( data = dataAnalysisSelection, 
                               ageMin = min, 
                               ageMax = max, 
                               caco = "SEX",
                               cases = "female",
                               control = "male",
                               correctionMethod = "bonferroni")
  
  save(sexFisher, file=paste0("./Results/sexFisher_", min, "to", max, "_MedicalHistory.RData"))
  
}
sink()

