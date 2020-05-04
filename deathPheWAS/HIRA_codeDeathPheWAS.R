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

###PheWAS Analysis
myPhewasAnalysis <- function( data, ageMin, ageMax, caco, cases, control, correctionMethod ){
  
  #select subset for data analysis based on the age of the patient
  subset <- data[ data$AGE >= ageMin & data$AGE <= ageMax, 
                  c("PATIENT_ID", "SEX", "DIAGNOSTIC_3D", "AGE", "DEATH")]
  subset <- subset[!duplicated( subset), ]
  
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

#Select the variables of Interest from corona claim file
dataAnalysisCorona = co19_t200_trans_dn[,c( "MID", "SEX_TP_CD", "PAT_AGE", "MAIN_SICK", "SUB_SICK", "DGRSLT_TP_CD" )]

#Select the variables of Interest from medical use history file
dataAnalysisHistory = co19_t200_twjhe_dn[,c(
  "MID", "SEX_TP_CD", "PAT_AGE", "MAIN_SICK", "SUB_SICK", "DGRSLT_TP_CD" )]

#combine both
dataAnalysis <- rbind( dataAnalysisCorona, dataAnalysisHistory )

#Map the number for SEX_TP_CD to meaningful text
dataAnalysis = merge(dataAnalysis, sex_tp_cd_map, by="SEX_TP_CD", all.x=T)

#Map death info for DGRSLT_TP_CD to meaningful
dataAnalysis$DEATH <- ifelse( dataAnalysis$DGRSLT_TP_CD==4, "yes", "no")


#Create a data.frame with meaningful column names and joining main and sub sick under the same column
dataAnalysisSelection <- data.frame( PATIENT_ID  = dataAnalysis$MID, 
                                     SEX = dataAnalysis$SEX, 
                                     DEATH = dataAnalysis$DEATH,
                                     AGE = dataAnalysis$PAT_AGE,
                                     DIAGNOSTIC_CODE = c( dataAnalysis$MAIN_SICK, dataAnalysis$SUB_SICK) 
                                     )

#Create a new column with the diagnostic code at 3 digits level
dataAnalysisSelection$DIAGNOSTIC_3D <- substr(dataAnalysisSelection$DIAGNOSTIC_CODE, 1, 3)
dataAnalysisSelection <- dataAnalysisSelection[! is.na (dataAnalysisSelection$DIAGNOSTIC_3D),]
dataAnalysisSelection <- unique( dataAnalysisSelection )


##### Death PheWAS 
deathPheWAS_0to100 <- myPhewasAnalysis(data    = dataAnalysisSelection, 
                       ageMin  = 0,
                       ageMax  = 100,
                       caco    = "DEATH",
                       cases   = "yes",
                       control = "no",
                       correctionMethod = "bonferroni" )

#save(deathPheWAS_0to100, file="deathPheWAS_0to100_CoronaClaim.RData")


