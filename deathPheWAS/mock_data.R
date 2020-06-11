# ## ONLY FOR SAMPLE DATA: to have severe ICD codes in sample data
covid_file_path <- "./HIRA COVID-19 Sample Data_20200325.xlsx"


co19_t200_trans_dn[co19_t200_trans_dn$MAIN_SICK == "J029", "MAIN_SICK"] <- "J80"
co19_t200_trans_dn[co19_t200_trans_dn$SUB_SICK == "J029", "SUB_SICK"] <- "J80"
co19_t530_twjhe_dn[is.na(co19_t200_twjhe_dn$MAIN_SICK), "MAIN_SICK"] <- "J80"
co19_t530_twjhe_dn[is.na(co19_t200_twjhe_dn$SUB_SICK), "SUB_SICK"] <- "J80"
co19_t530_trans_dn[co19_t530_trans_dn$GNL_CD %in% c("430101ATB", "179303ATE"), "GNL_CD"] <- "450200BIJ"
co19_t530_twjhe_dn[co19_t530_twjhe_dn$GNL_CD %in% c("248902ATB", "179303ATE"), "GNL_CD"] <- "450200BIJ"

# ONLY FOR SAMPLE DATA: data management for sample medical history, to have overlapping MID
co19_t200_twjhe_dn$MID <- sample(co19_t200_trans_dn$MID,
                                 length(co19_t200_twjhe_dn$MID),
                                 replace=T)
co19_t530_twjhe_dn$MID <- sample(co19_t200_trans_dn$MID,
                                 length(co19_t530_twjhe_dn$MID),
                                 replace=T)
co19_t530_trans_dn$MID <- sample(co19_t200_trans_dn$MID,
                                 length(co19_t530_trans_dn$MID),
                                 replace=T)
co19_t200_twjhe_dn[co19_t200_twjhe_dn$RECU_FR_DD %in% c(20190101, 20150404),'RECU_FR_DD'] <- "20200101"
#ONLY FOR SAMPLE DATA: hospitalization patients
co19_t200_trans_dn[1, "FOM_TP_CD"] <- "021"
co19_t200_twjhe_dn[1, "FOM_TP_CD"] <- "021"
# Adding death
co19_t200_trans_dn[c(4,6), "DGRSLT_TP_CD"] <- "4"
