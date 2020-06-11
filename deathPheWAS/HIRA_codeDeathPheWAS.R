covid_file_path <- "./HIRA COVID-19 Sample Data_20200325.xlsx"
drug_mapping_path <- "./4CE_in_GNL_drug_overlap.tsv"

##########################
# Libraries installation #
##########################
packages <- c("readxl","tidyverse", "lubridate")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
library(readxl)
library(tidyverse)
library(lubridate)

##########################
# Utils #
##########################
as_date_assert <- function(date_vector) {
   if (class(date_vector) == "Date") {
      output <- rep(TRUE, length(date_vector))
   } else {
      is_not_null <- !is.na(as.numeric(as.character(date_vector)))
      is_8_chars <- nchar(date_vector) == 8
      output <- is_not_null & is_8_chars
   }
   return(output)
}

# Apply as_date_assert on columns and reduce result by rows
rows_are_dates <- function(df) {
   test <- lapply(df, as_date_assert) %>% as.data.frame()
   apply(test, 1, all)
}


factory <- function(fun){
  function(...) {
    warn <- err <- NULL
    res <- withCallingHandlers(
      tryCatch(fun(...), error=function(e) {
        err <<- conditionMessage(e)
        NULL
      }), warning=function(w) {
        warn <<- append(warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    if (is.null(warn)) warn = "OK"
    if (is.null(err)) err = "OK"
    list(value = res, warn=warn, err=err)
  }
}

factory_confint <- function(...) {
  func <- factory(confint)
  out <- func(...)
  if (is.null(out$value)) {
    out$value <- list()
    out$value[["2.5 %"]] <- NA
    out$value[["97.5 %"]] <- NA
  }
  return(out$value)
}

##########################
# DATA MANAGEMENT #
##########################
#corona claim data
co19_t200_trans_dn = read_excel(covid_file_path,
                                sheet=2)
#medication for claim data
co19_t530_trans_dn = read_excel(covid_file_path, sheet=5)

#medical use history data
co19_t200_twjhe_dn = read_excel(covid_file_path, sheet=6)

#medication for medical use history data
co19_t530_twjhe_dn = read_excel(covid_file_path, sheet=9)

if (exists("EXAMPLE_CONFIG")) {
  source("mock_data.R")
}

#SEX_TP_CD to sex
sex_recoding <- c("male" = "1",
                  "female" = "2",
                  "other" = "9",
                  "$" = "$")
## Drugs
gnl_to_4ce = read.delim(drug_mapping_path, stringsAsFactors = F)[
  c("GNL_CD", "ATC_Code", "Type_for_4CE_Analysis", "Class_Name", "Med_Name_x")
  ]

##### ENV VARIABLES
SEVERE_ICD <- c("J80", "J95851", "0BH17EZ", "5A093", "5A094", "5A095")
SEVERE_DRUGS <- gnl_to_4ce[gnl_to_4ce$Type_for_4CE_Analysis == "Severe Illness Medication", "GNL_CD"]

## Ensuring tibble
co19_t200_trans_dn = as_tibble(co19_t200_trans_dn)
co19_t530_trans_dn = as_tibble(co19_t530_trans_dn)
co19_t200_twjhe_dn = as_tibble(co19_t200_twjhe_dn)
co19_t530_twjhe_dn = as_tibble(co19_t530_twjhe_dn)
gnl_to_4ce = as_tibble(gnl_to_4ce)

filter_dates <- rows_are_dates(co19_t200_trans_dn[c("RECU_FR_DD", "RECU_TO_DD")])
co19_t200_trans_dn <- co19_t200_trans_dn[filter_dates, ]
co19_t200_trans_dn[c("RECU_FR_DD", "RECU_TO_DD")] <-
  lapply(co19_t200_trans_dn[c("RECU_FR_DD", "RECU_TO_DD")], as_date)

filter_dates <- rows_are_dates(co19_t200_twjhe_dn[c("RECU_FR_DD", "RECU_TO_DD")])
co19_t200_twjhe_dn <- co19_t200_twjhe_dn[filter_dates, ]
co19_t200_twjhe_dn[c("RECU_FR_DD", "RECU_TO_DD")] <-
  lapply(co19_t200_twjhe_dn[c("RECU_FR_DD", "RECU_TO_DD")], as_date)

#### Person table creation
df_death <- co19_t200_trans_dn[co19_t200_trans_dn$DGRSLT_TP_CD == "4",
                               c("MID", "RECU_FR_DD", "PAT_AGE")] %>%
  rename(death_date = "RECU_FR_DD",
         age_death = "PAT_AGE") %>%
  mutate(death=1)

person_table <- co19_t200_trans_dn[, c("MID", "RECU_FR_DD", "PAT_AGE", "SEX_TP_CD")] %>%
  group_by(MID) %>%
  slice(which.min(RECU_FR_DD)) %>%
  rename(alignment_date = RECU_FR_DD,
         age = PAT_AGE,
         sex = SEX_TP_CD) %>%
  left_join(df_death, by = "MID") %>%
  replace_na(list(death = 0))
person_table[["sex"]] <- factor(person_table[["sex"]],
                                levels=sex_recoding,
                                labels=names(sex_recoding))

##### T200 tables merging
co19_t200_trans_dn["before_since"] <- "since"
co19_t200_twjhe_dn["before_since"] <- "before"
cols_t200 <- c("MID", "RECU_FR_DD", "RECU_TO_DD", "MAIN_SICK", "SUB_SICK", "before_since")
long_t200 <- bind_rows(co19_t200_trans_dn[cols_t200], co19_t200_twjhe_dn[cols_t200]) %>%
  gather(value = "ICD",
         key = "ICD_type",
         MAIN_SICK,
         SUB_SICK) %>%
  mutate(ICD_3D = substr(ICD, 1, 3),
         before_since = as.factor(before_since)) %>%
  drop_na(ICD_3D) %>%
  arrange(ICD_3D)

####### T530 tables merging
cols_t530 <- c("MID", "GNL_CD", "before_since", "PRSCP_GRANT_NO")
co19_t530_trans_dn[["before_since"]] <- "since"
co19_t530_twjhe_dn[["before_since"]] <- "before"
long_t530_severe <- bind_rows(co19_t530_trans_dn[cols_t530], co19_t530_twjhe_dn[cols_t530]) %>%
  subset(GNL_CD %in% SEVERE_DRUGS) %>%
  mutate(before_since = as.factor(before_since))

# SEVERITY
pattern_drugs <- '^(J80)|(J95851)|(0BH17EZ)|(5A093)|(5A094)|(5A095)'
pat_severe_icd <- long_t200[which(grepl(pattern_drugs, long_t200$ICD) &
                                    long_t200$before_since == "since"), "MID"] %>%
  unique() %>% unlist()
pat_severe_drugs <- long_t530_severe[long_t530_severe$before_since == "since", "MID"] %>%
  unique() %>% unlist()
MID_severe <- union(pat_severe_icd, pat_severe_drugs)

person_table["evere_severe"] <- if_else(person_table[["MID"]] %in% MID_severe, TRUE, FALSE)


#### Creating PheWAS data frame
unique_icd_3d  <- unique(long_t200$ICD_3D)
long_t200$present <- 1
wide_t200 <- spread(long_t200[c("MID", "RECU_FR_DD", "RECU_TO_DD", "before_since", "ICD_3D", "present")],
                    key = "ICD_3D",
                    fill = 0,
                    value = "present")
phewas_df <- left_join(person_table, wide_t200, by = "MID")

##########################
# STATS FUNCTIONS #
##########################

response_var <- "death"
glm_process <- function(phewas_df,
                        icd_studied,
                        covariates = c("age", "sex"),
                        response_var = "death") {
  ind_vars <- c(icd_studied, covariates)
  model <- glm( formula = paste(response_var, "~", paste(ind_vars, collapse = "+")),
                data = phewas_df[c(response_var, ind_vars)],
                na.action = na.omit,
                family = binomial(link = "logit"),
                x = F, y = F
  )
  sum_model <- summary(model)
  estimate <- sum_model$coefficients[icd_studied, "Estimate"]
  std_err <- sum_model$coefficients[icd_studied, "Std. Error"]
  p <- sum_model$coefficients[icd_studied, "Pr(>|z|)"]
  ci <- factory_confint(model, icd_studied, level=0.95)
  list(coeff = estimate,
       std_err = std_err,
       inf_95 = ci[["2.5 %"]],
       sup_95 = ci[["97.5 %"]],
       p = p)
}
factory_glm_process <- factory(glm_process)

phewas_function <- function(phewas_df, unique_icd_3d) {
  list_results_phewas <- vector(mode = "list", length = length(unique_icd_3d))
  names(list_results_phewas) <- unique_icd_3d
  for (icd_studied in unique_icd_3d) {
    list_results_phewas[[icd_studied]][["univariate"]] <- factory_glm_process(phewas_df,
                                                                              icd_studied,
                                                                              covariates = NULL,
                                                                              response_var = "death")
    list_results_phewas[[icd_studied]][["multivariate"]] <- factory_glm_process(phewas_df,
                                                                                icd_studied,
                                                                                covariates = c("age", "sex"),
                                                                                response_var = "death")
  }
  return(list_results_phewas)
}
factory_phewas_function <- factory(phewas_function)


### DESCRIPTIVE STATISTICS OF THE POPULATION, RETURNING AGGREGATED COUNTS ONLY, NO RAW DATA OR PATIENT LEVEL DATA
descriptive_stats <- function(phewas_df, person_table, long_t200, unique_icd_3d) {
  characteristics_var <- c("age", "sex", "death", "before_since", "evere_severe")
  describe_icd_df <- phewas_df
  describe_icd_df[c("sex", "death", "before_since", "evere_severe", unique_icd_3d)] <-
    lapply(describe_icd_df[c("sex", "death", "before_since", "evere_severe",
                             unique_icd_3d)],
           as.factor)
  person_table[c("sex", "death", "evere_severe")] <-
    lapply(person_table[c("sex", "death", "evere_severe")],
           as.factor)
  long_desc_t200 <- left_join(person_table, long_t200, by = "MID")

  description <- summary(describe_icd_df[c(characteristics_var, unique_icd_3d)], digits = 7)
  description_bygroup <- by(describe_icd_df[c(characteristics_var, unique_icd_3d)],
                            list(describe_icd_df$sex,
                                 describe_icd_df$evere_severe,
                                 describe_icd_df$before_since,
                                 describe_icd_df$death
                            ),
                            summary)
  person_table_summary <- summary(person_table[c("age", "sex", "death", "evere_severe", "age_death")])
  count_cat <- table(long_desc_t200$sex,
                     long_desc_t200$evere_severe,
                     long_desc_t200$before_since,
                     long_desc_t200$death,
                     long_desc_t200$ICD_type)
  nb_severe_icd <- length(pat_severe_icd)
  nb_severe_drugs <- length(pat_severe_drugs)
  return(list(description = description,
              description_bygroup = description_bygroup,
              person_table_summary = person_table_summary,
              count_cat = count_cat,
              nb_severe_icd = nb_severe_icd,
              nb_severe_drugs = nb_severe_drugs))
}
factory_descriptive_stats <- factory(descriptive_stats)


########## COMPUTATIONS --> OUTPUTS
##############
output_phewas_whole <- factory_phewas_function(phewas_df, unique_icd_3d)

output_phewas_evere_severe <- by(phewas_df,
                                 phewas_df$evere_severe,
                                 factory_phewas_function,
                                 unique_icd_3d = unique_icd_3d)

output_phewas_before_since <- by(phewas_df,
                                 phewas_df$before_since,
                                 factory_phewas_function,
                                 unique_icd_3d = unique_icd_3d)

output_descriptive_stats <- factory_descriptive_stats(phewas_df,
                                                      person_table,
                                                      long_t200,
                                                      unique_icd_3d)

##########################
# DATA TO EXPORT #
##########################
save(output_phewas_whole,
     output_phewas_evere_severe,
     output_phewas_before_since,
     output_descriptive_stats,
     file = "output_pheWAS.RData")

