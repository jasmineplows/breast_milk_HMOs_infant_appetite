dataSet <- within(dataSet, {
  study_id <- factor(study_id)
  Secretor <- factor(Secretor, levels = 0:1, labels = c("Non-Secretors", "Secretors"))
  baby_gender <- factor(baby_gender, levels = 1:2, labels = c("Female", "Male"))
  Timepoint <- factor(timepoint, levels = c(1, 6, 12, 18, 24, 36), labels = c("1", "6", "12", "18", "24", "36"))
})

dataSet$bmi_percentile <- round(pnorm(dataSet$zbmi)*100,0)

dataSet$bmi_category <- ifelse(dataSet$bmi_percentile < 5,"Underweight",
                               ifelse(dataSet$bmi_percentile < 85,"Normal",
                                      ifelse(dataSet$bmi_percentile < 95,"Overweight",
                                             ifelse(dataSet$bmi_percentile <= 100,"Obese")
                                      )
                               )
)

dataSet$mom_bmi_category <- ifelse(dataSet$mom_BMI < 24.9,"Normal",
                                      ifelse(dataSet$mom_BMI < 29.9,"Overweight",
                                             ifelse(dataSet$mom_BMI <= 100,"Obese")
                                      )
)

dataSet <- within(dataSet, {
  bmi_category <- factor(bmi_category, levels = c("Underweight", "Normal", "Overweight", "Obese"), labels = c("Underweight", "Normal", "Overweight", "Obese"))
  mom_bmi_category <- factor(mom_bmi_category, levels = c("Normal", "Overweight", "Obese"), labels = c("Normal", "Overweight", "Obese"))
})

TabledataSet <- dataSet %>%
  filter(!is.na(Secretor)) %>%
  group_by(study_id) %>%
  fill(SES_index_final, .direction = "updown")

TabledataSet <- within(TabledataSet, {
  Timepoint <- factor(Timepoint, levels = c(1, 6, 12, 18, 24), labels = c("1 month", "6 months", "12 months", "18 months", "24 months"))
})

#########Cleaned dataset for median plots and summary statistics within text#########
data_for_median_plots <- dataSet %>%
  drop_na(Secretor) %>%
  rename_at(vars(ends_with("_ug_ml")),
            funs(str_replace(., "_ug_ml", "")))
# make_clean_names()

######Fully grouped dataset
grouped_dataSet <- data_for_median_plots %>%
  group_by(Secretor, Timepoint)

#######Grouped by timepoint only
grouped_time_only_dataSet <- data_for_median_plots %>%
  group_by(Timepoint)

#######Grouped by secretor only
grouped_secretor_only_dataSet <- data_for_median_plots %>%
  group_by(Secretor)

#######Secretors only
TabledataSet_Secretors <- TabledataSet %>% 
  dplyr::filter(Secretor == "Secretors")

######Non-secretors only
TabledataSet_NonSecretors <- TabledataSet %>% 
  dplyr::filter(Secretor == "Non-Secretors")


#######COMPLETE CASES ONLY
dataSet_reduced <- TabledataSet %>%
  select("study_id", "Timepoint", "Secretor", "2FL_ug_ml",	"3FL_ug_ml",	"LNnT_ug_ml",	"3SL_ug_ml",	"DFLac_ug_ml",	"6SL_ug_ml",	"LNT_ug_ml",	"LNFPI_ug_ml",	"LNFPII_ug_ml",	"LNFPIII_ug_ml",	"LSTb_ug_ml",	"LSTc_ug_ml",	"DFLNT_ug_ml",	"LNH_ug_ml",	"DSLNT_ug_ml",	"FLNH_ug_ml",	"DFLNH_ug_ml",	"FDSLNH_ug_ml",	"DSLNH_ug_ml",	"SUM_ug_ml") %>%
  dplyr::filter(Timepoint != "36 months")

complete_cases <- names(table(dataSet_reduced$study_id)[table(dataSet_reduced$study_id) == 5]) 

data_complete_cases <- dataSet_reduced[dataSet_reduced$study_id %in% complete_cases,]

####Complete cases Secretors only
data_complete_cases_Secretors <- data_complete_cases %>% 
  dplyr::filter(Secretor == "Secretors")


####Complete cases Non-secretors only
data_complete_cases_Non_secretors <- data_complete_cases %>% 
  dplyr::filter(Secretor == "Non-Secretors")
