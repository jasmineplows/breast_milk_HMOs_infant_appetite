coefficients_function <- function(dS, hmo) {
  f <- paste0("`", hmo, "`", " ~ Timepoint*Secretor + (1|study_id)")
  fit <- lmer(f, data = dS)
  result <- summary(fit)
  return(result$coefficients)
}

summary_function <- function(dS, x) {
  x <- ensym(x)
  summarise(dS, 
            median = median(!! x, na.rm=T), lower = quantile(!!x, 0.25, na.rm=T), upper = quantile(!!x, 0.75, na.rm=T), n = n())
  
}

###Summary data for abundance
######Fully grouped dataset
grouped_dataSet_abundance <- data_for_abundance_table %>%
  group_by(Secretor, Timepoint)

#######Grouped by timepoint only
grouped_time_only_dataSet_abundance <- data_for_abundance_table %>%
  group_by(Timepoint)

#######Grouped by secretor only
grouped_secretor_only_dataSet_abundance <- data_for_abundance_table %>%
  group_by(Secretor)

#####Below here to create tables of summary statistics, not required for inline statistics

summary_function <- function(x) {
  summary <- list(tibble(median = median(x, na.rm=T), lower = quantile(!!x, 0.25, na.rm=T), upper = quantile(!!x, 0.75, na.rm=T), n = length(x[!is.na(x)])))
  return(summary)
  }

 summary_table <- data_for_median_plots %>%
   group_by(Secretor, Timepoint) %>%
   dplyr::summarise(across("2FL":"SUM", summary_function)) %>%
   unnest("2FL":"SUM", names_sep = "_")
 
write.table(summary_table, file = "summary_stats.txt", sep = "\t")

