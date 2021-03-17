###Functions
stat_sum_df_all <- function(fun, geom="pointrange", ...) {
  stat_summary(fun.data=fun, geom=geom, ...)
}

my_fitting_function <- function(hmo) {
  f <- paste0("`", hmo, "`", " ~ Timepoint*Secretor + (1|study_id)")
  fit <- lmer(f, data = data_for_looped_plots_complete_cases)
  aov <- anova(fit)
  return(aov)
}

plot_fun = function(x, y) {
  ggplot(data = data_for_looped_plots_complete_cases, aes(x = .data[[x]], y = .data[[y]], group = Secretor, colour = Secretor)) +
    stat_summary(geom = "line", fun.data = median_hilow) +
    stat_sum_df_all("median_hilow", fun.args=(conf.int = 0.5), linetype = "solid") +
    theme_bw() +
    theme(legend.title=element_blank()) +
    labs(
      title = paste("Time = ", format(my_fitting_function(y)[1,6],digits=3),", Secretor = ", format(my_fitting_function(y)[2,6],digits=3), ", Interaction = ", format(my_fitting_function(y)[3,6],digits=3), sep=""),
      x = "Timepoint",
      y = paste(y, "(\u03BCg/mL)")
    )
  # stat_compare_means(aes(group = Secretor), label = "p.signif") 
  # label.y = c(8.5, 6.5, 6.5, 5.5, 5.6))
} 



##########UP TO 12 MONTHSs################
dataSet_reduced <- dataSet %>%
  drop_na(Secretor) %>%
  select("study_id", "Timepoint", "Secretor", "2FL_ug_ml",	"3FL_ug_ml",	"LNnT_ug_ml",	"3SL_ug_ml",	"DFLac_ug_ml",	"6SL_ug_ml",	"LNT_ug_ml",	"LNFPI_ug_ml",	"LNFPII_ug_ml",	"LNFPIII_ug_ml",	"LSTb_ug_ml",	"LSTc_ug_ml",	"DFLNT_ug_ml",	"LNH_ug_ml",	"DSLNT_ug_ml",	"FLNH_ug_ml",	"DFLNH_ug_ml",	"FDSLNH_ug_ml",	"DSLNH_ug_ml",	"SUM_ug_ml") %>%
  rename_at(vars(ends_with("_ug_ml")),
            funs(str_replace(., "_ug_ml", ""))) %>%
  filter(Timepoint != "18 months" & Timepoint != "24 months" & Timepoint != "36 months")

complete_cases <- names(table(dataSet_reduced$study_id)[table(dataSet_reduced$study_id) > 2]) 

data_for_looped_plots_complete_cases <- dataSet_reduced[dataSet_reduced$study_id %in% complete_cases,]
# make_clean_names()

# which(colnames(data_for_median_plots)=="Timepoint")
hmos = names(data_for_looped_plots_complete_cases)[4:23]
Timepoint = names(data_for_looped_plots_complete_cases)[2]

hmos = purrr::set_names(hmos)
hmos

Timepoint = purrr::set_names(Timepoint)
Timepoint

#Example analysis
# example <- my_fitting_function("LNT_pct")
# models <- purrr::map(hmos, my_fitting_function)

#Example plot
# example_plot <- plot_fun("Timepoint", "LNT_pct")

all_plots_median_complete_12m = map(hmos,
                ~map(Timepoint, plot_fun, y = .x) )


pdf("all_change_plots_median_complete_12m.pdf")
all_plots_median_complete_12m
dev.off()

######18 MONTHS###########
dataSet_reduced <- dataSet %>%
  drop_na(Secretor) %>%
  select("study_id", "Timepoint", "Secretor", "2FL_ug_ml",	"3FL_ug_ml",	"LNnT_ug_ml",	"3SL_ug_ml",	"DFLac_ug_ml",	"6SL_ug_ml",	"LNT_ug_ml",	"LNFPI_ug_ml",	"LNFPII_ug_ml",	"LNFPIII_ug_ml",	"LSTb_ug_ml",	"LSTc_ug_ml",	"DFLNT_ug_ml",	"LNH_ug_ml",	"DSLNT_ug_ml",	"FLNH_ug_ml",	"DFLNH_ug_ml",	"FDSLNH_ug_ml",	"DSLNH_ug_ml",	"SUM_ug_ml") %>%
  rename_at(vars(ends_with("_ug_ml")),
            funs(str_replace(., "_ug_ml", ""))) %>%
  filter(Timepoint != "24 months" & Timepoint != "36 months") %>%
  filter(study_id != 65 & study_id != 78 & study_id != 117)

complete_cases <- names(table(dataSet_reduced$study_id)[table(dataSet_reduced$study_id) == 4]) 

data_for_looped_plots_complete_cases <- dataSet_reduced[dataSet_reduced$study_id %in% complete_cases,]

hmos = names(data_for_looped_plots_complete_cases)[4:23]
Timepoint = names(data_for_looped_plots_complete_cases)[2]

hmos = purrr::set_names(hmos)
hmos

Timepoint = purrr::set_names(Timepoint)
Timepoint

#Example analysis
# example <- my_fitting_function("LNT_pct")
# models <- purrr::map(hmos, my_fitting_function)

#Example plot
# example_plot <- plot_fun("Timepoint", "LNT_pct")

all_plots_median_complete_18m = map(hmos,
                                    ~map(Timepoint, plot_fun, y = .x) )


pdf("all_change_plots_median_complete_18m.pdf")
all_plots_median_complete_18m
dev.off()