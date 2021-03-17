###Functions
stat_sum_df_all <- function(fun, geom="pointrange",...) {
  stat_summary(fun.data=fun, geom=geom, ...)
}

my_fitting_function_bmi_ns <- function(hmo) {
  f <- paste0("`", hmo, "`", " ~ Timepoint*mom_bmi_category + (1|study_id)")
  fit <- lmer(f, data = data_for_median_plots_non_secretors)
  aov <- anova(fit)
  return(aov)
}

plot_fun_to_revise_bmi_ns = function(x, y) {
  ggplot(data = data_for_median_plots_non_secretors, aes(x = .data[[x]], y = .data[[y]], group = mom_bmi_category, colour = mom_bmi_category)) +
    stat_summary(geom = "line", fun.data = median_hilow, size = 0.8) +
    stat_sum_df_all("median_hilow",
                    fun.args = (conf.int = 0.5),
                    linetype = "solid",
                    size = 0.8) +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          # aspect.ratio = 4/4,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust=0.5),
          title = element_text(face = "bold", size = 15),
          axis.title = element_text(face = "bold", size = 15),
          axis.text.x = element_text(face = "bold", size = 15),
          axis.text.y = element_text(face = "bold", size= 15),
          legend.position = c(0.5, 0.95),
          legend.direction = "horizontal",
          legend.text = element_text(size = 15),
          plot.margin = unit(c(1.0, 1.0, 2.0, 1.0), "lines"), #top, right, bottom, left
          legend.title=element_blank()) +
    labs(
      title = paste("Time p = ", format(my_fitting_function_bmi_ns(y)[1,6],digits=2),", BMI p = ", format(my_fitting_function_bmi_ns(y)[2,6],digits=2), ", \nInteraction p = ", format(my_fitting_function_bmi_ns(y)[3,6],digits=2), sep=""),
      x = "Time (months postpartum)"
    )
  # stat_compare_means(aes(group = Secretor), label = "p.signif") 
  # label.y = c(8.5, 6.5, 6.5, 5.5, 5.6))
}

#########Median plots#########
data_for_median_plots_non_secretors <- dataSet %>%
  filter(Secretor == "Non-Secretors") %>%
  drop_na(Secretor) %>%
  rename_at(vars(ends_with("_ug_ml")),
            funs(str_replace(., "_ug_ml", "")))


# which(colnames(data_for_median_plots)=="Timepoint")
hmos_ns = names(data_for_median_plots_non_secretors)[4:23]
Timepoint_ns = names(data_for_median_plots_non_secretors)[2]

hmos_ns = purrr::set_names(hmos_ns)
hmos_ns

Timepoint_ns = purrr::set_names(Timepoint_ns)
Timepoint_ns

non_secretor_BMI_plots = map(hmos_ns,
                         ~map(Timepoint_ns, plot_fun_to_revise_bmi_ns, y = .x) )

non_secretor_BMI_plots

pdf("all_change_plots_bmi_non_secretors.pdf")
non_secretor_BMI_plots
dev.off()

