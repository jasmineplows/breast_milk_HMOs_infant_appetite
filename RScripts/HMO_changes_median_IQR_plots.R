###Functions
stat_sum_df_all <- function(fun, geom="pointrange",...) {
  stat_summary(fun.data=fun, geom=geom, ...)
}

my_fitting_function <- function(hmo) {
  f <- paste0("`", hmo, "`", " ~ Timepoint*Secretor + (1|study_id)")
  fit <- lmer(f, data = data_for_median_plots)
  aov <- anova(fit)
  return(aov)
}

plot_fun = function(x, y) {
  ggplot(data = data_for_median_plots, aes(x = .data[[x]], y = .data[[y]], group = Secretor, colour = Secretor)) +
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



#########Median plots#########
data_for_median_plots <- dataSet %>%
  drop_na(Secretor) %>%
  rename_at(vars(ends_with("_ug_ml")),
            funs(str_replace(., "_ug_ml", "")))
  # make_clean_names()

# which(colnames(data_for_median_plots)=="Timepoint")
hmos = names(data_for_median_plots)[4:23]
Timepoint = names(data_for_median_plots)[2]

hmos = purrr::set_names(hmos)
hmos

Timepoint = purrr::set_names(Timepoint)
Timepoint

#Example analysis
# example <- my_fitting_function("LNT_pct")
# models <- purrr::map(hmos, my_fitting_function)

#Example plot
# example_plot <- plot_fun("Timepoint", "LNT_pct")

all_plots = map(hmos,
                ~map(Timepoint, plot_fun, y = .x) )

all_plots

pdf("all_change_plots_medians.pdf")
all_plots
dev.off()
