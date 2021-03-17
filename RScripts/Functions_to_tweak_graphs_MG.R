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

plot_fun_to_revise = function(x, y) {
  ggplot(data = data_for_median_plots, aes(x = .data[[x]], y = .data[[y]], group = Secretor, colour = Secretor)) +
    stat_summary(geom = "line", fun.data = median_hilow, size = 1.8) +
    stat_sum_df_all("median_hilow",
                    fun.args = (conf.int = 0.5),
                    linetype = "solid",
                    size = 1.5) +
    theme_bw(base_size = 20) +
    theme(axis.line = element_line(colour = "black"),
          # aspect.ratio = 4/4,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust=0.5),
          title = element_text(face = "bold", size = 50),
          axis.title = element_text(face = "bold", size = 50),
          axis.text.x = element_text(face = "bold", size = 50),
          axis.text.y = element_text(face = "bold", size= 50),
          legend.position = c(0.5, 0.95),
          legend.direction = "horizontal",
          legend.text = element_text(size = 50),
          # plot.margin = unit(c(1.0, 4.1, 4.1, 2.1), "lines"),
          legend.title=element_blank()) +
    labs(
      title = paste("Time p = ", format(my_fitting_function(y)[1,6],digits=2),", Secretor p = ", format(my_fitting_function(y)[2,6],digits=2), ", \nInteraction p = ", format(my_fitting_function(y)[3,6],digits=2), sep=""),
      x = "Infant Age (Months)"
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