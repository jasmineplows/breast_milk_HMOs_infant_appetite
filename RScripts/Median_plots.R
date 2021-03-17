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

###Used for all cases
post_hoc_function_mixed <- function(hmo) {
  f <- paste0("`", hmo, "`", " ~ Timepoint*Secretor + (1|study_id)")
  fit <- lmer(f, data = data_for_median_plots)
  c <- emmeans(fit, ~ Timepoint*Secretor, adjust = "tukey")
  pairs(c, simple = "each") 
}

###Used for complete cases
my_fitting_function_complete <- function(hmo) {
  f <- paste0("`", hmo, "`", " ~ Timepoint + (1|study_id)")
  fit <- lmer(f, data = data_complete_cases_Secretors)
  aov <- anova(fit)
  return(aov)
}

post_hoc_function_complete <- function(hmo) {
  f <- paste0("`", hmo, "`", " ~ Timepoint + (1|study_id)")
  fit <- lmer(f, data = data_complete_cases_Secretors)
  c <- emmeans(fit, ~ Timepoint, adjust = "tukey")
  pairs(c, simple = "each") 
}

hmos = names(data_complete_cases_Secretors)[4:23]

hmos = purrr::set_names(hmos)
hmos


sink(file='myoutput.txt')
map(hmos, post_hoc_function_complete)
sink()

sink(file='myoutput2.txt')
map(hmos, my_fitting_function_complete)
sink()



hmos = names(data_for_median_plots)[4:23]

hmos = purrr::set_names(hmos)
hmos


sink(file='myoutput3.txt')
map(hmos, post_hoc_function_mixed)
sink()

plot_fun_to_revise = function(x, y) {
  ggplot(data = data_for_median_plots, aes(x = .data[[x]], y = .data[[y]], group = Secretor, linetype = Secretor)) +
    stat_summary(geom = "line", fun.data = median_hilow, size = 0.5) +
    stat_sum_df_all("median_hilow",
                    fun.args = (conf.int = 0.5),
                    linetype = "solid",
                    size = 0.5) +
    theme_classic() +
    theme(text = element_text(family = "ArialUnicodeMS", size = 10, colour = "black"),
          axis.line = element_line(colour = "black"),
          # aspect.ratio = 4/4,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust=0.5),
          # title = element_text(face = "bold", size = 10),
          # axis.title = element_text(face = "bold", size = 10),
          # axis.text.x = element_text(face = "bold", size = 10),
          # axis.text.y = element_text(face = "bold", size= 10),
          legend.position = c(0.5, 0.95),
          legend.direction = "horizontal",
          legend.text = element_text(size = 12),
          plot.margin = unit(c(1.0, 1.0, 2.0, 1.0), "lines"), #top, right, bottom, left
          legend.title=element_blank()) +
    labs(
      title = paste("Time p = ", format(my_fitting_function(y)[1,6],digits=2),", Secretor p = ", format(my_fitting_function(y)[2,6],digits=2), ", \nInteraction p = ", format(my_fitting_function(y)[3,6],digits=2), sep=""),
      x = "Time (months postpartum)"
    )
  # stat_compare_means(aes(group = Secretor), label = "p.signif")
  # label.y = c(8.5, 6.5, 6.5, 5.5, 5.6))
}


# which(colnames(data_for_median_plots)=="Timepoint")
hmos = names(data_for_median_plots)[4:23]
Timepoint = names(data_for_median_plots)[2]

hmos = purrr::set_names(hmos)
hmos

Timepoint = purrr::set_names(Timepoint)
Timepoint

