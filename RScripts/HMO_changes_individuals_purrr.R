##########Looped plots################
data_for_looped_plots <- dataSet %>%
  drop_na(Secretor) %>%
  rename_at(vars(ends_with("_ug_ml")),
            funs(str_replace(., "_ug_ml", "")))

hmos = names(data_for_looped_plots)[4:23]
Timepoint = names(data_for_looped_plots)[2]

hmos = purrr::set_names(hmos)
hmos

Timepoint = purrr::set_names(Timepoint)
Timepoint

individual_plot_fun = function(x, y) {
  ggplot(data = data_for_looped_plots, aes(x = .data[[x]], y = .data[[y]], group = study_id, colour = Secretor)) +
    geom_line() +
    theme_bw() +
    theme(legend.title=element_blank()) +
    labs(
      x = x,
      y = paste(y, "(\u03BCg/mL)"))
}

#Example plot
# individual_plot_fun("Timepoint", "LNFP II [ug/mL]")

all_individual_plots = map(hmos,
                ~map(Timepoint, individual_plot_fun, y = .x) )

pdf("all_change_plots.pdf")
all_individual_plots
dev.off()
