###########Plot together################
data_for_plot <- dataSet %>%
  drop_na(Secretor) %>%
  pivot_longer(
    cols = ends_with("_ug_ml"),
    names_to = "HMO",
    values_to = "Concentration",
    values_drop_na = TRUE
  ) %>%
  mutate(HMO=str_replace_all(HMO,"_ug_ml", "")) %>%
  group_by(HMO, timepoint, Secretor) %>%
  summarise(mean.concentration = mean(Concentration, na.rm=TRUE))
  
plot_with_SUM <- ggplot(data = data_for_plot, aes(x = timepoint, y = mean.concentration, group = HMO, colour = HMO))

plot_with_SUM <- plot_with_SUM + geom_line() +
  theme_bw() +
  theme(legend.title=element_blank()) +
  facet_grid(. ~ Secretor) +
  xlab("Months of Age") +
  ylab("Average Concentration (\u03BCg/mL)") +
  hues::scale_colour_iwanthue(
    hmin = 0,
    hmax = 360,
    cmin = 30,
    cmax = 80,
    lmin = 35,
    lmax = 80,
  )

ggexport(plot_with_SUM,
         filename = "combined_plot_with_SUM.pdf")
         # width = 20,
         # height = 28,
         # unit = "in",
         # dpi = 300)

#########Plot together with SUM Removed#############
data_for_plot_SUM_removed <- dataSet %>%
  drop_na(Secretor) %>%
  select(-"SUM_ug_ml") %>%
  pivot_longer(
    cols = ends_with("_ug_ml"),
    names_to = "HMO",
    values_to = "Concentration",
    values_drop_na = TRUE
    ) %>%
  mutate(HMO=str_replace_all(HMO,"_ug_ml", "")) %>%
  group_by(HMO, timepoint, Secretor) %>%
  summarise(mean.concentration = mean(Concentration, na.rm=TRUE))

data_for_plot_SUM_removed$HMO <- str_remove(data_for_plot_SUM_removed$HMO, "_ug_mL")

plot_without_SUM <- ggplot(data = data_for_plot_SUM_removed, aes(x = timepoint, y = mean.concentration, group = HMO, colour = HMO))

plot_without_SUM <- plot_without_SUM + geom_line() +
  theme_bw() +
  theme(legend.title=element_blank()) +
  facet_grid(. ~ Secretor) +
  xlab("Months of Age") +
  ylab("Average Concentration (\u03BCg/mL)") +
  hues::scale_colour_iwanthue(
    hmin = 0,
    hmax = 360,
    cmin = 30,
    cmax = 80,
    lmin = 35,
    lmax = 80,
  )

ggexport(plot_without_SUM,
         filename = "combined_plot_without_SUM.pdf")
