colors <- c("#f17048", "#89c9dd", "#c0d084", "#d2cfc8", "#f8a090", "#e4a4bc", "#b07c8c", "#5c9454", "#84549c", "#a4bc44", "#f59138", "#8ad1ba", "#ef553c", "#f4ad84", "#f89599", "#ffb0b9", "#f9a2bf", "#cfa0be", "#ad5b8f", "#815799", "#508ab2", "#bfe2d6", "#9bd4a7", "#589651", "#a4bf43", "#ecdcc5")

col2 <- c("#d851e5",
          "#56b219",
          "#5330cf",
          "#2ab53c",
          "#c346f0",
          "#ed6114",
          "#4a51de",
          "#e22f2f",
          "#596cf3",
          "#f62590",
          "#311ea9",
          "#d939bd",
          "#4032b6",
          "#9e34c0",
          "#530eb0",
          "#a260f1",
          "#7817a5",
          "#8a3adf",
          "#753fc2")

col3 <- c("#ffab94",
          "#2ad1ed",
          "#e4a271",
          "#5ff2f7",
          "#e992ae",
          "#6df2e5",
          "#d299ce",
          "#b0e6a4",
          "#d2b0f7",
          "#aaffdc",
          "#ffbcaf",
          "#6bb3e4",
          "#ffe7b1",
          "#b1d1ff",
          "#adaf75",
          "#a7a9cb",
          "#7ab997",
          "#befff4",
          "#aeae8c")

###########Abundance plot################
data_for_abundance_plot <- dataSet %>%
  drop_na(Secretor) %>%
  select(-"SUM_pct") %>%
  pivot_longer(
    cols = ends_with("_pct"),
    names_to = "HMO",
    values_to = "Abundance",
    values_drop_na = TRUE
  ) %>%
  mutate(HMO=str_replace_all(HMO,"_pct", "")) %>%
  group_by(HMO, Timepoint, Secretor) %>%
  summarise(mean.abundance = mean(Abundance, na.rm=TRUE))

abundance_plot = ggplot(data_for_abundance_plot, aes(x = Timepoint, y = mean.abundance, fill = HMO)) + 
  geom_bar(stat = "identity", colour = "black") + 
  theme_bw() +
  theme(legend.title=element_blank()) +
  facet_grid(. ~ Secretor) +
  theme(axis.title.x = element_text(colour = "black", size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"), 
        axis.text.x = element_text(size = 16, colour = "black", vjust = 0.5, hjust = 1, face= "bold"),
        axis.text.y = element_text(colour = "black", size = 16, face = "bold"),
        legend.text = element_text(size = 16, face = "bold", colour = "black"),
        strip.text = element_text(colour = "black", size = 16, face = "bold")
        ) + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(x = "Time (months postpartum)", y = "Relative Abundance (%)", fill = "HMO") +
  # hues::scale_fill_iwanthue(
  #   hmin = 0,
  #   hmax = 360,
  #   cmin = 75,
  #   cmax = 100,
  #   lmin = 25,
  #   lmax = 70
  # )
  scale_fill_manual(
    values = colors,
    breaks = c("2FL", "3FL", "3SL", "6SL", "DFLac", "DFLNH", "DFLNT", "DSLNH", "DSLNT", "FDSLNH", "FLNH", "LNFPI", "LNFPII", "LNFPIII", "LNH", "LNnT", "LNT", "LSTb", "LSTc"),
    labels = c("2'FL", "3FL", "3'SL", "6'SL", "DFL", "DFLNH", "DFLNT", "DSLNH", "DSLNT", "FDSLNH", "FLNH", "LNFPI", "LNFPII", "LNFPIII", "LNH", "LNnT", "LNT", "LSTb", "LSTc"))


abundance_plot
