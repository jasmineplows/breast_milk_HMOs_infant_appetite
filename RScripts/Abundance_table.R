###Functions
abundance_function <- function(hmo) {
  f <- paste0("`", hmo, "`", " ~ Timepoint*Secretor + (1|study_id)")
  fit <- lmer(f, data = data_for_abundance_table)
  aov <- anova(fit)
  return(aov)
}

###Data
data_for_abundance_table <- dataSet %>%
  drop_na(Secretor) %>%
  select(-"SUM_pct") %>%
  rename_at(vars(ends_with("_pct")),
            funs(str_replace(., "_pct", "")))


###Format
# which(colnames(data_for_abundance_table)=="Timepoint")
hmos_ab = names(data_for_abundance_table)[25:42]
Timepoint_ab = names(data_for_abundance_table)[2]

hmos_ab = purrr::set_names(hmos_ab)
hmos_ab

Timepoint_ab = purrr::set_names(Timepoint_ab)
Timepoint_ab


###Put it all together
abundance_table <- hmos_ab %>%
  map(abundance_function) %>%
  map("Pr(>F)") %>%
  map2_dfr(.x = names(hmos_ab),
           .f = ~tibble("HMO"=.x,
                        "Timepoint"=.y[1],
                        "Secretor"=.y[2],
                        "Interaction"=.y[3],
           )) %>%
  arrange(HMO)


abundance_table$Timepoint <- format.pval(abundance_table$Timepoint, eps = 0.001, digits = 2)
abundance_table$Secretor <- format.pval(abundance_table$Secretor, eps = 0.001, digits = 2)
abundance_table$Interaction <- format.pval(abundance_table$Interaction, eps = 0.001, digits = 2)

####Look at one HMO to determine direction of effect
fit <- lmer(`6SL` ~ Timepoint*Secretor + (1|study_id), data = data_for_abundance_table)
aov <- anova(fit)
aov

lsmeans(fit, pairwise~Timepoint*Secretor, adjust="tukey")
