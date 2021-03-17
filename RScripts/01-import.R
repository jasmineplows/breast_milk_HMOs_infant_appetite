options(max.print=10000000)

#Packages
library(tidyverse)
library(tidymodels)
library(Hmisc)
library(emmeans)
library(car)
library(rstatix)
library(ggplot2)
library(ggpubr)
library(lmerTest)
library(emojifont)
library(patchwork)
library(Gmisc)
library(reshape2)
library(kableExtra)
library(gridExtra)
library(emmeans)
library(ecoflux)
library(showtext)
font_add("ArialUnicodeMS", "/Library/Fonts/Arial Unicode.ttf")  # Use the actual file path
showtext_auto()
font_add("Helvetica", "/System/Library/Fonts/Helvetica.ttc")
showtext_auto()

##### Load in data ########
dataSet <- read.delim("../Datasets/mothers_milk_data_new_hmos_added_predicted_fat_lean_added.txt",quote="", header=TRUE,check.names = FALSE)