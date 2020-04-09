#--------------
# Title: Exploration of FIM, FIMFAM, NPDS, NIS
# Description: Visualisation and exploratory analysis of FIM, FIMFAM, NPDS, and NIS scores.
# Author: Raphael Sonabend (raphael.sonabend.15@ucl.ac.uk, GH: @RaphaelS1)
# Date: 09/04/20
# -------------

source("R/helpers.R")

df = load_tests() %>%
  select(ID, ends_with("Imp"), ends_with("Adm")) %>%
  left_join(load_demos()) %>% subset_data()

#---------------------
# Histograms
#---------------------
p = ggplot(df, aes(y=..density..)) + labs(y = "Density")

p1 = p + labs(x = "NPDS Admission") +
  geom_histogram(aes(x = NPDS_Adm), binwidth = 5, color = "black", fill = "white")

p2 = p + labs(x = "FIM Sum Admission") +
  geom_histogram(aes(x = FIM_Sum_Adm), binwidth = 10, color = "black", fill = "white")

p3 = p + labs(x = "FIMFAM Sum Admission") +
  geom_histogram(aes(x = FIMFAM_Sum_Adm), binwidth = 10, color = "black", fill = "white")

p4 = p + labs(x = "NIS Admission") +
  geom_histogram(aes(x = NIS_Adm), binwidth = 3, color = "black", fill = "white")

gridExtra::grid.arrange(p1,p2,p3,p4)
dev.copy(png, "results/plots/tests_hists.png", width = 6, height = 6, units = "in", res = 100)
dev.off()

#-----------------
# NPDS
#-----------------
continuous_auto_test(df, "NPDS_Adm")
continuous_auto_test(df, "NPDS_Imp")
#-----------------
# FIM_Motor
#-----------------
continuous_auto_test(df, "FIM_Motor_Adm")
continuous_auto_test(df, "FIM_Motor_Imp")
#-----------------
# FIM_Cog
#-----------------
continuous_auto_test(df, "FIM_Cog_Adm")
continuous_auto_test(df, "FIM_Cog_Imp")
#-----------------
# FIM_Sum
#-----------------
continuous_auto_test(df, "FIM_Sum_Adm")
continuous_auto_test(df, "FIM_Sum_Imp")
#-----------------
# FIMFAM_Motor
#-----------------
continuous_auto_test(df, "FIMFAM_Motor_Adm")
continuous_auto_test(df, "FIMFAM_Motor_Imp")
#-----------------
# FIMFAM_Cog
#-----------------
continuous_auto_test(df, "FIMFAM_Cog_Adm")
continuous_auto_test(df, "FIMFAM_Cog_Imp")
#-----------------
# FIMFAM_Sum
#-----------------
continuous_auto_test(df, "FIMFAM_Sum_Adm")
continuous_auto_test(df, "FIMFAM_Sum_Imp")
#-----------------
# NIS
#-----------------
continuous_auto_test(df, "NIS_Adm")
continuous_auto_test(df, "NIS_Imp")

#---------------------
# Correlations
#---------------------
cor = Hmisc::rcorr(as.matrix(df[,-c(1,18:21)]))
cor$P[is.na(cor$P)] = 0
cor$P = matrix(round(p.adjust(cor$P, method = "BH"), 2), nrow = nrow(cor$r),
               ncol = ncol(cor$r), dimnames = dimnames(cor$r))

ggcorrplot(round(cor$r, 2), p.mat = cor$P,type = "lower", outline.color = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"), lab = TRUE, insig = "blank",
           title = "Correlation Heatmap for FIM, FIMFAM, NIS, and  NPDS",
           tl.cex = 10, digits = 2, lab_size = 3)
dev.copy(png, "results/plots/tests_corr.png", width = 7, height = 7, units = "in", res = 100)
dev.off()
