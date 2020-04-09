#--------------
# Title: Correlation of UPPS and DKEF
# Description: Correlation analysis of UPPS and DKEF scores.
# Author: Raphael Sonabend (raphael.sonabend.15@ucl.ac.uk, GH: @RaphaelS1)
# Date: 09/04/20
# -------------

df = load_data(file = file, diagnosis = F, outcome = F, dates = F, demos = F) %>%
  select(starts_with("UPPS"), starts_with("dkef"))
cor = Hmisc::rcorr(as.matrix(df))
cor$P[is.na(cor$P)] = 0
cor$P = matrix(round(p.adjust(cor$P, method = "BH"), 2), nrow = nrow(cor$r),
               ncol = ncol(cor$r), dimnames = dimnames(cor$r))

ggcorrplot(round(cor$r, 2), p.mat = cor$P,type = "lower", outline.color = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"), lab = FALSE, insig = "blank",
           title = "Correlation Heatmap for DKEF and UPPS",
           tl.cex = 9)
dev.copy(png, "results/plots/dkef_upps_corr.png", width = 6, height = 6, units = "in", res = 100)
dev.off()
