#--------------
# Title: Exploration of Cognitive Features
# Description: Visualisation and exploratory analysis of cognitive variables, i.e. DKEFS, UPPS, IRS.
# Author: Raphael Sonabend (raphael.sonabend.15@ucl.ac.uk, GH: @RaphaelS1)
# Date: 09/04/20
# -------------

source("R/helpers.R")

df = load_cognitive(file = file, flipupps = TRUE) %>%
  select(ID, dkef_total, dkef_TC4_RS, UPPS_Self_Total, UPPS_Inf_Total, IRS_items_reduced) %>%
  mutate(UPPS_Dif_Total = UPPS_Self_Total - UPPS_Inf_Total,
         IRS_items_reduced = factor(IRS_items_reduced),
         dkef_TC4_RS = as.numeric(dkef_TC4_RS)) %>%
  left_join(load_demos()) %>%
  subset_data()

#---------------------
# Histograms
#---------------------
p = ggplot(df, aes(y=..density..)) + labs(y = "Density")
p1 = p + labs(x = "DKEF Total") +
  geom_histogram(aes(x = dkef_total), binwidth = 3, color = "black", fill = "white")
p2 = p + labs(x = "UPPS Self Total") +
  geom_histogram(aes(x = UPPS_Self_Total), binwidth = 3, color = "black", fill = "white")
p3 = p + labs(x = "UPPS Informant Total") +
  geom_histogram(aes(x = UPPS_Inf_Total), binwidth = 3, color = "black", fill = "white")
p4 = p + labs(x = "UPPS Difference Total") +
  geom_histogram(aes(x = UPPS_Dif_Total), binwidth = 3, color = "black", fill = "white")
p5 = p + labs(x = "DKEF TC4") +
  geom_histogram(aes(x = dkef_TC4_RS), binwidth = 3, color = "black", fill = "white")

gridExtra::grid.arrange(p1,p2,p3,p4)
dev.copy(png, "results/plots/cognitive_hists.png", width = 6, height = 6, units = "in", res = 100)
dev.off()

#---------------------
# Correlations
#---------------------
cor = Hmisc::rcorr(as.matrix(df[,2:5]))
cor$P[is.na(cor$P)] = 0
cor$P = matrix(round(p.adjust(cor$P, method = "BH"), 2), nrow = nrow(cor$r),
               ncol = ncol(cor$r), dimnames = dimnames(cor$r))

ggcorrplot(round(cor$r, 2), p.mat = cor$P,type = "lower", outline.color = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"), lab = TRUE, insig = "blank",
           title = "Correlation Heatmap for UPPS, DKEF, and IRS")
dev.copy(png, "results/plots/cognitive_corr.png", width = 6, height = 6, units = "in", res = 100)
dev.off()
#---------------
# dkef totals
#---------------
continuous_auto_test(df, "dkef_total")

#---------------
# dkef TC4
#---------------
continuous_auto_test(df, "dkef_TC4_RS")

#---------------
# UPPS_Self_Total
#---------------
continuous_auto_test(df, "UPPS_Self_Total")

#---------------
# UPPS_Inf_Total
#---------------
continuous_auto_test(df, "UPPS_Inf_Total")
#---------------
# UPPS_Dif_Total
#---------------
continuous_auto_test(df, "UPPS_Dif_Total")

#---------------
# IRS_items_reduced
#---------------
discrete_auto_test(df, "IRS_items_reduced")

ggplot(df, aes(x = IRS_items_reduced, fill = IRS_items_reduced)) + geom_bar(aes(y=..count../sum(..count..))) +
  labs(y = "Proportion", title = "IRS Items Reduced Distribution") + theme(axis.title.x=element_blank(),
                                                                                axis.text.x=element_blank(),
                                                                                axis.ticks.x=element_blank())
dev.copy(png, "results/plots/irs_reduced.png", width = 7, height = 5, units = "in", res = 100)
dev.off()
#---------------
# Correlations and tests by FND
#---------------
df %<>% left_join(load_diagnosis() %>%
                    mutate(FND = if_else(derived_category == "FND", 1, 0)) %>%
                    select(ID, FND))

# FND = 1
cor = Hmisc::rcorr(as.matrix(df[df$FND == 1,2:5]))
cor$P[is.na(cor$P)] = 0
cor$P = matrix(round(p.adjust(cor$P, method = "BH"), 2), nrow = nrow(cor$r),
               ncol = ncol(cor$r), dimnames = dimnames(cor$r))
cor$P

# FND = 0
cor = Hmisc::rcorr(as.matrix(df[df$FND == 0,2:5]))
cor$P[is.na(cor$P)] = 0
cor$P = matrix(round(p.adjust(cor$P, method = "BH"), 2), nrow = nrow(cor$r),
               ncol = ncol(cor$r), dimnames = dimnames(cor$r))
cor$P
cor$r

ks.test(as.numeric(unlist(subset(df, FND == 1, select = dkef_total))),
        as.numeric(unlist(subset(df, FND == 0, select = dkef_total))))
ks.test(as.numeric(unlist(subset(df, FND == 1, select = UPPS_Self_Total))),
        as.numeric(unlist(subset(df, FND == 0, select = UPPS_Self_Total))))
ks.test(as.numeric(unlist(subset(df, FND == 1, select = UPPS_Inf_Total))),
        as.numeric(unlist(subset(df, FND == 0, select = UPPS_Inf_Total))))
ks.test(as.numeric(unlist(subset(df, FND == 1, select = UPPS_Dif_Total))),
        as.numeric(unlist(subset(df, FND == 0, select = UPPS_Dif_Total))))

wilcox.test(as.numeric(unlist(subset(df, FND == 1, select = dkef_total))),
        as.numeric(unlist(subset(df, FND == 0, select = dkef_total))))
wilcox.test(as.numeric(unlist(subset(df, FND == 1, select = UPPS_Self_Total))),
        as.numeric(unlist(subset(df, FND == 0, select = UPPS_Self_Total))))
wilcox.test(as.numeric(unlist(subset(df, FND == 1, select = UPPS_Inf_Total))),
        as.numeric(unlist(subset(df, FND == 0, select = UPPS_Inf_Total))))
wilcox.test(as.numeric(unlist(subset(df, FND == 1, select = UPPS_Dif_Total))),
        as.numeric(unlist(subset(df, FND == 0, select = UPPS_Dif_Total))))

continuous_auto_test(filter(df, FND == 1), "dkef_total")
continuous_auto_test(filter(df, FND == 1), "UPPS_Self_Total")
continuous_auto_test(filter(df, FND == 1), "UPPS_Inf_Total")
continuous_auto_test(filter(df, FND == 1), "UPPS_Dif_Total")

continuous_auto_test(filter(df, FND == 0), "dkef_total")
continuous_auto_test(filter(df, FND == 0), "UPPS_Self_Total")
continuous_auto_test(filter(df, FND == 0), "UPPS_Inf_Total")
continuous_auto_test(filter(df, FND == 0), "UPPS_Dif_Total")
