#--------------
# Title: Exploration of UPPS Features
# Description: Visualisation and exploratory analysis of UPPS features, more detailed than
# `exploration_cognitive.R`
# Author: Raphael Sonabend (raphael.sonabend.15@ucl.ac.uk, GH: @RaphaelS1)
# Date: 09/04/20
# -------------

source("R/helpers.R")

df = load_cognitive(file, flipupps = TRUE) %>%
  select(ID, starts_with("UPPS")) %>%
  left_join(load_demos(file)) %>%
  left_join(load_outcome(file) %>%
              select(ID, fell)) %>%
  subset_data()

#---------------------
# UPPS Self
#---------------------
continuous_auto_test(df, "UPPS_Self_PreM")
continuous_auto_test(df, "UPPS_Self_U")
continuous_auto_test(df, "UPPS_Self_SS")
continuous_auto_test(df, "UPPS_Self_PerS")
continuous_auto_test(df, "UPPS_Self_Total")

df_fell = df %>% filter(fell == 1)
continuous_auto_test(df_fell, "UPPS_Self_PreM")
continuous_auto_test(df_fell, "UPPS_Self_U")
continuous_auto_test(df_fell, "UPPS_Self_SS")
continuous_auto_test(df_fell, "UPPS_Self_PerS")
continuous_auto_test(df_fell, "UPPS_Self_Total")

df_nofell = df %>% filter(fell == 0)
continuous_auto_test(df_nofell, "UPPS_Self_PreM")
continuous_auto_test(df_nofell, "UPPS_Self_U")
continuous_auto_test(df_nofell, "UPPS_Self_SS")
continuous_auto_test(df_nofell, "UPPS_Self_PerS")
continuous_auto_test(df_nofell, "UPPS_Self_Total")

round(wilcox.test(df$UPPS_Self_PreM[df$fell == 1], df$UPPS_Self_PreM[df$fell == 0], conf.int = TRUE)$conf.int, 2)
round(wilcox.test(df$UPPS_Self_PreM[df$fell == 1], df$UPPS_Self_PreM[df$fell == 0], conf.int = TRUE)$estimate, 2)

round(wilcox.test(df$UPPS_Self_U[df$fell == 1], df$UPPS_Self_U[df$fell == 0], conf.int = TRUE)$conf.int, 2)
round(wilcox.test(df$UPPS_Self_U[df$fell == 1], df$UPPS_Self_U[df$fell == 0], conf.int = TRUE)$estimate, 2)

round(wilcox.test(df$UPPS_Self_SS[df$fell == 1], df$UPPS_Self_SS[df$fell == 0], conf.int = TRUE)$conf.int, 2)
round(wilcox.test(df$UPPS_Self_SS[df$fell == 1], df$UPPS_Self_SS[df$fell == 0], conf.int = TRUE)$estimate, 2)

round(wilcox.test(df$UPPS_Self_PerS[df$fell == 1], df$UPPS_Self_PerS[df$fell == 0], conf.int = TRUE)$conf.int, 2)
round(wilcox.test(df$UPPS_Self_PerS[df$fell == 1], df$UPPS_Self_PerS[df$fell == 0], conf.int = TRUE)$estimate, 2)

round(wilcox.test(df$UPPS_Self_Total[df$fell == 1], df$UPPS_Self_Total[df$fell == 0], conf.int = TRUE)$conf.int, 2)
round(wilcox.test(df$UPPS_Self_Total[df$fell == 1], df$UPPS_Self_Total[df$fell == 0], conf.int = TRUE)$estimate, 2)

#---------------------
# UPPS Inf
#---------------------
chisq.test(table(data.frame(df$Gender, is.na(df$UPPS_Inf_Total))))
chisq.test(table(data.frame(df$Ethnicity, is.na(df$UPPS_Inf_Total))))
ks.test(df$Age[is.na(df$UPPS_Inf_Total)], df$Age[!is.na(df$UPPS_Inf_Total)])

continuous_auto_test(df, "UPPS_Inf_PreM")$summary
continuous_auto_test(df, "UPPS_Inf_U")$summary
continuous_auto_test(df, "UPPS_Inf_SS")$summary
continuous_auto_test(df, "UPPS_Inf_PerS")$summary
continuous_auto_test(df, "UPPS_Inf_Total")$summary

df_fell = df %>% filter(fell == 1)
continuous_auto_test(df_fell, "UPPS_Inf_PreM")$summary
continuous_auto_test(df_fell, "UPPS_Inf_U")$summary
continuous_auto_test(df_fell, "UPPS_Inf_SS")$summary
continuous_auto_test(df_fell, "UPPS_Inf_PerS")$summary
continuous_auto_test(df_fell, "UPPS_Inf_Total")$summary

df_nofell = df %>% filter(fell == 0)
continuous_auto_test(df_nofell, "UPPS_Inf_PreM")$summary
continuous_auto_test(df_nofell, "UPPS_Inf_U")$summary
continuous_auto_test(df_nofell, "UPPS_Inf_SS")$summary
continuous_auto_test(df_nofell, "UPPS_Inf_PerS")$summary
continuous_auto_test(df_nofell, "UPPS_Inf_Total")$summary

round(wilcox.test(df$UPPS_Inf_PreM[df$fell == 1], df$UPPS_Inf_PreM[df$fell == 0], conf.int = TRUE)$conf.int, 2)
round(wilcox.test(df$UPPS_Inf_PreM[df$fell == 1], df$UPPS_Inf_PreM[df$fell == 0], conf.int = TRUE)$estimate, 2)

round(wilcox.test(df$UPPS_Inf_U[df$fell == 1], df$UPPS_Inf_U[df$fell == 0], conf.int = TRUE)$conf.int, 2)
round(wilcox.test(df$UPPS_Inf_U[df$fell == 1], df$UPPS_Inf_U[df$fell == 0], conf.int = TRUE)$estimate, 2)

round(wilcox.test(df$UPPS_Inf_SS[df$fell == 1], df$UPPS_Inf_SS[df$fell == 0], conf.int = TRUE)$conf.int, 2)
round(wilcox.test(df$UPPS_Inf_SS[df$fell == 1], df$UPPS_Inf_SS[df$fell == 0], conf.int = TRUE)$estimate, 2)

round(wilcox.test(df$UPPS_Inf_PerS[df$fell == 1], df$UPPS_Inf_PerS[df$fell == 0], conf.int = TRUE)$conf.int, 2)
round(wilcox.test(df$UPPS_Inf_PerS[df$fell == 1], df$UPPS_Inf_PerS[df$fell == 0], conf.int = TRUE)$estimate, 2)

round(wilcox.test(df$UPPS_Inf_Total[df$fell == 1], df$UPPS_Inf_Total[df$fell == 0], conf.int = TRUE)$conf.int, 2)
round(wilcox.test(df$UPPS_Inf_Total[df$fell == 1], df$UPPS_Inf_Total[df$fell == 0], conf.int = TRUE)$estimate, 2)

