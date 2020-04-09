#--------------
# Title: Data Imputation
# Description: Imputes data for analysis throughout the study.
# Author: Raphael Sonabend (raphael.sonabend.15@ucl.ac.uk, GH: @RaphaelS1)
# Date: 09/04/20
# -------------

df = load_data(dates = FALSE, outcome = FALSE, file = file) %>%
  select(-AgeBinned, -ends_with("Imp"), -ends_with("Total"), -ends_with("Sum"), -ID, dkef_total)
set.seed(1)
impdf = mice(df, m = 5, maxit = 10, method = "pmm", seed = 42)
imputed = complete(impdf)
imputed %<>%
  mutate(NPDS_Imp = NPDS_Adm - NPDS_Dis,
         FIM_Motor_Imp = FIM_Motor_Dis - FIM__Motor_Adm,
         FIM_Cog_Imp = FIM_Cog_Dis - FIM_Cog_Adm,
         FIM_Sum_Imp = FIM_Cog_Imp + FIM_Motor_Imp,
         FIMFAM_Motor_Imp = FIMFAM_Motor_Dis - FIMFAM_Motor_Adm,
         FIMFAM_Cog_Imp = FIMFAM_Cog_Dis - FIMFAM_Cog_Adm,
         FIMFAM_Sum_Imp = FIMFAM_Cog_Imp + FIMFAM_Motor_Imp,
         NIS_Imp = NIS_Dis - NIS_Adm,
         UPPS_Self_Total = UPPS_Self_PerS + UPPS_Self_PreM + UPPS_Self_SS + UPPS_Self_U,
         UPPS_Inf_Total = UPPS_Inf_PerS + UPPS_Inf_PreM + UPPS_Inf_SS + UPPS_Inf_U,
         AgeBinned = cut(Age, breaks = seq.int(20,100,10)))

df = load_data(demos = FALSE, diagnosis = FALSE, outcome = TRUE,
               cognitive = FALSE, tests = FALSE, dates = TRUE,
               exclude_eligible = TRUE, exclude_notfit = TRUE, file = file)
imputed %<>% cbind(df)


write.csv(imputed, file = "data/imputed_data.csv")
