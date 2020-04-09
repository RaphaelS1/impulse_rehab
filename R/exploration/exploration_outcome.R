#--------------
# Title: Exploration of Outcome
# Description: Visualisation and exploratory analysis of outcome variables: `falls` and `fell`.
# Author: Raphael Sonabend (raphael.sonabend.15@ucl.ac.uk, GH: @RaphaelS1)
# Date: 09/04/20
# -------------

source("R/helpers.R")

df = load_outcome(file = file) %>% left_join(load_demos(file = file)) %>% subset_data()

#--------------
# Number falls
#--------------
continuous_auto_test(df, "falls")

#--------------
# Fall Y/N
#--------------
discrete_auto_test(df, "fell")

#--------------
# Fall by length stay
#--------------
df %<>% left_join(
  readxl::read_excel("data/alessia data - modified.xlsx", sheet = "Dates") %>%
    mutate(date_admission = as.Date(date_admission, format = "%d/%m/%Y"),
           date_discharge = as.Date(date_discharge, format = "%d/%m/%Y"),
           time_to_discharge = as.numeric(date_discharge - date_admission)) %>%
    select(-starts_with("date"))
) %>%
  mutate(falls_per_day = as.numeric(falls/time_to_discharge),
         falls_per_100_day = falls_per_day * 100)

continuous_auto_test(df, "falls_per_100_day")
plot(distr6::Empirical$new(df$falls_per_100_day), fun = "cdf", main = "CDF of Falls Per 100 Days",
     xlab = "Number of Falls")
dev.copy(png, "results/plots/cdf_falls.png", width = 7, height = 5, units = "in", res = 100)
dev.off()

#--------------
# Time to first fall
#--------------
df %<>% left_join(
  readxl::read_excel("data/alessia data - modified.xlsx", sheet = "Dates") %>%
    mutate(date_admission = as.Date(date_admission, format = "%d/%m/%Y"),
           date_discharge = as.Date(date_discharge, format = "%d/%m/%Y")) %>%
    select(ID, date_admission, date_discharge)
  ) %>%
  mutate(survival_date = if_else(is.na(date_fall1), date_discharge, date_fall1),
         time = as.numeric(survival_date - date_admission))

ggsurvplot(survfit(Surv(time, fell) ~ 1, data = df), risk.table = TRUE,
           ylab = "Probability of not falling",
           title = "Kaplan-Meier Estimate of Falling Distribution",
           legend = "none")
dev.copy(png, "results/plots/surv_fall.png", width = 7, height = 5, units = "in", res = 100)
dev.off()

survdiff(Surv(time, fell) ~ Gender, data = df)
survdiff(Surv(time, fell) ~ Ethnicity, data = df)
survdiff(Surv(time, fell) ~ AgeBinned, data = df)

ggsurvplot(survfit(Surv(time, fell) ~ AgeBinned, data = df),
           ylab = "Probability of not falling",
           title = "Kaplan-Meier Estimate of Falling Distribution stratified by Age")
dev.copy(png, "plots/surv_fall_age.png", width = 7, height = 5, units = "in", res = 100)
dev.off()

#-----------
# Falls/100 days
#-----------
df = load_outcome(file = file) %>%
  left_join(load_demos(file = file)) %>%
  left_join(load_dates(file = file))
sum(df$falls, na.rm = T)/sum(df$time_to_discharge, na.rm = T) * 100
