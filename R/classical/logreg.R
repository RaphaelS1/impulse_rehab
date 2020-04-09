#--------------
# Title: Logistic Regression and Odds Ratios
# Description: Analysis of DKEF and UPPS odds ratios.
# Author: Raphael Sonabend (raphael.sonabend.15@ucl.ac.uk, GH: @RaphaelS1)
# Date: 09/04/20
# -------------

source("R/helpers.R")
#---------------
# DKEF
#---------------
df = load_data(file = file, dates = FALSE) %>%
  select(Age, Ethnicity, Gender, fell, ends_with("RS"),
         -starts_with("UPPS"))

fit = glm(fell~., data = df, family = binomial())
round(exp(cbind(coef(fit), confint(fit))), 2)[8:12, ]

df = load_data(file = file, dates = FALSE) %>% select(fell, ends_with("RS"), -starts_with("UPPS"))
fit = glm(fell~., data = df, family = binomial())
round(exp(cbind(coef(fit), confint(fit))), 2)[2:6,]

#---------------
# UPPS
#---------------
# Self-Reported - Adjusted
df = load_data(file = file, dates = FALSE) %>%
  select(Age, Ethnicity, Gender, fell, contains("Self"), -UPPS_Self_Total)
fit = glm(fell~., data = df, family = binomial())
round(exp(cbind(coef(fit), confint(fit))), 2)[8:11,]

# Self-Reported - Unadjusted
df = load_data(file = file, dates = FALSE) %>% select(fell, contains("Self"), -UPPS_Self_Total)
fit = glm(fell~., data = df, family = binomial())
round(exp(cbind(coef(fit), confint(fit))), 2)[2:5, ]

# Inf-Reported - Adjusted
df = load_data(file = file, dates = FALSE) %>% select(Age, Ethnicity, Gender, fell,
                                         contains("Inf"), -UPPS_Inf_Total)
fit = glm(fell~., data = df[complete.cases(df), ], family = binomial)
round(exp(cbind(coef(fit), confint(fit))), 2)[8:11,]

# Inf-Reported - Unadjusted
df = load_data(file= file, dates = FALSE) %>% select(fell, contains("Inf"), -UPPS_Inf_Total)
fit = glm(fell~., data = df, family = binomial())
round(exp(cbind(coef(fit), confint(fit))), 2)[2:5, ]
