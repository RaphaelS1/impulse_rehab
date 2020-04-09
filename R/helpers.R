#--------------
# Title: Helper functions
# Description: Creates helper functions for use throughout analysis. Loads all required packages.
# Author: Raphael Sonabend (raphael.sonabend.15@ucl.ac.uk, GH: @RaphaelS1)
# Date: 09/04/20
# -------------

#----------------------
# Uncomment as required
#----------------------
# install.packages("tidyverse")
# install.packages("magrittr")
# install.packages("mlr")
# install.packages("mice")
# install.packages("VIM")
# install.packages("randomForest")
# install.packages("mlr3tuning")
# install.packages("paradox")
# install.packages("distr6")
# install.packages("mlr3proba")
# install.packages("mlr3")
# install.packages("survival")
# install.packages("ggplot2")
# install.packages("ggcorrplot")
# install.packages("tableone")
# install.packages("survminer")

#----------------------
# Load packages
#----------------------
# Data manipulation
library(tidyverse)
library(magrittr)
# Missing data
library(mice)
library(VIM)
# Modelling
library(randomForest)
library(mlr)
library(mlr3)
library(mlr3tuning)
library(mlr3proba)
library(paradox)
library(distr6)
library(survival)
# Visualisation & Exploration
library(ggplot2)
library(ggcorrplot)
library(tableone)
library(survminer)

#----------------------
# Helper functions
#----------------------
continuous_auto_test = function(df, var, unstrat = TRUE, ks = TRUE, wrs = TRUE, ethnicity = TRUE,
                                gender = TRUE, age = TRUE, other = NULL){
  try({

    var = df %>% select(var) %>% unlist() %>% as.numeric()

    factor = list()
    if(ethnicity) factor$ethnicity = df$Ethnicity
    if(gender) factor$gender = df$Gender
    if(!is.null(other)) factor$other = factor(df[[other]])

    res = try(lapply(factor, function(x){

      ks.res = c()
      wrs.res = c()

      for(i in 1:length(levels(x))){
        for(j in 1:length(levels(x))){
          if(i < j){
            if(ks)
              ks.res = c(ks.res, ks.test(var[x == levels(x)[i]],
                                         var[x == levels(x)[j]])$p.val)
            if(wrs)
              wrs.res = c(wrs.res, wilcox.test(var[x == levels(x)[i]],
                                               var[x == levels(x)[j]])$p.val)
          }

        }
      }

      return(list(ks = ks.res,
                  wrs = wrs.res))
    }))
    if(inherits(res, "try-error"))
      res = c()

    if(age)
      res = c(res, age = summary(lm(var ~ Age, df))$coefficients[2,4])

    if(unstrat)
      res = c(res, list(summary = summary(var)))

    return(res)

  }, silent = TRUE)
}

discrete_auto_test = function(df, var, unstrat = TRUE, ks = TRUE, wrs = TRUE, ethnicity = TRUE,
                              gender = TRUE, age = TRUE, binnedage = TRUE){
  try({

    variable = df %>% select(var) %>% unlist()

    ret = list()

    if(unstrat)
      ret = c(ret,
              list(tab = table(variable),
                   prop.tab = prop.table(table(variable)),
                   unstrat_p = chisq.test(table(variable))$p.val))

    if(ethnicity)
      ret = c(ret, list(ethnicity = chisq.test(table(df$Ethnicity, variable))$p.val))
    if(gender)
      ret = c(ret, list(gender = chisq.test(table(df$Gender, variable))$p.val))
    if(binnedage)
      ret = c(ret, list(binnedage = chisq.test(table(df$AgeBinned, variable))$p.val))
    if(age){
      ret = c(ret, list(age = continuous_auto_test(df, var = "Age", other = var, ethnicity =FALSE,
                                                   gender=FALSE, age = FALSE, unstrat = FALSE)))
    }

    return(ret)

  }, silent = TRUE)
}

#----------------------
# Load data
#----------------------
load_demos = function(file){
  df = readxl::read_excel(file, sheet = "Demography") %>%
    mutate(DOB = as.Date(DOB, format = "%d/%m/%Y"),
           Gender = factor(Gender),
           Ethnicity = factor(Ethnicity),
           Age = as.numeric(round((Sys.Date() - DOB)/365.25)),
           AgeBinned = cut(Age, breaks = seq.int(20,100,10))) %>%
    select(-DOB)
  levels(df$Ethnicity) = c(rep("Asian", 3), rep("Black", 2), "Asian", "Mixed",
                           rep("Other/Unknown", 3), rep("White", 3))
  return(df)
}
load_diagnosis = function(file){
  readxl::read_excel(file, sheet = "Diagnosis") %>%
    mutate(diagnosis_main = factor(diagnosis_main),
           diagnosis_cat = factor(diagnosis_cat),
           diagnosis_subcat = factor(diagnosis_subcat),
           diagnosis_local = factor(diagnosis_local),
           diagnosis_condition = factor(diagnosis_condition),
           derived_category = factor(case_when(
             grepl("Cerebral|ganglia|infarct", diagnosis_cat) ~ "CVE",
             grepl("Traumatic", diagnosis_cat) ~ "TBI",
             grepl("Spinal",diagnosis_cat) ~ "SCI",
             grepl("Functional", diagnosis_cat) ~ "FND",
             grepl("Multiple", diagnosis_cat) ~ "MS",
             grepl("Other", diagnosis_cat) ~ "Other",
           )))
}
load_outcome = function(file){
  df = readxl::read_excel(file, sheet = "Outcome") %>%
    mutate(fell = if_else(falls != 0, 1, 0),
           falls = as.numeric(falls),
           fall_cat = case_when(
             falls == 0 ~ "No_Falls",
             falls == 1 ~ "One_Fall",
             falls > 1  ~ "Recurrent_Faller"
           ))
  df[,3:13] = lapply(df[, 3:13], as.Date, format = "%d/%m/%Y")
  return(df)
}
load_cognitive = function(file, flipupps = FALSE){
  fliptab = data.frame(old = 0:16, new = 16:0)
  df = readxl::read_excel(file, sheet = "Cognitive Data") %>%
    select(-IRS_8)

  if (flipupps) {
    df = cbind(df %>% select(-starts_with("UPPS")),
               df %>% select(starts_with("UPPS"), -ends_with("Total")) %>%
                 apply(2, function(x) fliptab$new[match(x, fliptab$old)]) %>%
                 data.table::as.data.table() %>%
                 mutate(UPPS_Inf_Total = UPPS_Inf_PerS + UPPS_Inf_SS + UPPS_Inf_U + UPPS_Inf_PreM,
                        UPPS_Self_Total = UPPS_Self_PerS + UPPS_Self_SS + UPPS_Self_U + UPPS_Self_PreM))
  }

  return(df)
}
load_tests = function(file){
  readxl::read_excel(file, sheet = "NPDS FIM FAM NIS")
}
load_dates = function(file){
  readxl::read_excel(file, sheet = "Dates") %>%
    mutate(date_admission = as.Date(date_admission, format = "%d/%m/%Y"),
           date_assessment = as.Date(date_assessment, format = "%d/%m/%Y"),
           date_discharge = as.Date(date_discharge, format = "%d/%m/%Y"),
           date_firstonset = as.Date(date_firstonset, format = "%d/%m/%Y"),
           time_to_discharge = as.numeric(date_discharge - date_admission),
           time_onset_to_admission = as.numeric(date_admission - date_firstonset),
           time_to_assessment = as.numeric(date_assessment - date_admission)
    )
}
load_data = function(demos = TRUE, diagnosis = TRUE, outcome = TRUE,
                     cognitive = TRUE, tests = TRUE, dates = TRUE,
                     exclude_eligible = TRUE, exclude_notfit = TRUE, file){
  df = load_demos(file) %>% select(ID)

  if(demos)
    df %<>% left_join(load_demos(file))
  if(diagnosis)
    df %<>% left_join(load_diagnosis(file))
  if(outcome)
    df %<>% left_join(load_outcome(file))
  if(cognitive)
    df %<>% left_join(load_cognitive(file))
  if(tests)
    df %<>% left_join(load_tests(file))
  if(dates)
    df %<>% left_join(load_dates(file))

  if(exclude_eligible | exclude_notfit){
    df %<>% subset_data(exclude_eligible, exclude_notfit)
  }

  return(df)
}

load_regr_data = function(target){
  load_data() %>%
    select(-starts_with("date"), -falls, -fall_cat, -AgeBinned, -ID,
           -starts_with("diagnosis"), -fell, -ends_with("Imp"), -ends_with("Dis"), target,
           -starts_with("time")) %>%
    mutate(Ethnicity = factor(case_when(
      Ethnicity == "White" ~ "White",
      TRUE ~ "BAME"
    ))) %>%
    filter(!is.na(.data[[target]]))
}
load_imputed_data = function(target = NULL){
  df = read.csv("data/imputed_data.csv") %>%
    mutate(Ethnicity = factor(case_when(
      Ethnicity == "White" ~ "White",
      TRUE ~ "BAME"
    )),
    date_admission = as.Date(date_admission),
    date_assessment = as.Date(date_assessment),
    date_discharge = as.Date(date_discharge),
    date_firstonset = as.Date(date_firstonset)
    )

  df$X = NULL
  df[,64:74] = lapply(df[, 64:74], as.Date)

  if (!is.null(target)) {
    df %<>%
      select(-starts_with("date"),  -starts_with("diagnosis"), -ends_with("Imp"), -ends_with("Dis"),
             -starts_with("time"), target)
  }

  return(df)
}

#----------------------
# Data manipulation
#----------------------
complete_case = function(df){
  df = df[,!unlist(lapply(df, function(x) (sum(is.na(x))/nrow(df))*100 > 10))]
  df = df[complete.cases(df), ]
  return(df)
}
subset_data = function(df, filter_eligible = TRUE, filter_notfit = TRUE){
  df %<>% left_join(readxl::read_excel(file, sheet = "Eligibility"))

  if(filter_eligible)
    df %<>% filter(eligible == "Y")
  if(filter_notfit)
    df %<>% filter(assessment_completed == "Y")

  df %<>% select(-eligible, -assessment_completed)

  return(df)
}

#----------------------
# mlr(2) helper
#----------------------
make_regr_learners = function(){
  lrn = makeTuneWrapper(
    makeLearner("regr.randomForestSRC"),
    makeResampleDesc("CV", iters = 3),
    list(rmse, mae),
    makeParamSet(
      makeDiscreteParam("ntree", values = c(100, 250, 500)),
      makeDiscreteParam("nodesize", value = c(1, 5, 10))
    ),
    makeTuneControlGrid()
  )
  lrn = c(list(lrn), makeLearners(c("featureless","cvglmnet","lm","svm"), type = "regr"))
  lrn$regr.featureless$par.vals = list(method = "median")
  return(lrn)
}
