#--------------
# Title: Survival Analysis Predictions
# Description: Predicting the time until falling using mlr3.
# Author: Raphael Sonabend (raphael.sonabend.15@ucl.ac.uk, GH: @RaphaelS1)
# Date: 09/04/20
# -------------

source("R/helpers.R")
set.seed(1)

#--------------
# Common data, measures, learners, resample
#--------------
df = load_data() %>%
  mutate(survival_date = if_else(is.na(date_fall1), date_discharge, date_fall1),
         survival_time = as.numeric(survival_date - date_admission)) %>%
  select(-starts_with("date"),-ends_with("Dis"),-ends_with("Imp"),
         -starts_with("time"), -starts_with("diagnosis"), -falls, -fall_cat,
         -survival_date, -ID, -AgeBinned, -ends_with("SS"), -ends_with("Per"))

terminator = term("evals")
tuner = tnr("grid_search")
param_tuned = AutoTuner$new(lrn("surv.parametric"), rsmp("cv", folds = 3), msr("surv.harrellC"),
                   ParamSet$new(
                     list(ParamFct$new(id = "dist", levels = c("weibull")),
                          ParamFct$new(id = "type", levels = c("ph", "aft")))
                   ), terminator, tuner)
ranger_tuned = AutoTuner$new(lrn("surv.ranger"), rsmp("cv", folds = 3), msr("surv.harrellC"),
                            ParamSet$new(
                              list(ParamInt$new(id = "num.trees", lower = 100, upper = 500),
                                   ParamDbl$new(id = "min.node.size", lower = 1, upper = 10))
                            ), terminator, tuner)
glm_tuned = AutoTuner$new(lrn("surv.cvglmnet"), rsmp("cv", folds = 3), msr("surv.harrellC"),
                             ParamSet$new(
                               list(ParamDbl$new(id = "alpha", lower = 0, 1))
                             ), terminator, tuner)

lrn = lapply(paste0("surv.", c("kaplan","coxph")), lrn)
lrn = c(lrn, list(param_tuned), list(ranger_tuned), list(glm_tuned))

msr = lapply(paste0("surv.", c("logloss","loglossSE","graf","grafSE","harrellC")), msr)

rsmp = rsmp("cv", folds = 3)

#--------------------------------
# Task 1: Complete case analysis
#--------------------------------
df_complete = df %>% complete_case()
task = TaskSurv$new(id = "falls", backend = df_complete, event = "fell", time = "survival_time")
lrn = lapply(paste0("surv.", c("kaplan","coxph","cvglmnet")), lrn)
lrn[[3]]$param_set$values = list(alpha = 0.3333)
design = benchmark_grid(task, lrn, rsmp)
set.seed(4)
bmr = benchmark(design)
bmr$aggregate(msr)

#-----------------------------------
# Task 2: Imputation by Random Forest
#-----------------------------------
lrn = lrn("surv.randomForestSRC")
lrn$param_set$values = list(na.action = "na.impute")
lrn = c(lrn, lrn('surv.kaplan'))
task = TaskSurv$new(id = "falls", backend = df, event = "fell", time = "survival_time")
design = benchmark_grid(task, lrn, rsmp)
set.seed(42)
bmr = benchmark(design)
bmr$aggregate(msr)
#-----------------------------------
# Task 3: Multiple Imputation
#-----------------------------------
set.seed(1)
impdf = mice(df, m = 5, maxit = 10, method = "pmm", seed = 42)
task = TaskSurv$new(id = "falls", backend = complete(impdf), event = "fell", time = "survival_time")
lrn[[3]] = NULL
rsmp = rsmp("cv", folds = 5)
design = benchmark_grid(task, lrn, rsmp)
set.seed(10)
bm = benchmark(design)
bm$aggregate(msr("surv.harrellC"))
