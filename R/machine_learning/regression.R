#--------------
# Title: Regression Predictions
# Description: Predicting improvement in scores using mlr(2).
# Author: Raphael Sonabend (raphael.sonabend.15@ucl.ac.uk, GH: @RaphaelS1)
# Date: 09/04/20
# Note: Some functions used in this script are not currently publically available but will be
#       released in the near-future.
# -------------

#--------------------------------
# Common parts
#--------------------------------
msr = list(mse, mse.se, mae, mae.se, rmse, rmse.se)
rsmp = makeResampleDesc("CV", iters = 2)
set.seed(13)
#--------------------------------
# Task 1: Complete case analysis
#--------------------------------
df = load_regr_data("NIS_Imp") %>% complete_case()
task = makeRegrTask("FIM_Imp", df, "NIS_Imp")
lrn = make_regr_learners()
complete_bm = benchmark(lrn, task, rsmp, msr)
comparisontest(complete_bm)

#-----------------------------------
# Task 2: Imputation by Random Forest
#-----------------------------------
df = load_regr_data("NIS_Imp")
task = makeRegrTask("FIM_Imp", df, "NIS_Imp")
lrn = make_regr_learners()[1:2]
cartimpute_bm = benchmark(lrn, task, rsmp, msr)
comparisontest(cartimpute_bm)

#-----------------------------------
# Task 3: Multiple Imputation
#-----------------------------------
df = load_imputed_data("FIMFAM_Sum_Imp") %>% select(-FIMFAM_Sum_Imp)
task = makeRegrTask("FIM_imputed_Imp", df, "NIS_Imp")
lrn = make_regr_learners()
imputed_bm = benchmark(lrn, task, rsmp, msr)
comparisontest(imputed_bm)

# ------------------
# Variable importance
# ------------------
task = makeRegrTask("FIM_imputed_Imp", df, "FIMFAM_Sum_Imp")

lrn = makeTuneWrapper(
  makeLearner("regr.randomForest"),
  makeResampleDesc("CV", iters = 3),
  list(rmse, mae),
  makeParamSet(
    makeDiscreteParam("ntree", values = c(100, 250, 500)),
    makeDiscreteParam("nodesize", value = c(1, 5, 10))
  ),
  makeTuneControlGrid()
)

trained_model = train(lrn, task)
fit = trained_model$learner.model$next.model$learner.model
varImpPlot(fit)
