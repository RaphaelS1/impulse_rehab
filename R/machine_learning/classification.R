#--------------
# Title: Classification Predictions
# Description: Predicting the probability of falling using mlr(2).
# Author: Raphael Sonabend (raphael.sonabend.15@ucl.ac.uk, GH: @RaphaelS1)
# Date: 09/04/20
# -------------

source("R/helpers.R")
#--------------------------------
# Task 1: Complete case analysis
#--------------------------------
df = load_data() %>%
  select(-starts_with("date"), -falls, -fall_cat, -AgeBinned, -ID, -starts_with("diagnosis")) %>%
  mutate(fell = factor(fell),
         Ethnicity = factor(case_when(
           Ethnicity == "White" ~ "White",
           TRUE ~ "BAME"
         )))


df = df[,!unlist(lapply(df, function(x) (sum(is.na(x))/94)*100 > 10))]
df = df[complete.cases(df), ]
task = makeClassifTask("binaryClassif", df, "fell", positive = 1)
lrn = makeLearners(c("logreg","glmnet","randomForest","featureless","lda"), type = "classif", predict.type = "prob")
msr = list(brier, brier.se, logloss, logloss.se, acc, acc.se,tpr, tpr.se,ppv, ppv.se)
rsmp = makeResampleDesc("CV", iters = 2)
set.seed(42)
bm = benchmark(lrn, task, rsmp, msr)
bm
comparisontest(bm)
#-----------------------------------
# Task 2: Imputation by Random Forest
#-----------------------------------
df = load_data() %>%
  select(-starts_with("date"), -falls, -fall_cat, -AgeBinned, -ID, -starts_with("diagnosis")) %>%
  mutate(fell = factor(fell),
         Ethnicity = factor(case_when(
           Ethnicity == "White" ~ "White",
           TRUE ~ "BAME"
         )))
task = makeClassifTask("binaryClassif_missing", df, "fell", positive = 1)
lrn = makeTuneWrapper(
  makeLearner("classif.randomForestSRC", predict.type = "prob"),
  makeResampleDesc("CV", iters = 3),
  list(brier, logloss),
  makeParamSet(
    makeDiscreteParam("ntree", values = c(100, 250, 500)),
    makeDiscreteParam("nodesize", value = c(1, 5, 10))
  ),
  makeTuneControlGrid()
)
lrn = c(list(lrn), makeLearners(c("featureless"), type = "classif", predict.type = "prob"))
set.seed(42)
bm = benchmark(lrn, task, rsmp, msr)
bm
comparisontest(bm)
#-----------------------------------
# Task 3: Multiple Imputation
#-----------------------------------
df = load_imputed_data() %>%
  select(-starts_with("date"), -falls, -fall_cat, -AgeBinned, -ID, -starts_with("diagnosis")) %>%
  mutate(fell = factor(fell))
df = df[complete.cases(df),]

task = makeClassifTask("binary_imputed_Classif", df, "fell", positive = 1)

lrn = makeTuneWrapper(
  makeLearner("classif.randomForest", predict.type = "prob"),
  makeResampleDesc("CV", iters = 3),
  list(brier, logloss),
  makeParamSet(
    makeDiscreteParam("ntree", values = c(100, 250, 500)),
    makeDiscreteParam("nodesize", value = c(1, 5, 10))
  ),
  makeTuneControlGrid()
)
lrn = c(list(lrn), makeLearners(c("logreg","glmnet","featureless","lda"),
                   type = "classif", predict.type = "prob"))
set.seed(42)
msr = list(brier, brier.se, logloss, logloss.se, acc, acc.se,tpr, tpr.se,ppv, ppv.se)
bm = mlr::benchmark(lrn, task, rsmp, msr)
res = as.data.frame(data.table::rbindlist(getBMRAggrPerformances(bm)))
rownames(res) = sapply(bm$measures, function(x) x$id)
res = t(round(res, 3))
write.csv(res,"outcome_classif.csv")
p.adjustframe(comparisontest(bm)$binary_imputed_Classif$comparison)
