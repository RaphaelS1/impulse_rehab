#--------------
# Title: Cox PH and Hazard Ratios
# Description: Hazard ratios and variable importance with Cox PH.
# Author: Raphael Sonabend (raphael.sonabend.15@ucl.ac.uk, GH: @RaphaelS1)
# Date: 09/04/20
# -------------

source("R/helpers.R")

# -------------
#  Benchmark Cox and Kaplan
# -------------

df = load_imputed_data() %>%
  mutate(survival_date = if_else(is.na(date_fall1), date_discharge, date_fall1),
         survival_time = as.numeric(survival_date - date_admission)) %>%
  select(-starts_with("date"),-ends_with("Dis"),-ends_with("Imp"),
         -starts_with("time"), -starts_with("diagnosis"), -falls, -fall_cat,
         -survival_date, -ID, -AgeBinned, -ends_with("SS"), -ends_with("Per"))


task = TaskSurv$new(id = "falls", backend = df, event = "fell", time = "survival_time")
lrn = lrns(c("surv.coxph","surv.kaplan"))
rsmp = rsmp("cv", folds = 3)
set.seed(42)
design = benchmark_grid(task, lrn, rsmp)
bm = mlr3::benchmark(design)
bmr  = bm$aggregate(msr("surv.harrellC"))

# -------------
#  Inspection of Cox wrt FND
# -------------

lrn = lrn("surv.coxph")
task = TaskSurv$new(id = "falls", backend = df, event = "fell", time = "survival_time")
lrn$train(task)
p = lrn$predict(task)
set.seed(42)
fnd = sample(which(df$derived_category == "FND"), 5)
nfnd = sample(which(df$derived_category != "FND"), 5)
par(mfrow = c(1,2))
plot(p$distr[fnd[1]], fun = "s", ylim = c(0,1), col = 1,
     main = "FND Survival")
lines(p$distr[fnd[2]], fun = "s", ylim = c(0,1), col = 2)
lines(p$distr[fnd[3]], fun = "s", ylim = c(0,1), col = 3)
lines(p$distr[fnd[4]], fun = "s", ylim = c(0,1), col = 4)
lines(p$distr[fnd[5]], fun = "s", ylim = c(0,1), col = 5)

plot(p$distr[nfnd[1]], fun = "s", ylim = c(0,1), col = 1,
     main = "Non-FND Survival")
lines(p$distr[nfnd[2]], fun = "s", ylim = c(0,1), col = 2)
lines(p$distr[nfnd[3]], fun = "s", ylim = c(0,1), col = 3)
lines(p$distr[nfnd[4]], fun = "s", ylim = c(0,1), col = 4)
lines(p$distr[nfnd[5]], fun = "s", ylim = c(0,1), col = 5)

# -------------
#  Get an out-of-sample estimate using hold-out
# -------------

set.seed(1)
df = df[!is.na(df$survival_time),]
train = sample(1:nrow(df), nrow(df)*(2/3))
test = setdiff(1:nrow(df), train)
lrn = lrn("surv.coxph")
task = TaskSurv$new(id = "falls", backend = df, event = "fell", time = "survival_time")
lrn$train(task, row_ids = train)
p = lrn$predict(task, row_ids = test)
# 49% - in line with above
p$score()

# -------------
#  Get hazard ratios
# -------------
# dealing with multi-collinearity
df %<>% select(-ends_with("Total"), -contains("Sum"))
fit = coxph(Surv(survival_time, fell)~., data = df)
sumfit = summary(fit)
# testing PH
round(p.adjust(cox.zph(fit)$table[,3], method = "BH"),2 )

write.csv(data.frame(round(sumfit$conf.int[,c(1,3:4)], 2),
                     p = round(p.adjust(sumfit$coefficients[,5], method = "BH"), 2)),
          "results/tables/coxph.csv")
