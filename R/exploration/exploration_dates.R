#--------------
# Title: Exploration of Date-Related Features
# Description: Visualisation and exploratory analysis of time-related variables: `time_to_discharge`,
# `time_to_assessment`, `time_onset_to_admission`.
# Author: Raphael Sonabend (raphael.sonabend.15@ucl.ac.uk, GH: @RaphaelS1)
# Date: 09/04/20
# -------------

source("R/helpers.R")

df = load_dates() %>% left_join(load_demos()) %>% subset_data()

#---------------------
# Histograms
#---------------------
p = ggplot(df, aes(y=..density..)) + labs(y = "Density")
p1 = p + labs(x = "Time to Discharge") +
  geom_histogram(aes(x = time_to_discharge), binwidth = 15, color = "black", fill = "white")
p2 = p + labs(x = "Time to Assessment") +
  geom_histogram(aes(x = time_to_assessment), binwidth = 15, color = "black", fill = "white")
p3 = p + labs(x = "Time to Admission from Onset") +
  geom_histogram(aes(x = time_onset_to_admission), binwidth = 500, color = "black", fill = "white")
gridExtra::grid.arrange(p1,p2,p3)
dev.copy(png, "results/plots/time_hists.png", width = 6, height = 7, units = "in", res = 100)
dev.off()


#---------------------
# Time to discharge
#---------------------
continuous_auto_test(df, "time_to_discharge")


#---------------------
# Time to assessment
#---------------------
continuous_auto_test(df, "time_to_assessment")

df2 = df
df2 = df2[!is.na(df$Ethnicity) & !is.na(df$time_to_assessment), ]
hist(df2$time_to_assessment, col = "gray",freq =F, ylim = c(0,0.05))
lines(density(df2$time_to_assessment[df2$Ethnicity == "White"]), col = "red", lwd =2)
lines(density(df2$time_to_assessment[df2$Ethnicity == "Other/Unknown"]), col = "blue", lwd =2)

#---------------------
# Time to Onset
#---------------------
continuous_auto_test(df, "time_onset_to_admission")

plot(lm(time_onset_to_admission ~ Age, df))
plot(df$Age, df$time_onset_to_admission, xlab = "Age", ylab = "Time to Admission from Onset",
     main = "Linear Regression Plot of Time to Admission from Onset vs Age")
abline(lm(time_onset_to_admission ~ Age, df), lwd = 2, col = 2)
legend("topright", lwd = 2, lty = c(1,0,0), col = 2,
       legend = c("Intercept = 1094.29", "Slope = -12.85", "p <0.01"))
dev.copy(png, "results/plots/time_onset_lm.png", width = 7, height = 6, units = "in", res = 100)
dev.off()

summary(lm(time_onset_to_admission ~ Age,
   df[!(df$time_onset_to_admission > 2000 & !is.na(df$time_onset_to_admission)),]))
Hmisc::rcorr(df$time_onset_to_admission, df$Age)
Hmisc::rcorr(df$time_onset_to_admission[!(df$time_onset_to_admission > 2000 & !is.na(df$time_onset_to_admission))],
             df$Age[!(df$time_onset_to_admission > 2000 & !is.na(df$time_onset_to_admission))])
