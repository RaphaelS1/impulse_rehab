#--------------
# Title: Exploration of Demographics Features
# Description: Visualisation and exploratory analysis of demographic variables: `Gender`,
# `Ethnicity`, `Age`.
# Author: Raphael Sonabend (raphael.sonabend.15@ucl.ac.uk, GH: @RaphaelS1)
# Date: 09/04/20
# -------------

source("R/helpers.R")

df = load_demos() %>% subset_data()

tableOne = CreateTableOne(colnames(df)[2:4], data = df, includeNA = TRUE)
tableOne <- print(tableOne,  quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(tableOne, file = "results/tables/tableone.csv")
rm(tableOne)

# Gender
discrete_auto_test(df, "Gender", gender = F, ethnicity = F, age = F, binnedage = F)

ggplot(df, aes(x = Gender, fill = Gender)) + geom_bar(aes(y=..count../sum(..count..))) +
  labs(y = "Proportion", title = "Gender Distribution") + theme(axis.title.x=element_blank(),
                                 axis.text.x=element_blank(),
                                 axis.ticks.x=element_blank())
dev.copy(png, "plots/gender.png", width = 7, height = 5, units = "in", res = 100)
dev.off()

# Ethnicity
discrete_auto_test(df, "Ethnicity", gender = F, ethnicity = F, age = F, binnedage = F)
chisq.test(table(df$Ethnicity[df$Ethnicity!="White"]))

ggplot(df, aes(x = Ethnicity, fill = Ethnicity)) + geom_bar(aes(y=..count../sum(..count..))) +
  labs(y = "Proportion", title = "Grouped Ethnicity Distribution") + theme(axis.title.x=element_blank(),
                                 axis.text.x=element_blank(),
                                 axis.ticks.x=element_blank())
dev.copy(png, "plots/ethnicity_grouped.png", width = 7, height = 5, units = "in", res = 100)
dev.off()

# Age vs. Gender and Ethnicity
continuous_auto_test(df, "Age", age = FALSE)

# Gender vs. Ethnicity
discrete_auto_test(df, "Gender", gender = FALSE, age = FALSE, unstrat = FALSE)

