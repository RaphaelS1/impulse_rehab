#--------------
# Title: Exploration of Diagnosis Features
# Description: Visualisation and exploratory analysis of diagnosis variables: `diagnosis_main`,
# `derived_category`, `diagnosis_local`, and `diagnosis_condition`.
# Author: Raphael Sonabend (raphael.sonabend.15@ucl.ac.uk, GH: @RaphaelS1)
# Date: 09/04/20
# -------------

source("R/helpers.R")

df = load_diagnosis() %>% left_join(load_demos()) %>% subset_data()

#---------------
# Main Diagnosis
#---------------
discrete_auto_test(df, "diagnosis_main")

ggplot(df, aes(x = diagnosis_main, fill = diagnosis_main)) + geom_bar(aes(y=..count../sum(..count..))) +
  labs(y = "Proportion", title = "Main Diagnosis Distribution") + theme(axis.title.x=element_blank(),
                                                                axis.text.x=element_blank(),
                                                                axis.ticks.x=element_blank())
dev.copy(png, "plots/diagnosis_main.png", width = 7, height = 5, units = "in", res = 100)
dev.off()

#--------------------
# Diagnosis Category
#--------------------
# Unstratified
discrete_auto_test(df, "derived_category")

ggplot(df, aes(x = derived_category, fill = derived_category)) + geom_bar(aes(y=..count../sum(..count..))) +
  labs(y = "Proportion", title = "Diagnosis Category Distribution") + theme(axis.title.x=element_blank(),
                                                                        axis.text.x=element_blank(),
                                                                        axis.ticks.x=element_blank())
dev.copy(png, "results/plots/derived_category.png", width = 7, height = 5, units = "in", res = 100)
dev.off()

ggplot(df %>% select(Age, derived_category)) +
  geom_histogram(aes(x = Age, y = ..density..), fill = "white", color = "blue") +
  geom_density(aes(Age, group = derived_category, color = derived_category),
               df %>% select(Age, derived_category) %>%
                 filter(derived_category == "FND")) +
  theme(legend.position="top")
dev.copy(png, "results/plots/derived_category_age.png", width = 7, height = 5, units = "in", res = 100)
dev.off()

#--------------------
# Diagnosis Sub-Category
#--------------------
# Unstratified
discrete_auto_test(df, "diagnosis_subcat")

#--------------------
# Diagnosis Location
#--------------------
# Unstratified
discrete_auto_test(df, "diagnosis_local")

#--------------------
# Diagnosis Condition
#--------------------
discrete_auto_test(df, "diagnosis_condition")
