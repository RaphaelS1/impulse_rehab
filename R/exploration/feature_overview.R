#--------------
# Title: Feature Overview Table
# Description: Creates an overview table of features with mean(sd) and count(prop).
# Author: Raphael Sonabend (raphael.sonabend.15@ucl.ac.uk, GH: @RaphaelS1)
# Date: 09/04/20
# -------------

source("R/helpers.R")

df = load_data(file = file)

fallers = df$fell==1
nonfallers = df$fell==0

meansdfall = function(variable, fallers, nonfallers, varname){
  df = data.frame(fallers = paste(round(mean(variable[fallers], na.rm = TRUE), 1), "\u00b1",  round(sd(variable[fallers], na.rm = TRUE), 1)),
                  nonfallers = paste(round(mean(variable[nonfallers], na.rm = TRUE), 1), "\u00b1",  round(sd(variable[nonfallers], na.rm = TRUE), 1)),
                  all = paste(round(mean(variable, na.rm = TRUE),1), "\u00b1",  round(sd(variable, na.rm = TRUE), 1)))
  rownames(df) = varname
  return(df)
}

countpropfall = function(variable, fallers, nonfallers){
  df = data.frame(fallers = paste0(table(variable[fallers]), " (", round(prop.table(table(variable[fallers]))*100), ")"),
       nonfallers = paste0(table(variable[nonfallers]), " (", round(prop.table(table(variable[nonfallers]))*100), ")"),
       all = paste0(table(variable), " (", round(prop.table(table(variable))*100), ")"))
  rownames(df) = levels(variable)
  return(df)
}

#------------------------
# Age (years)
#------------------------
tab = meansdfall(df$Age, fallers, nonfallers, "Age")

#------------------------
# Gender
#------------------------
tab = rbind(tab, countpropfall(df$Gender, fallers, nonfallers))

#------------------------
# Ethnicity
#------------------------
tab = rbind(tab, countpropfall(df$Ethnicity, fallers, nonfallers))

#------------------------
# Diagnosis
#------------------------
tab = rbind(tab, countpropfall(df$derived_category, fallers, nonfallers))

#------------------------
# Length of stay
#------------------------
tab = rbind(tab, meansdfall(df$time_to_discharge, fallers, nonfallers, "Length_Stay"))

#------------------------
# FIM Total
#------------------------
tab = rbind(tab, meansdfall(df$FIM_Sum_Adm, fallers, nonfallers, "FIM_Sum_Adm"))
tab = rbind(tab, meansdfall(df$FIM_Sum_Imp, fallers, nonfallers, "FIM_Sum_Imp"))
tab = rbind(tab, meansdfall(df$FIMFAM_Sum_Adm, fallers, nonfallers, "FIMFAM_Sum_Adm"))
tab = rbind(tab, meansdfall(df$FIMFAM_Sum_Imp, fallers, nonfallers, "FIMFAM_Sum_Imp"))

#------------------------
# DKEF Total
#------------------------
tab = rbind(tab, meansdfall(df$dkef_TC1_RS, fallers, nonfallers, "DKEF_TC1"))
tab = rbind(tab, meansdfall(df$dkef_TC2_RS, fallers, nonfallers, "DKEF_TC2"))
tab = rbind(tab, meansdfall(df$dkef_TC3_RS, fallers, nonfallers, "DKEF_TC3"))
tab = rbind(tab, meansdfall(df$dkef_TC4_RS, fallers, nonfallers, "DKEF_TC4"))
tab = rbind(tab, meansdfall(df$dkef_TC5_RS, fallers, nonfallers, "DKEF_TC5"))

write.csv(tab, "results/tables/plos_table_2")


#------------------------
# DKEF Wilcox
#------------------------
wilcox.test(df$dkef_TC1_RS[fallers], df$dkef_TC1_RS[nonfallers], conf.int = TRUE)
wilcox.test(df$dkef_TC2_RS[fallers], df$dkef_TC2_RS[nonfallers], conf.int = TRUE)
wilcox.test(df$dkef_TC3_RS[fallers], df$dkef_TC3_RS[nonfallers], conf.int = TRUE)
wilcox.test(df$dkef_TC4_RS[fallers], df$dkef_TC4_RS[nonfallers], conf.int = TRUE)
wilcox.test(df$dkef_TC5_RS[fallers], df$dkef_TC5_RS[nonfallers], conf.int = TRUE)

ks.test(df$dkef_TC1_RS[fallers], df$dkef_TC1_RS[nonfallers])
ks.test(df$dkef_TC2_RS[fallers], df$dkef_TC2_RS[nonfallers])
ks.test(df$dkef_TC3_RS[fallers], df$dkef_TC3_RS[nonfallers])
ks.test(df$dkef_TC4_RS[fallers], df$dkef_TC4_RS[nonfallers])
ks.test(df$dkef_TC5_RS[fallers], df$dkef_TC5_RS[nonfallers])
