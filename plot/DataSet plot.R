rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot
library(tree)
library(readxl)
library(rpart)
Obesity_WOUT_TEXT <- read_excel("C:/Users/nicol/Desktop/Obesity_WOUT_TEXT.xlsx", 
                                col_types = c("numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric"))
#View(Obesity_WOUT_TEXT)

# Classification tree

#View(ObesityDataSet_raw_and_data_sinthetic)

Obesity <- na.omit(Obesity_WOUT_TEXT); # remove row with nan;
View(Obesity)
Obesity$Problem <- factor( ifelse(Obesity$NObeyesdad <=3  , " No " , " Yes " ))
barplot(table(Obesity$Gender), main = "Gender Histogram",ylab = "Frequency")
barplot(table(Obesity$family_history_with_overweight),
        main = "family history with overweight Histogram",ylab = "Frequency")
barplot(table(Obesity$FCCI), main = "FCCI Histogram",ylab = "Frequency")
barplot(table(Obesity$MFP), main = "MFP Histogram",ylab = "Frequency")
barplot(table(Obesity$SMOKE), main = "Smoke Histogram",ylab = "Frequency")
barplot(table(Obesity$CALC), main = "CALC Histogram",ylab = "Frequency")
barplot(table(Obesity$Problem), main = "Obesity problem Histogram",ylab = "Frequency") 
barplot(table(Obesity$SCC), main = "SCC Histogram",ylab = "Frequency")
barplot(table(Obesity$Nobeyesdad_text), main = "Obesity level Histogram",ylab = "Frequency")

?hist
hist(ObesityDataSet_raw_and_data_sinthetic$V5)
Obesity_eating <- Obesity[ -c(1:6,9,10,12:14,16:22) ]
boxplot(Obesity_eating, main="BoxPlot eating habits")
Obesity_physic <- Obesity[ -c(1:12,15:22) ]
boxplot(Obesity_physic, main="BoxPlot physical conditions")
Obesity_gen<- Obesity[ -c(1:2,4:22) ]
boxplot(Obesity_gen, main="BoxPlot Height")
boxplot(Obesity$Age,main="BoxPlot Age")
boxplot(Obesity$Weight,main="BoxPlot Weight")
boxplot(Obesity$family_history_with_overweight,main="BoxPlot family history with overweight")
