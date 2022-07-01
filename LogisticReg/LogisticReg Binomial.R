rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot
library(readxl)
library(rpart)
library(readxl)
Obesity_WOUT_TEXT <- read_excel("D:/UNI/SL/Obesity_WOUT_TEXT.xlsx", 
                                col_types = c("numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric"))
#View(Obesity_WOUT_TEXT)

#View(ObesityDataSet_raw_and_data_sinthetic)
sum(is.na(Obesity));
Obesity <- na.omit(Obesity_WOUT_TEXT); # remove row with nan;
Obesity$Problem <- factor( ifelse(Obesity$NObeyesdad <=3  , " No " , " Yes " ))
head(Obesity)
set.seed(123)
train <- sample(1:nrow(Obesity),floor(nrow(Obesity)*0.5))
train_Obesity = Obesity[train, ]
test_Obesity = Obesity[-train, ]
#SULLE ABITUDINI ALIMENTRI
log_reg <- glm( Obesity$Problem ~FCCI+ NP+ CWater
                + FCV + CALC + MFP ,
                data =  Obesity , family = binomial)
summary(log_reg)
fit <- predict( log_reg , type = "response")
clas <- rep ( " No " , 2111)
clas[fit > .5] = " Yes ";
table(clas, Obesity$Problem)
err.rate=(table(clas, Obesity$Problem)[1,2]+
                 table(clas, Obesity$Problem)[2,1])/
             sum(table(clas, Obesity$Problem))
err.rate
#NP,Cwater e CALC sono poco utili
log_reg <- glm( Obesity$Problem ~FCCI
                + FCV + MFP ,
                data =  Obesity , family = binomial)
summary(log_reg)
fit <- predict( log_reg , type = "response")
clas <- rep ( " No " , 2111)
clas[fit > .5] = " Yes ";
table(clas, Obesity$Problem)
err.rate=(table(clas, Obesity$Problem)[1,2]+
            table(clas, Obesity$Problem)[2,1])/
  sum(table(clas, Obesity$Problem))
err.rate
#leggero miglioramento
#SULLE ABITUDINI LEGATE AL FISICO
log_reg <- glm(  Obesity$Problem ~SCC+FAF + TUE +Public_Transportation 
                + Walking+Automobile +
                  Motorbike	+Bike ,
                data =  Obesity , family = binomial)
summary(log_reg)
fit <- predict( log_reg , type = "response")
clas <- rep ( " No " , 2111)
clas[fit > .5] = " Yes ";
table(clas, Obesity$Problem)
err.rate=(table(clas, Obesity$Problem)[1,2]+
            table(clas, Obesity$Problem)[2,1])/
  sum(table(clas, Obesity$Problem))
err.rate
#le abitudini di trasporto sono poco influenti
log_reg <- glm(  Obesity$Problem ~SCC+FAF + TUE ,
                data =  Obesity , family = binomial)
summary(log_reg)
fit <- predict( log_reg , type = "response")
clas <- rep ( " No " , 2111)
clas[fit > .5] = " Yes ";
table(clas, Obesity$Problem)
err.rate=(table(clas, Obesity$Problem)[1,2]+
            table(clas, Obesity$Problem)[2,1])/
  sum(table(clas, Obesity$Problem))
err.rate

#CONSIDERIAMO TUTTI I REGRESSORI TRANNE ALTEZZA E PESO
log_reg <- glm(  Obesity$Problem ~FCCI+ FCV + CALC +MFP+ SCC + FAF + TUE +Public_Transportation 
                 + Walking+Automobile +Motorbike	+Bike +Age 
                 +Obesity$family_history_with_overweight
                 +Obesity$SMOKE +Obesity$Gender,
                 data =  Obesity , family = binomial)
summary(log_reg)
fit <- predict( log_reg , type = "response")
clas <- rep ( " No " , 2111)
clas[fit > .6] = " Yes ";
table(clas, Obesity$Problem)
err.rate=(table(clas, Obesity$Problem)[1,2]+
            table(clas, Obesity$Problem)[2,1])/
  sum(table(clas, Obesity$Problem))
err.rate

#togliamo i regressori meno influenti
log_reg <- glm(  Obesity$Problem ~FCCI+ FCV +MFP+ SCC + FAF +Age 
                 +Obesity$family_history_with_overweight+ CWater
                 +Obesity$Public_Transportation,
                 data =  Obesity , family = binomial)
fit <- predict( log_reg , type = "response")
clas <- rep ( " No " , 2111)
clas[fit > 0.5] = " Yes ";
table(clas, Obesity$Problem)
test.err=(table(clas, Obesity$Problem)[1,2]+
                table(clas, Obesity$Problem)[2,1])/
                sum(table(clas, Obesity$Problem))


#facciamo variare la soglia di classificazione da 0.1 a 0.9, default 0.5

test.err.rate = double(9)
p=double(9)
for(ptry in 1:9){
log_reg <- glm(  Obesity$Problem ~FCCI+ FCV +MFP+ SCC + FAF +Age 
                 +Obesity$family_history_with_overweight+ CWater
                 +Obesity$Public_Transportation,
                 data =  Obesity , family = binomial)
fit <- predict( log_reg , type = "response")
clas <- rep ( " No " , 2111)
p[ptry]=ptry*0.1
clas[fit > p[ptry]] = " Yes ";
table(clas, Obesity$Problem)
test.err.rate[ptry]=(table(clas, Obesity$Problem)[1,2]+
                     table(clas, Obesity$Problem)[2,1])/
                      sum(table(clas, Obesity$Problem))
err.rate
}
plot(p,test.err.rate,type='b',main="Logistic Regression Test Error(p)",
     ylab="test err rate", xlab="p",col='blue', pch=18,
     lwd=3,xaxp = c(0, 0.9, 9))

#il valore ottimale è intorno a p=0.6, proviamo in 20 valori tra 0.55 
# e 0.65

test.err.rate = double(20)
p=double(20)
for(ptry in 1:20){
  log_reg <- glm(  Obesity$Problem ~FCCI+ FCV +MFP+ SCC + FAF +Age 
                   +Obesity$family_history_with_overweight+ CWater
                   +Obesity$Public_Transportation,
                   data =  Obesity , family = binomial)
  fit <- predict( log_reg , type = "response")
  clas <- rep ( " No " , 2111)
  p[ptry]=0.55+ptry*0.005
  clas[fit > p[ptry]] = " Yes ";
  table(clas, Obesity$Problem)
  test.err.rate[ptry]=(table(clas, Obesity$Problem)[1,2]+
                         table(clas, Obesity$Problem)[2,1])/
    sum(table(clas, Obesity$Problem))
  err.rate
}
plot(p,test.err.rate,type='b',main="Logistic Regression Test Error(p)",
     ylab="test err rate", xlab="p",col='blue', pch=18,
     lwd=3, xaxp = c(0.55, 0.65, 10))
min(test.err.rate)
#minimo con p = 0.59
#

log_reg <- glm(  Obesity$Problem ~FCCI+ FCV +MFP+ SCC + FAF +Age 
                 +Obesity$family_history_with_overweight+ CWater
                 +Obesity$Public_Transportation,
                 data =  Obesity , family = binomial)
fit <- predict( log_reg , type = "response")
class <- rep ( " No " , 2111)
class[fit > 0.59] = " Yes ";
table(class, Obesity$Problem)
test.err=(table(class, Obesity$Problem)[1,2]+
            table(class, Obesity$Problem)[2,1])/
  sum(table(clas, Obesity$Problem))
summary(log_reg)
test.err