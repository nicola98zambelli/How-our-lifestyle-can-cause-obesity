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
sum(is.na(Obesity));
Obesity <- na.omit(Obesity_WOUT_TEXT); # remove row with nan;
View(Obesity)
Obesity$Problem <- factor( ifelse(Obesity$NObeyesdad <=3  , " No " , " Yes " ))
hist(Obesity$NObeyesdad)r
# train model
set.seed(123)
#ALBERO SULLE ABITUDINI ALIMENTRI
# Valuate performance with train and test data sets
train <- sample(1:nrow(Obesity),floor(nrow(Obesity)*0.5))

tree_model <- tree(Obesity$Problem ~ FCCI+ NP+ CWater
                    + FCV + CALC + MFP , Obesity, subset = train)
summary(tree_model)
pred_value <- predict(tree_model, newdata = Obesity[-train,],type = "class")
table(pred_value,Obesity$Problem[-train])
test.err.rate=(table(pred_value,Obesity$Problem[-train])[1,2]+
                 table(pred_value,Obesity$Problem[-train])[2,1])/
                 sum(table(pred_value,Obesity$Problem[-train]))
test.err.rate
# Cross validation per potare l'albero
tree_cv <- cv.tree(tree_model, FUN = prune.misclass)
plot(tree_cv)
plot(tree_cv$size, tree_cv$dev)
best = min(tree_cv$size[tree_cv$dev == min(tree_cv$dev)])
k = min(tree_cv$k[tree_cv$dev == min(tree_cv$dev)])

prune <- prune.misclass(tree_model, best = best)
summary(prune)

plot(prune, type = c("uniform"))
text(prune, pretty = 0)
#uguale al precedente
#proviamo con size =5 che ha un misclass appena piu alto ma molto meno complesso
#plot(prune, type = "uniform")
best2 = tree_cv$size[tree_cv$size==5]
prune2 <- prune.misclass(tree_model, best = best2)
plot(prune2, type = c("uniform"))
text(prune2, pretty = 0)
pred_value <- predict(prune2, newdata = Obesity[-train,],type = "class")
table(pred_value,Obesity$Problem[-train])
test.err.rate=(table(pred_value,Obesity$Problem[-train])[1,2]+
                 table(pred_value,Obesity$Problem[-train])[2,1])/
  sum(table(pred_value,Obesity$Problem[-train]))
test.err.rate

#ALBERO SULLE ABITUDINI LEGATE AL FISICO

# Valuate performance with train and test data sets
tree_model <- tree(Problem ~ SCC+ FAF + TUE +Public_Transportation 
                   + Walking+Automobile +
                     Motorbike	+Bike , Obesity, subset = train)
summary(tree_model)
plot(tree_model, type = c("uniform"))
text(tree_model, pretty = 0)
pred_value <- predict(tree_model, newdata = Obesity[-train,],type = "class")
table(pred_value,Obesity$Problem[-train])
test.err.rate=(table(pred_value,Obesity$Problem[-train])[1,2]+
                 table(pred_value,Obesity$Problem[-train])[2,1])/
  sum(table(pred_value,Obesity$Problem[-train]))
test.err.rate
# Cross validation per potare l'albero
tree_cv <- cv.tree(tree_model, FUN = prune.misclass)
plot(tree_cv)
plot(tree_cv$size, tree_cv$dev)
best = min(tree_cv$size[tree_cv$dev == min(tree_cv$dev)])
k = min(tree_cv$k[tree_cv$dev == min(tree_cv$dev)])

prune <- prune.misclass(tree_model, best = best)
summary(prune)

plot(prune, type = c("uniform"))
text(prune, pretty = 0)
pred_value <- predict(prune, newdata = Obesity[-train,],type = "class")
table(pred_value,Obesity$Problem[-train])
test.err.rate=(table(pred_value,Obesity$Problem[-train])[1,2]+
                 table(pred_value,Obesity$Problem[-train])[2,1])/
  sum(table(pred_value,Obesity$Problem[-train]))
test.err.rate
#stesso err rate ma meno complesso

#ALBERO SU tutte le abitudini

# Valuate performance with train and test data sets
tree_model <- tree(Problem ~ FCCI+ NP+ CWater
                   + FCV + CALC +MFP+ SCC + FAF + TUE +Public_Transportation 
                   + Walking+Automobile +
                     Motorbike	+Bike, Obesity, subset = train)
summary(tree_model)
pred_value <- predict(tree_model, newdata = Obesity[-train,],type = "class")
table(pred_value,Obesity$Problem[-train])
test.err.rate=(table(pred_value,Obesity$Problem[-train])[1,2]+
                 table(pred_value,Obesity$Problem[-train])[2,1])/
                 sum(table(pred_value,Obesity$Problem[-train]))
test.err.rate
# Cross validation per potare l'albero
tree_cv <- cv.tree(tree_model, FUN = prune.misclass)
plot(tree_cv)
plot(tree_cv$size, tree_cv$dev)
best = min(tree_cv$size[tree_cv$dev == min(tree_cv$dev)])
k = min(tree_cv$k[tree_cv$dev == min(tree_cv$dev)])

prune <- prune.misclass(tree_model, best = best)
summary(prune)

plot(prune, type = c("uniform"))
text(prune, pretty = 0)
#simile al precedente
#proviamo con size =6 che ha un misclass appena piu alto ma molto meno complesso
#plot(prune, type = "uniform")
best2 = tree_cv$size[tree_cv$size==6]
prune2 <- prune.misclass(tree_model, best = best2)
plot(prune2, type = c("uniform"))
text(prune2, pretty = 0)
pred_value <- predict(prune2, newdata = Obesity[-train,],type = "class")
table(pred_value,Obesity$Problem[-train])
test.err.rate=(table(pred_value,Obesity$Problem[-train])[1,2]+
                 table(pred_value,Obesity$Problem[-train])[2,1])/
                sum(table(pred_value,Obesity$Problem[-train]))
test.err.rate
#BAGGING E RF
library ( randomForest )
bagging <- randomForest(Obesity$Problem ~ FCCI+ NP+ CWater
                   + FCV + CALC +MFP+ SCC + FAF + TUE +Public_Transportation 
                   + Walking+Automobile +
                     Motorbike	+Bike, 
                        data = Obesity, 
                   subset = train,
                   mtry = 14, importance = TRUE,replace = TRUE)
#decrease tree

bagging <- randomForest(Obesity$Problem ~ FCCI+ NP+ CWater
                        + FCV + CALC +MFP+ SCC + FAF + TUE +Public_Transportation 
                        + Walking+Automobile +
                          Motorbike	+Bike, 
                        data = Obesity, 
                        subset = train,
                        mtry = 14, importance = TRUE,replace = TRUE,ntree= 350)
bagging
plot(bagging)
yhat <- predict(bagging, newdata = Obesity[-train,])
table(yhat,Obesity$Problem[-train])
tset.err.rate=(table(yhat,Obesity$Problem[-train])[1,2]+
          table(yhat,Obesity$Problem[-train])[2,1])/
          sum(table(yhat,Obesity$Problem[-train]))
tset.err.rate
importance(bagging)
bagging$err.rate[350]
rf <- randomForest(Obesity$Problem ~ FCCI+ NP+ CWater
                   + FCV + CALC +MFP+ SCC + FAF + TUE +Public_Transportation 
                   + Walking+Automobile +
                     Motorbike	+Bike, 
                   data = Obesity, 
                   subset = train,
                   mtry = floor(sqrt(14)), importance = TRUE,replace = TRUE,ntree= 350)
rf$err.rate[350]
#choose best m
oob.err.rate = double(14)
test.err.rate = double(14)
for(mtry in 1:14){
  fit = randomForest(Obesity$Problem ~ FCCI+ NP+ CWater
                     + FCV + CALC +MFP+ SCC + FAF + TUE +Public_Transportation 
                     + Walking+Automobile +
                       Motorbike	+Bike, 
                     data = Obesity, subset=train, mtry=mtry, ntree = 350)
  oob.err.rate[mtry] = fit$err.rate[350]
  yhat <- predict(fit, newdata = Obesity[-train,])
  test.err.rate[mtry] = (table(yhat,Obesity$Problem[-train])[1,2]+
                         table(yhat,Obesity$Problem[-train])[2,1])/
                          sum(table(yhat,Obesity$Problem[-train]))
}
plot(test.err.rate,type='b',main="Random Forest(m)",
     ylab="train vs test err rate", xlab="m",col='blue', pch=18,
     lwd=3,col.axis='white')
par(new=TRUE)
plot(oob.err.rate,type='b',main="Random Forest(m)",
     ylab="train vs test err rate", xlab="m",col='red', 
     pch=18,lwd=3)
min(test.err.rate)
#con m = 5 risulta migliore
rf <- randomForest(Obesity$Problem ~ FCCI+ NP+ CWater
                   + FCV + CALC +MFP+ SCC + FAF + TUE +Public_Transportation 
                   + Walking+Automobile +
                     Motorbike	+Bike, 
                   data = Obesity, 
                   subset = train,
                   mtry = 5, importance = TRUE,replace = TRUE,ntree= 350)
rf$err.rate[350]
importance(rf)
#Compare
yhat <- predict(rf, newdata = Obesity[-train,])
table(yhat,Obesity$Problem[-train])
plot(rf,pch = "+",col='blue',lwd=2,col.axis='white')
par(new=TRUE)
plot(bagging,pch='o',col='red',lwd=2)
oob.err.rate[14]
test.err.rate[14]
oob.err.rate[5]
test.err.rate[5]
#boosting
library ( gbm )
set.seed(1)
sum(Obesity$Bike)
ntree = 500; 
boost_model <- gbm(Obesity[train,]$NObeyesdad ~ FCCI+ NP+ CWater
                   + FCV + CALC +MFP+ SCC + FAF + TUE +Public_Transportation 
                   + Walking+Automobile +
                     Motorbike	+Bike, 
                   data = Obesity[train,], 
                   distribution = "gaussian" , n.trees = ntree,
                   interaction.depth = 4, shrinkage = 0.01 , verbose = F)
summary(boost_model)
 #install.packages("caret")
library(caret)
train_Obesity = Obesity[train, ]
test_Obesity = Obesity[-train, ]

featurePlot(x = train_Obesity[, c("FAF", "FCCI")], 
            y = train_Obesity$Problem,
            plot = "density", 
            scales = list(x = list(relation = "free"), 
                          y = list(relation = "free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(2, 1), 
            auto.key = list(columns = 2))
#install.packages("ellipse")
library(ellipse)
featurePlot(x = train_Obesity[, c("TUE", "Age")], 
            y = train_Obesity$Problem, 
            plot = "ellipse",
            auto.key = list(columns = 2))
Obesity_eating <- Obesity[ -c(1:5,10,12:14,16:22) ]
boxplot(Obesity_eating, main="BoxPlot eating habits")
Obesity_physic <- Obesity[ -c(1:11,15,21,22) ]
boxplot(Obesity_physic, main="BoxPlot physical conditions")
Obesity_gen<- Obesity[ -c(2,4:22) ]
boxplot(Obesity_gen, main="BoxPlot generics")
boxplot(Obesity$Age,main="BoxPlot Age")
boxplot(Obesity$Weight,main="BoxPlot Weight")
boxplot(Obesity$family_history_with_overweight,main="BoxPlot family history with overweight")
boxplot(Obesity$Weight,main="BoxPlot Weight")