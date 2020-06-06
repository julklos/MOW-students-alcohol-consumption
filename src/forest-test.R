err <- function(y.true, y.pred) { sum(y.pred!=y.true)/length(y.true) }
predc <- function(...) { predict(..., type="c") }
library(e1071)
library(randomForest)
library(tidyverse)
library(tidyr)
library(caret)
library(rfUtilities)

students<- read.csv("students.csv",header=TRUE)
students$X <- NULL

students$Walc = as.factor(students$Walc)
students$Dalc = as.factor(students$Dalc)


## klasy 
cwt = c(0.1,0.2,0.2,0.2,0.3) 
cwt = c(1,5,5,5,5) 
cwt = c(0.5, 2,2,2,2)
cwt = c(0.1, 2,2,2,2)
RF_Walc = randomForest(Walc~., data=train, classwt = cwt , replace = FALSE, mtry=9, ntree= 50, maxnodes = 50)
prediction <-predict(RF_Walc, test)
confusionMatrix(prediction, test$Walc)

RF_Dalc = randomForest(Dalc~., data=train, classwt = cwt , replace = FALSE, mtry=9, ntree= 50, maxnodes = 50)
prediction <-predict(RF_Dalc, test)
confusionMatrix(prediction, test$Dalc)
