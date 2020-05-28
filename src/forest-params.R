if (! "randomForest" %in% row.names(installed.packages()))  
  install.packages("randomForest")

err <- function(y.true, y.pred) { sum(y.pred!=y.true)/length(y.true) }
predc <- function(...) { predict(..., type="c") }
library(e1071)
library(randomForest)
library(tidyverse)
library(tidyr)
library(caret)

students<- read.csv("../data/students.csv",header=TRUE)
students$X <- NULL

students$Walc = as.factor(students$Walc)
students$Dalc = as.factor(students$Dalc)
sample <- sample.int(n = nrow(students), size = floor(.8*nrow(students)), replace = F)
train <- students[sample, ]
test  <- students[-sample, ]


##dobór najlepszego modelu

##Mona dobrać dzięki temu 3 parametry. Uwaga, dosyć długo się liczy.
##sugerowałabym mtry=9-11, maxnodes >=50), ntrees = 100-300, dla 500 liczy się lepiej, ale wydłuża proces, a charekterystykę widać już przy 300.



seed <- 7
metric <- "Accuracy"
set.seed(seed)

customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree", "maxnodes"), class = rep("numeric", 3), label = c("mtry", "ntree", "maxnodes"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, maxnodes=param$maxnodes, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes



# train model
control <- trainControl(## 10-fold CV
  method = "cv",
  number = 10)
#control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(mtry=c(1:10), ntree=c(10,50,100,300,500), maxnodes=c(3,10,20,50))
set.seed(seed)
custom <- train(Dalc~., data=train, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)


