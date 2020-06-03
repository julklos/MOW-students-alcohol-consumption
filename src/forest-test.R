err <- function(y.true, y.pred) { sum(y.pred!=y.true)/length(y.true) }
predc <- function(...) { predict(..., type="c") }
library(e1071)
library(randomForest)
library(tidyverse)
library(tidyr)
library(caret)
library(rfUtilities)

students<- read.csv("../data/students.csv",header=TRUE)
students$X <- NULL

students$Walc = as.factor(students$Walc)
students$Dalc = as.factor(students$Dalc)


## testuje 20 razy model dla 2 stalych paramtrow, 1 zmienny

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


control <- trainControl(## 10-fold CV
  method = "cv",
  number = 10)


############################################# DALC

#mtry ntree maxnodes replace
#248    7    10       50    TRUE
tunegrid <- expand.grid(mtry=c(7), ntree=c(100), maxnodes=c(3,10,20,50, 100, 150))
#tunegrid <- expand.grid(mtry=c(7), ntree=c(1,10,30,100,300,500), maxnodes=c(10))
#tunegrid <- expand.grid(mtry=c(1:9), ntree=c(200), maxnodes=c(40))

iter = 30 #liczba uruchomien algorytmu
LIST=list()
for(i in 1:iter){
  
  sample <- sample.int(n = nrow(students), size = floor(.8*nrow(students)), replace = F)
  train <- students[sample, ]
  test  <- students[-sample, ]
  custom_D <- train(Dalc~., data=train, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
  
  LIST[[i]] <- list(select(custom_D$results, c('mtry','ntree','maxnodes','Accuracy') ) ) 
  
  
}
# prepocess data
temp = lapply(LIST, as.data.frame)
output = bind_rows(temp)


boxplot( Accuracy~maxnodes, 
         data=output,
         main="Wykres pudełkowy z 2 stałymi parametrami 
         liczba losowanych zmiennych 7, liczba drzew 100",
         horizontal = FALSE
         )


############################################# WALC
#     mtry ntree maxnodes replace
# 335    9    50       50   FALSE   

#tunegrid <- expand.grid(mtry=c(9), ntree=c(100), maxnodes=c(3,10,20,50, 100, 150))
#tunegrid <- expand.grid(mtry=c(9), ntree=c(1,10,30,100,300,500), maxnodes=c(50))
tunegrid <- expand.grid(mtry=c(1:9), ntree=c(100), maxnodes=c(50))
iter = 30 #liczba uruchomien algorytmu
LIST=list()
for(i in 1:iter){
  
  sample <- sample.int(n = nrow(students), size = floor(.8*nrow(students)), replace = F)
  train <- students[sample, ]
  test  <- students[-sample, ]
  custom_D <- train(Walc~., data=train, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
  
  LIST[[i]] <- list(select(custom_D$results, c('mtry','ntree','maxnodes','Accuracy') ) ) 
  
  
}
# prepocess data
temp = lapply(LIST, as.data.frame)
output = bind_rows(temp)


boxplot( Accuracy~mtry, 
         data=output,
         main="Wykres pudełkowy dla zmiennej Walc z 2 stałymi parametrami 
         liczba drzew 100, max. liczba podziałóW 50",
         horizontal = FALSE
)

