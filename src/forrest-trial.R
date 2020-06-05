if (! "randomForest" %in% row.names(installed.packages()))  
  install.packages("randomForest")

err <- function(y.true, y.pred) { sum(y.pred!=y.true)/length(y.true) }
predc <- function(...) { predict(..., type="c") }
library(e1071)
library(randomForest)
library(tidyverse)
library(tidyr)
library(caret)


students<- read.csv("students.csv",header=TRUE)
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
customRF$parameters <- data.frame(parameter = c("mtry", "ntree", "maxnodes", "replace"), class = c(rep("numeric", 3), "logical"), label = c("mtry", "ntree", "maxnodes", "replace"))
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
tunegrid <- expand.grid(mtry=c(1:10), ntree=c(10,50,100,300,500), maxnodes=c(3,10,20,50), replace=c(TRUE, FALSE))
set.seed(seed)
custom_D <- train(Dalc~., data=train, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
summary(custom_D)
custom_D$bestTune


    #mtry ntree maxnodes replace
#248    7    10       50    TRUE
#png("custom_D.png")

# Call:
#   randomForest(x = x, y = y, ntree = param$ntree, mtry = param$mtry,      maxnodes = param$maxnodes) 
# Type of random forest: classification
# Number of trees: 10
# No. of variables tried at each split: 7
# 
# OOB estimate of  error rate: 29.62%
# Confusion matrix:
#   1  2 3 4 5 class.error
# 1 343 14 5 0 2  0.05769231
# 2  68 19 6 0 0  0.79569892
# 3  22 10 3 0 2  0.91891892
# 4   5  3 2 0 2  1.00000000
# 5  10  0 3 0 1  0.92857143

plot(custom_D)


custom_W <- train(Walc~., data=train, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
summary(custom_W)
custom_W$bestTune
custom_W$finalModel

plot(custom_W)
#     mtry ntree maxnodes replace
# 335    9    50       50   FALSE   

# Call:
#   randomForest(x = x, y = y, ntree = param$ntree, mtry = param$mtry,      maxnodes = param$maxnodes) 
# Type of random forest: classification
# Number of trees: 50
# No. of variables tried at each split: 9
# 
# OOB estimate of  error rate: 55.77%
# Confusion matrix:
#   1  2  3  4 5 class.error
# 1 179  7  7  0 1  0.07731959
# 2  88 13 15  7 0  0.89430894
# 3  50 18 12 22 1  0.88349515
# 4  25  9 21 22 1  0.71794872
# 5   7  3  5  8 8  0.74193548

cwt = c(0.1,0.2,0.2,0.2,0.3) 
cwt = c(1,5,5,5,5) 
# Confusion Matrix and Statistics
# 
# Reference
# Prediction  1  2  3  4  5
# 1  0  0  0  0  0
# 2 51 21 11  2  2
# 3  5  2  4  0  1
# 4  5  0  5  9  6
# 5  0  1  1  1  6
# 
# Overall Statistics
# 
# Accuracy : 0.3008      

cwt = c(0.5, 2,2,2,2)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction  1  2  3  4  5
# 1  0  0  0  1  0
# 2 50 22 10  1  1
# 3  5  1  5  0  1
# 4  6  0  6  9  7
# 5  0  1  0  1  6
# 
# Overall Statistics
# 
# Accuracy : 0.3158        
# 95% CI : (0.238, 0.402)
# No Information Rate : 0.4586        
# P-Value [Acc > NIR] : 0.9997        
# 
# Kappa : 0.1879  
cwt = c(0.1, 2,2,2,2)
RF_Walc = randomForest(Walc~., data=train, classwt = cwt , replace = FALSE, mtry=9, ntree= 50, maxnodes = 50)
prediction <-predict(RF_Walc, test)
confusionMatrix(prediction, test$Walc)






