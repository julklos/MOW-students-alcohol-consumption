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


sample <- sample.int(n = nrow(students), size = floor(.8*nrow(students)), replace = F)
train <- students[sample, ]
test  <- students[-sample, ]

mtryDf<- data.frame(mtry=integer(),
                 err=double(),
                 errTest=double())
maxNodesDf<- data.frame(maxNodes=integer(),
                    err=double(),
                    errTest=double())
numTreesDf <- data.frame(numtrees=integer(),
                               err=double(),
                         errTest=double())

mtryAr = c(1:30)
numTreesAr = c(1, 10, 30, 50, 100, 300, 500, 1000, 3000)
maxNodseAr = c(1, 5, 10, 25, 30, 60, 100)
iters = 3
constmMtry = 9
constMaxNodes = 60
constNtrees = 300
for (att in mtryAr){
  ## opcja jeden, ewentualnie CV/ OBB- to bardziej niżej do rysunku prowadzi
  for(i in 1:iters){
    sample <- sample.int(n = nrow(students), size = floor(.8*nrow(students)), replace = F)
    train <- students[sample, ]
    test  <- students[-sample, ]

    model <- randomForest(Walc ~ . , data = train, mtry = att, ntree =constNtrees, replace = TRUE, maxnodes = constMaxNodes ) 
    
    newValues <- data.frame(att, err(train$Walc, predict(model, train)), err(test$Walc, predict(model, test)) )      
    names(newValues) <- c("mtry", "err", "errTest")  
    mtryDf <- rbind(mtryDf, newValues)  
    
  }
}
boxplot(err~mtry, data=mtryDf,xlab="mytry", ylab="błąd")
boxplot(errTest~mtry, data=mtryDf,xlab="mytry", ylab="błąd")

