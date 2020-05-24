if (! "randomForest" %in% row.names(installed.packages()))  
  install.packages("randomForest")

err <- function(y.true, y.pred) { sum(y.pred!=y.true)/length(y.true) }
predc <- function(...) { predict(..., type="c") }
library(e1071)
library(randomForest)
library(tidyverse)
library(tidyr)
library(caret)

students<- read.csv("./data/students.csv",header=TRUE)
students$X <- NULL

students$Walc = as.factor(students$Walc)
students$Dalc = as.factor(students$Dalc)
sample <- sample.int(n = nrow(students), size = floor(.8*nrow(students)), replace = F)
train <- students[sample, ]
test  <- students[-sample, ]
sim = 20
iters = c(1,5, 10, 20, 50,100, 200, 500, 1000, 10000)
for(i in iters){
  
}
