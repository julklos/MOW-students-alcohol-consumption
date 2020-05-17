#biblioteka do bayesa
if (! "e1071" %in% row.names(installed.packages()))
  install.packages("e1071")
if (! "randomForest" %in% row.names(installed.packages()))  
  install.packages("randomForest")
library(e1071)
library(randomForest)

students<- read.csv("../data/students.csv",header=TRUE)
students$X <- NULL

sample <- sample.int(n = nrow(students), size = floor(.8*nrow(students)), replace = F)
train <- students[sample, ]
test  <- students[-sample, ]

