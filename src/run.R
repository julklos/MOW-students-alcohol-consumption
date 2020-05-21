#biblioteka do bayesa
if (! "e1071" %in% row.names(installed.packages()))
  install.packages("e1071")
if (! "randomForest" %in% row.names(installed.packages()))  
  install.packages("randomForest")
library(e1071)
library(randomForest)
library(dplyr)

setwd("C:/Users/Asia/Documents/MOW_2/MOW-students-alcohol-consumption/data")
students<- read.csv("students.csv",header=TRUE)
students$X <- NULL
