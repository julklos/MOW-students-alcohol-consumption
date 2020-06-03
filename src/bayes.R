#biblioteka do bayesa
if (! "e1071" %in% row.names(installed.packages()))
  install.packages("e1071")
if (! "randomForest" %in% row.names(installed.packages()))  
  install.packages("randomForest")
if (! "caret" %in% row.names(installed.packages()))
  install.packages("caret")
if (! "ROCR" %in% row.names(installed.packages()))  
  install.packages("ROCR")
library(e1071)
library(randomForest)
library(tidyverse)
library(tidyr)
library(caret)
err <- function(y.true, y.pred) { sum(y.pred!=y.true)/length(y.true) }

students<- read.csv("./data/students.csv",header=TRUE)
students$X <- NULL
students$Walc <- as.factor(students$Walc)
students$Dalc <- as.factor(students$Dalc)

iter = 5

## DAILY
for (i in 1:iter) {
  sample <- sample.int(n = nrow(students), size = floor(.8*nrow(students)), replace = F)
  train <- students[sample, ]
  test  <- students[-sample, ]
  train_D <- select(train, -Dalc)
  test_D <- select(test, -Dalc)
   #train_D <- subset(train, select = -c(Walc, Dalc))
   #test_D <- subset(test, select = -c(Walc, Dalc))
  train_labels <- students[sample,]$Dalc
  test_labels <- students[-sample,]$Dalc
  
  ##uczenie modelu
  model<- naiveBayes(train_D,train_labels)  
  model_laplace <- naiveBayes(train_D, train_labels, laplace=5)
  ##testowanie na zbiorze uczącym
  nam <- paste("D_confussion_matrix_bayes_train_", i, sep = "")
  assign(nam, confusionMatrix(predict(model, train_D), train_labels ))
  ##porównanie z laplacem
  nam <- paste("D_laplace_confussion_matrix_bayes_train_", i, sep = "")
  assign(nam, confusionMatrix(predict(model_laplace, train_D), train_labels ))
  
  ##testowanie na zbiorze trenującym
  nam <- paste("D_confussion_matrix_bayes_test_", i, sep = "")
  assign(nam, confusionMatrix(predict(model, test_D), test_labels ))
  ##porównanie z laplacem
  nam <- paste("D_laplace_confussion_matrix_bayes_test_", i, sep = "")
  assign(nam, confusionMatrix(predict(model_laplace, test_D), test_labels ))
         
}
##uśrednić te statystyki..?

##WEEKENDS
## DAILY
for (i in 1:iter) {
  sample <- sample.int(n = nrow(students), size = floor(.8*nrow(students)), replace = F)
  train <- students[sample, ]
  test  <- students[-sample, ]
  train_W <- select(train, -Walc)
  test_W <- select(test, -Walc)
  #train_W <- subset(train, select = -c(Walc, Dalc))
  #test_W <- subset(test, select = -c(Walc, Dalc))
  train_labels <- students[sample,]$Walc
  test_labels <- students[-sample,]$Walc
  
  ##uczenie modelu
  model<- naiveBayes(train_W,train_labels)  
  
  ##testowanie na zbiorze uczącym
  nam <- paste("W_confussion_matrix_bayes_train_", i, sep = "")
  assign(nam, confusionMatrix(predict(model, train_W), train_labels ))
  
  ##testowanie na zbiorze trenującym
  nam <- paste("W_confussion_matrix_bayes_test_", i, sep = "")
  assign(nam, confusionMatrix(predict(model, test_W), test_labels ))
  
}

##próba strojenia
sample <- sample.int(n = nrow(students), size = floor(.8*nrow(students)), replace = F)
train <- students[sample, ]
test  <- students[-sample, ]
train_D <- select(train, -Dalc)
test_D <- select(test, -Dalc)
train_W <- subset(train, select = -c(Walc, Dalc))
test_W <- subset(test, select = -c(Walc, Dalc))
train_labels <- students[sample,]$Dalc
test_labels <- students[-sample,]$Dalc
test_labels_Wlc <- students[-sample,]$Walc


# potrzebne jest też opakowanie na funkcję predict
# niewymagające przekazywania argumentu type
predc <- function(...) { predict(..., type="c") }

# 10-krotna walidacja krzyżowa na zbiorze trenującym
# przy domyślnych parametrach
          
cv100_Dalc <- tune(naiveBayes, Dalc ~ ., data=train,
                   predict.func=predc, metric = "Accuracy",
                   tunecontrol=tune.control(sampling="cross", cross=10))
# sprawdźmy błąd uzyskany dla walidacji krzyżowej  0.334688
cv100_Dalc

cv100_Dalc$best.model
bestModel_Dalc <- confusionMatrix( predict(cv100_Dalc$best.model, test),test_labels, mode="everything")
bestModel_Dalc
# Reference
# Prediction  1  2  3  4  5
# 1 74 13  2  2  0
# 2 12  7  2  1  1
# 3  3  4  2  2  2
# 4  1  0  0  0  1
# 5  1  1  0  2  0
# 
# Overall Statistics
# 
# Accuracy : 0.6241         
# 95% CI : (0.536, 0.7065)
# No Information Rate : 0.6842         
# P-Value [Acc > NIR] : 0.9419         
# 
# Kappa : 0.2378         
# 
# Mcnemar's Test P-Value : 0.6704         
# 
# Statistics by Class:
# 
#                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity            0.8132  0.28000  0.33333  0.00000  0.00000
# Specificity            0.5952  0.85185  0.91339  0.98413  0.96899
# Pos Pred Value         0.8132  0.30435  0.15385  0.00000  0.00000
# Neg Pred Value         0.5952  0.83636  0.96667  0.94656  0.96899
# Precision              0.8132  0.30435  0.15385  0.00000  0.00000
# Recall                 0.8132  0.28000  0.33333  0.00000  0.00000
# F1                     0.8132  0.29167  0.21053      NaN      NaN
# Prevalence             0.6842  0.18797  0.04511  0.05263  0.03008
# Detection Rate         0.5564  0.05263  0.01504  0.00000  0.00000
# Detection Prevalence   0.6842  0.17293  0.09774  0.01504  0.03008
# Balanced Accuracy      0.7042  0.56593  0.62336  0.49206  0.48450

# zobaczmy jak błąd na zbiorze testowym ma się do błędu z walidacji krzyżowej
err(test$Dalc, predict(cv100_Dalc$best.model, test, type="c")) #0.3759398

## laplace
cv100_Dalc_l <- tune(naiveBayes, Dalc ~ ., data=train,
                   predict.func=predc, metric = "Accuracy", laplace = 100,
                   tunecontrol=tune.control(sampling="cross", cross=10))
# sprawdźmy błąd uzyskany dla walidacji krzyżowej 
cv100_Dalc_l # 0.3136067

cv100_Dalc_l$best.model
bestModel_Dalc_l <- confusionMatrix( predict(cv100_Dalc$best.model, test),test_labels, mode="everything")
bestModel_Dalc_l
# 
# Confusion Matrix and Statistics
# 
# Reference
# Prediction  1  2  3  4  5
# 1 74 13  2  2  0
# 2 12  7  2  1  1
# 3  3  4  2  2  2
# 4  1  0  0  0  1
# 5  1  1  0  2  0
# 
# Overall Statistics
# 
# Accuracy : 0.6241         
# 95% CI : (0.536, 0.7065)
# No Information Rate : 0.6842         
# P-Value [Acc > NIR] : 0.9419         
# 
# Kappa : 0.2378         
# 
# Mcnemar's Test P-Value : 0.6704         
# 
# Statistics by Class:
# 
#                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity            0.8132  0.28000  0.33333  0.00000  0.00000
# Specificity            0.5952  0.85185  0.91339  0.98413  0.96899
# Pos Pred Value         0.8132  0.30435  0.15385  0.00000  0.00000
# Neg Pred Value         0.5952  0.83636  0.96667  0.94656  0.96899
# Precision              0.8132  0.30435  0.15385  0.00000  0.00000
# Recall                 0.8132  0.28000  0.33333  0.00000  0.00000
# F1                     0.8132  0.29167  0.21053      NaN      NaN
# Prevalence             0.6842  0.18797  0.04511  0.05263  0.03008
# Detection Rate         0.5564  0.05263  0.01504  0.00000  0.00000
# Detection Prevalence   0.6842  0.17293  0.09774  0.01504  0.03008
# Balanced Accuracy      0.7042  0.56593  0.62336  0.49206  0.48450
# zobaczmy jak błąd na zbiorze testowym ma się do błędu z walidacji krzyżowej
err(test$Dalc, predict(cv100_Dalc_l$best.model, test, type="c")) #0.3458647

##weekend
cv100_Walc <- tune(naiveBayes, Walc ~ ., data = train,
                   predict.func = predc, tunecontrol = tune.control(sampling = "cross", cross = 10))
cv100_Walc$best.model
cv100_Walc  #0.5654572

bestModel_Walc <- confusionMatrix(predict(cv100_Walc$best.model, test, type= "c"), test_labels_Wlc)
bestModel_Walc
# Confusion Matrix and Statistics
# 
# Reference
# Prediction  1  2  3  4  5
# 1 23 14  5  3  2
# 2 15  5  7  6  0
# 3  7  1  4  4  2
# 4  3  3  8  6  4
# 5  1  1  4  2  3
# 
# Overall Statistics
# 
# Accuracy : 0.3083          
# 95% CI : (0.2311, 0.3942)
# No Information Rate : 0.3684          
# P-Value [Acc > NIR] : 0.9383          
# 
# Kappa : 0.0913          
# 
# Mcnemar's Test P-Value : 0.4522          
# 
# Statistics by Class:
# 
#                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity            0.4694  0.20833  0.14286  0.28571  0.27273
# Specificity            0.7143  0.74312  0.86667  0.83929  0.93443
# Pos Pred Value         0.4894  0.15152  0.22222  0.25000  0.27273
# Neg Pred Value         0.6977  0.81000  0.79130  0.86239  0.93443
# Prevalence             0.3684  0.18045  0.21053  0.15789  0.08271
# Detection Rate         0.1729  0.03759  0.03008  0.04511  0.02256
# Detection Prevalence   0.3534  0.24812  0.13534  0.18045  0.08271
# Balanced Accuracy      0.5918  0.47573  0.50476  0.56250  0.60358
err(test$Walc, predict(cv100_Walc$best.model, test, type="c")) #0.6917293

## weekend laplace
cv100_Walc_l <- tune(naiveBayes, Walc ~ ., data = train,
                   predict.func = predc, tunecontrol = tune.control(sampling = "cross", cross = 10))
cv100_Walc_l$best.model
cv100_Walc_l #0.580479

bestModel_Walc_l <- confusionMatrix(predict(cv100_Walc_l$best.model, test, type= "c"), test_labels_Wlc)
bestModel_Walc_l
# Confusion Matrix and Statistics
# 
# Reference
# Prediction  1  2  3  4  5
# 1 23 14  5  3  2
# 2 15  5  7  6  0
# 3  7  1  4  4  2
# 4  3  3  8  6  4
# 5  1  1  4  2  3
# 
# Overall Statistics
# 
# Accuracy : 0.3083          
# 95% CI : (0.2311, 0.3942)
# No Information Rate : 0.3684          
# P-Value [Acc > NIR] : 0.9383          
# 
# Kappa : 0.0913          
# 
# Mcnemar's Test P-Value : 0.4522          
# 
# Statistics by Class:
# 
#                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity            0.4694  0.20833  0.14286  0.28571  0.27273
# Specificity            0.7143  0.74312  0.86667  0.83929  0.93443
# Pos Pred Value         0.4894  0.15152  0.22222  0.25000  0.27273
# Neg Pred Value         0.6977  0.81000  0.79130  0.86239  0.93443
# Prevalence             0.3684  0.18045  0.21053  0.15789  0.08271
# Detection Rate         0.1729  0.03759  0.03008  0.04511  0.02256
# Detection Prevalence   0.3534  0.24812  0.13534  0.18045  0.08271
# Balanced Accuracy      0.5918  0.47573  0.50476  0.56250  0.60358
err(test$Walc, predict(cv100_Walc_l$best.model, test, type="c")) #0.6917293

rm(list=ls())
dev.off()
graphics.off()

