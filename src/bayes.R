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
# sprawdźmy błąd uzyskany dla walidacji krzyżowej 0.3364659
cv100_Dalc
# końcowy model zbudowanego na całości przekazanych danych
# Confusion Matrix and Statistics
# 
# Reference
# Prediction  1  2  3  4  5
# 1 79 15  3  2  0
# 2  9  5  3  1  0
# 3  3  2  1  2  2
# 4  0  1  0  0  1
# 5  1  2  0  1  0
# 
# Overall Statistics
# 
# Accuracy : 0.6391          
# 95% CI : (0.5513, 0.7205)
# No Information Rate : 0.6917          
# P-Value [Acc > NIR] : 0.9190          
# 
# Kappa : 0.2057          
# 
# Mcnemar's Test P-Value : 0.3814          
# 
# Statistics by Class:
# 
#                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity            0.8587  0.20000 0.142857  0.00000  0.00000
# Specificity            0.5122  0.87963 0.928571  0.98425  0.96923
# Pos Pred Value         0.7980  0.27778 0.100000  0.00000  0.00000
# Neg Pred Value         0.6176  0.82609 0.951220  0.95420  0.97674
# Precision              0.7980  0.27778 0.100000  0.00000  0.00000
# Recall                 0.8587  0.20000 0.142857  0.00000  0.00000
# F1                     0.8272  0.23256 0.117647      NaN      NaN
# Prevalence             0.6917  0.18797 0.052632  0.04511  0.02256
# Detection Rate         0.5940  0.03759 0.007519  0.00000  0.00000
# Detection Prevalence   0.7444  0.13534 0.075188  0.01504  0.03008
# Balanced Accuracy      0.6854  0.53981 0.535714  0.49213  0.48462
cv100_Dalc$best.model
bestModel_Dalc <- confusionMatrix( predict(cv100_Dalc$best.model, test),test_labels, mode="everything")
bestModel_Dalc

# zobaczmy jak błąd na zbiorze testowym ma się do błędu z walidacji krzyżowej 0.3609023
err(test$Dalc, predict(cv100_Dalc$best.model, test, type="c"))


cv100_Walc <- tune(naiveBayes, Walc ~ ., data = train,
                   predict.func = predc, tunecontrol = tune.control(sampling = "cross", cross = 10))
cv100_Walc$best.model
cv100_Walc #0.5822206
# Reference
# Prediction  1  2  3  4  5
# 1 26 11 10  5  0
# 2 25  3  2  3  0
# 3  4  7  2  9  0
# 4  0  4  4  7  5
# 5  0  0  0  2  4
# 
# Overall Statistics
# 
# Accuracy : 0.3158        
# 95% CI : (0.238, 0.402)
# No Information Rate : 0.4135        
# P-Value [Acc > NIR] : 0.992         
# 
# Kappa : 0.0714        
# 
# Mcnemar's Test P-Value : NA            
# 
# Statistics by Class:
# 
#                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity            0.4727  0.12000  0.11111  0.26923  0.44444
# Specificity            0.6667  0.72222  0.82609  0.87850  0.98387
# Pos Pred Value         0.5000  0.09091  0.09091  0.35000  0.66667
# Neg Pred Value         0.6420  0.78000  0.85586  0.83186  0.96063
# Prevalence             0.4135  0.18797  0.13534  0.19549  0.06767
# Detection Rate         0.1955  0.02256  0.01504  0.05263  0.03008
# Detection Prevalence   0.3910  0.24812  0.16541  0.15038  0.04511
# Balanced Accuracy      0.5697  0.42111  0.46860  0.57387  0.71416
bestModel_Walc <- confusionMatrix(predict(cv100_Walc$best.model, test, type= "c"), test_labels_Wlc)
bestModel_Walc
err(test$Walc, predict(cv100_Walc$best.model, test, type="c")) #0.6842105


rm(list=ls())
dev.off()
graphics.off()

