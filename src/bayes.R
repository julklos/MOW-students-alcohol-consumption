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
#train_D <- subset(train, select = -c(Walc, Dalc))
#test_D <- subset(test, select = -c(Walc, Dalc))
train_labels <- students[sample,]$Dalc
test_labels <- students[-sample,]$Dalc


# potrzebne jest też opakowanie na funkcję predict
# niewymagające przekazywania argumentu type
predc <- function(...) { predict(..., type="c") }

# 10-krotna walidacja krzyżowa na zbiorze trenującym
# przy domyślnych parametrach
cv100 <- tune(naiveBayes, Dalc ~ ., data=train,
                predict.func=predc,
                tunecontrol=tune.control(sampling="cross", cross=10))
# sprawdźmy błąd uzyskany dla walidacji krzyżowej
cv100
# końcowy model zbudowanego na całości przekazanych danych
cv100$best.model
bestModel <- confusionMatrix( predict(cv100$best.model, test, type="c"),test_labels)

# zobaczmy jak błąd na zbiorze testowym ma się do błędu z walidacji krzyżowej
err(test$Dalc, predict(cv100$best.model, test, type="c"))




rm(list=ls())
dev.off()
graphics.off()

