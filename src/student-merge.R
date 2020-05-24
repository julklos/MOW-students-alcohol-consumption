library(tidyverse)
library(tidyr)
library(ggplot2)
library(Hmisc)

mat_students <- read.csv("./data/student-mat.csv",header=TRUE)
por_students <- read.csv("./data/student-por.csv",header=TRUE)
students = rbind(mat_students, por_students)

cls = c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")
students <- students[!duplicated(students[,cls]),]

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

hgD = hist(students$Dalc,plot = FALSE)
hgW =  hist(students$Walc, plot = FALSE)

hgD$density = hgD$counts/sum(hgD$counts)*100
hgW$density = hgW$counts/sum(hgW$counts)*100

plot(hgD, freq=FALSE, col = c2, main="Spożycie alkoholu", ylab = "Studenci [%]", xlab= "od 1 - bardzo rzadko to 5 - bardzo często") # Plot 1st histogram using a transparent color
plot(hgW,   freq=FALSE, col = c1, add = TRUE) # Add 2nd histogram using different color
legend("topright", 
       c("weekend", "dzień pracujący"), 
       lty=c(10, 2, 10), 
       col=c(c1,c2), 
       bty = "n")

write.csv(students,'students.csv')
# rm(list=ls())
#dev.off()
#graphics.off()



# prepocessing the data
students[students$Walc == 1,]$Walc = "bardzo_malo"
students[students$Walc == 2,]$Walc = "malo"
students[students$Walc == 3,]$Walc = "umiarkowanie"
students[students$Walc == 4,]$Walc = "duzo"
students[students$Walc == 5,]$Walc = "bardzo_duzo"
students$Walc = as.factor(students$Walc)

students[students$Dalc == 1,]$Dalc = "bardzo_malo"
students[students$Dalc == 2,]$Dalc = "malo"
students[students$Dalc == 3,]$Dalc = "umiarkowanie"
students[students$Dalc == 4,]$Dalc = "duzo"
students[students$Dalc == 5,]$Dalc = "bardzo_duzo"
students$Dalc = as.factor(students$Dalc)

# 80% train 20% test
sample <- sample.int(n = nrow(students), size = floor(.8*nrow(students)), replace = F)
train <- students[sample, ]
test  <- students[-sample, ]


# list -> dataframes
typeof(train)
typeof(test)

glimpse(train)
glimpse(test)
names(train)
# analysis

# basic approach

set.seed(1234)
# training

#ntree = 10 OOB = 58.93%
#ntree = 100 OOB = 55.77%
#ntree = 1000 OOB = 51.23%
#ntree = 10000 OOB = 51.04%


# classwt 
names(train)
myData = select(train, -Dalc)

iters = c( 10, 20, 30,40,100, 1000, 10000)
for(i in iters){
  nam <- paste("model_replaceTRUE_maxnodes60_", i, sep = "")
  assign(nam, randomForest(Walc ~ . , data = myData, ntree = i, replace = TRUE, maxnodes = 60 ) )
  
  name <- paste("model_replaceTRUE_maxnodes60_", i, sep = "")
  assign(name, importance(randomForest(Walc ~ . , data = myData, ntree = i, replace = TRUE, maxnodes = 60 )) )
  
  na <- paste("model_replaceTRUE_maxnodes60_", i, sep = "")
  assign(na, varImpPlot(randomForest(Walc ~ . , data = myData, ntree = i, replace = TRUE, maxnodes = 60 )) )
  
  
}


for(i in iters){
  nam <- paste("model_replaceTRUE_maxnodes6_", i, sep = "")
  assign(nam, randomForest(Walc ~ . , data = myData, ntree = i, replace = TRUE, maxnodes = 60 ) )
  
  name <- paste("model_replaceTRUE_maxnodes6_", i, sep = "")
  assign(name, importance(randomForest(Walc ~ . , data = myData, ntree = i, replace = TRUE, maxnodes = 60 )) )
  
  na <- paste("model_replaceTRUE_maxnodes6_", i, sep = "")
  assign(na, varImpPlot(randomForest(Walc ~ . , data = myData, ntree = i, replace = TRUE, maxnodes = 60 )) )
  
  
}
nam <- paste("A", i, sep = "")
assign(nam, rnorm(3)+d)

#https://stackoverflow.com/questions/46816174/confusion-matrix-for-random-forest-in-r-caret

model_1 = randomForest(Walc ~ . , data = train, ntree = 1 ) 
importance(model_1)
varImpPlot(nam) 
  name <- paste("importance_model", i, sep = "")
  assign(name,  )
  
  na <- paste("varImpPlot_model", i, sep = "")
  assign(na, varImpPlot(nam) )
  
  
}


