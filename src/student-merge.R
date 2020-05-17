library(tidyr)
library(ggplot2)
library(Hmisc)

mat_students <- read.csv("../data/student-mat.csv",header=TRUE)
por_students <- read.csv("../data/student-por.csv",header=TRUE)
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
rm(list=ls())
dev.off()
graphics.off()

