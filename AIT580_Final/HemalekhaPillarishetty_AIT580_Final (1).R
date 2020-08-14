library(tidyverse)
library(cluster)   
library(mclust)
Schooldata<- read.csv("~/Downloads/Immuniztion.csv")
na.omit(Schooldata)
summary(Schooldata)
#scatterplot
plot(Schooldata$K_12_enrollment, Schooldata$Number_exempt_for_HepatitisB,main = "Students vs HepB excemption", xlab = "number of students",ylab = "exveptions with hepb",pch = 19 , frame = FALSE)
abline(lm(Schooldata$Number_exempt_for_HepatitisB~Schooldata$K_12_enrollment, data = Schooldata), col= "lightblue")
install.packages("car")
library("car")
scatterplot(Schooldata$K_12_enrollment ~ Schooldata$Number_exempt_for_HepatitisB, data = Schooldata) 
#hypothesis testing
Enrolled<- which(Schooldata$Reported=='Y')
NEnrolled<- which(Schooldata$Reported=='N')
t.test(Schooldata$School_Name[Enrolled],Schooldata$School_Name[NEnrolled], alternative = "less")

#regression
scatter.smooth(x=Schooldata$Percent_complete_for_all_immunizations,y=Schooldata$K_12_enrollment,main="enrollment~completedimmunization")
#corelation
cor(Schooldata$Percent_with_medical_exemption,Schooldata$Percent_with_personal_exemption)
#clustering
omitteddata <-na.omit(Schooldata)
summary(omitteddata)
fit <- Mclust(omitteddata)
plot(fit)


