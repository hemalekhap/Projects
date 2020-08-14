# import library
library(tidyverse)
library(leaps)
library(lattice)
library(car)
library(tree)
library(randomForest)
library(gbm)
library(ggplot2)

# read the csv
data <- read.csv(file="~/Downloads/Facebook_metrics/dataset_Facebook1.csv")
# check na row
sum(is.na(data))
# clean na row
data=na.omit(data)
sum(is.na(data))
# get the data we needed
currentdata = data[c(1:7)]
# creat Ratio column of "Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post and Lifetime.Post.reach.by.people.who.like.your.Page"
currentdata$Ratio = data$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post / data$Lifetime.Post.reach.by.people.who.like.your.Page  *100 
# check the overall stucture
summary(currentdata)
splom(currentdata, as.matrix = TRUE,
      cex = .5,varname.cex = 0.56,
      varname.col = "red", pscale = 0)
# Assign factor variables
currentdata$Type = factor(currentdata$Type)
currentdata$Category = factor(currentdata$Category)
currentdata$Paid = factor(currentdata$Paid)

# explore the boxplot of Ratio
boxplot(currentdata$Ratio)
# clean the outlier
outliers<-boxplot(currentdata$Ratio, plot=FALSE)$out
currentdata <- currentdata[-which(currentdata$Ratio %in% outliers),]
#############  Finding best subsets ############# 
# full data
regfit.full = regsubsets(Ratio~.,currentdata)
reg.summary = summary(regfit.full)
reg.summary
round( 100*reg.summary$rsq, 1)
model.mse = reg.summary$rss/nrow(currentdata)
model.mse
# Plots showing model selection criteria
par(mfrow = c(2,2))
xlab = "Number of Variable"
# 1st row 1st  column
plot(reg.summary$rss,xlab = xlab,
     ylab = "RSS",type = "l")
# 1st row 2nd  column
plot(reg.summary$adjr2,xlab = xlab,
     ylab = "Adjusted RSq",type = "l")
loc <- which.max(reg.summary$adjr2)
loc
points(loc,reg.summary$adjr2[loc],
       col = "red",cex = 2,pch = 20)
# 2nd row 1st column
plot(reg.summary$cp,xlab = xlab,
     ylab = "Cp",type = 'l')
loc <- which.min(reg.summary$cp)
loc
points(loc,reg.summary$cp[loc],
       col = "red",cex = 2,pch = 20)
# 2nd row 2nd column
plot(reg.summary$bic,xlab = xlab,
     ylab = "BIC",type = 'l')
loc <-  which.min(reg.summary$bic)
loc
points(loc,reg.summary$bic[loc],
       col = "red",cex = 2,pch = 20)
# the best 3 data the MSE 25.54 TypePhoto TypeStatus Category2
regfit.3 = regsubsets(Ratio~.,currentdata,nvmax = 3)
reg.summary.3 = summary(regfit.3)
reg.summary.3
# the best 4 variable TypePhoto TypeStatus Category2 Category3 
regfit.4 = regsubsets(Ratio~.,currentdata,nvmax = 4)
reg.summary.4 = summary(regfit.4)
reg.summary.4 
dev.off()

#Linear regression model 
# Assign month as factor variable
currentdata$Post.Month = factor(currentdata$Post.Month)
# Divide the data in training and test sets
set.seed(123)
train = sample(1:nrow(currentdata),0.8*nrow(currentdata))
Test = currentdata$Ratio[-train]
length(train)
#384
length(Test)
#97

lm.reg <- lm(Ratio~.,data=currentdata, subset =train)
# summary
summary(lm.reg)
# confidence intervals
confint(lm.reg)
# check the diagnostic plots
par(mfrow=c(2,2))
plot(lm.reg)
# Statistical tests for regression diagnostics
# Residual autocorrelation test
# durbinWatsonTest(lm.reg)
# Non-constant variance test
ncvTest(lm.reg)
# Normal distribution hypothesis test
shapiro.test(rstandard(lm.reg))
# check outlier
outlierTest(lm.reg)
# Transforming Ratio to sqrt(Ratio)
currentdata$sqrtRatio = sqrt(currentdata$Ratio)
# check outlier
boxplot(currentdata$Ratio)
# no outlier
# fit the new model
lm.reg <- lm(sqrtRatio~.-Ratio,data=currentdata, subset =train)
# summary
summary(lm.reg)
# confidence intervals
confint(lm.reg)
# check the diagnostic plots
par(mfrow=c(2,2))
plot(lm.reg)
# Statistical tests for regression diagnostics
# Residual autocorrelation test
# durbinWatsonTest(lm.reg)
# Non-constant variance test
ncvTest(lm.reg)
# Normal distribution hypothesis test
shapiro.test(rstandard(lm.reg))
# check outlier
outlierTest(lm.reg)
# predict test set Ratio
lm.probs=predict(lm.reg,currentdata[-train,],type="response")
Ratio.test=currentdata[-train,"sqrtRatio"]
#Plot predicted vs test values of Ratio
plot(lm.probs*lm.probs,Ratio.test*Ratio.test)
abline(0,1)
#test set MSE
mean((lm.probs^2-Ratio.test^2)^2)

#Regression Trees
# Delete the column sqrtRatio
currentdata = currentdata[,-9]
# Fit the tree to the training data.
tree.Ratio=tree(Ratio~.,currentdata,subset=train)
summary(tree.Ratio)
plot(tree.Ratio)
text(tree.Ratio,pretty=0,cex=0.9)
tree.Ratio
# make predictions on the test set.
yhat=predict(tree.Ratio,newdata=currentdata[-train,])
Ratio.test=currentdata[-train,"Ratio"]
plot(yhat,Ratio.test)
abline(0,1)
mean((yhat-Ratio.test)^2)

# The function cv.tree() performs cross-validation in order to cv.tree() determine 
# the optimal pruning. The output has several elements
cv.Ratio=cv.tree(tree.Ratio)
names(cv.Ratio)
cv.Ratio$size
cv.Ratio$dev
cv.Ratio$k
cv.Ratio$method
par(mfrow=c(1,2))
plot(cv.Ratio$size,cv.Ratio$dev,type="b")
plot(cv.Ratio$k,cv.Ratio$dev,type="b")
dev.off()

# Now we can prune the tree using prune.tree() function to find the best 3 variables combination
prune.Ratio=prune.tree(tree.Ratio,best=6) #size
plot(prune.Ratio)
text(prune.Ratio,pretty=0)
summary(prune.Ratio)
# In keeping with the cross-validation results, we use the pruned tree to
# make predictions on the test set.
yhat=predict(prune.Ratio,newdata=currentdata[-train,])
Ratio.test=currentdata[-train,"Ratio"]
plot(yhat,Ratio.test)
abline(0,1)
mean((yhat-Ratio.test)^2)

#Comaprison of Number of views to be expected in case of paid posts and unpaid posts

#Paid-Posts
paid.data<-read.csv(file="~/Downloads/Facebook_metrics/Paid.csv",header=TRUE)
paid.views<-c(0)
for(i in 1:180)
   paid.views[i]=0
for(i in 1:139)
   paid.views[floor(paid.data$Lifetime.Post.Total.Reach[i]/1000)]=1+paid.views[floor(paid.data$Lifetime.Post.Total.Reach[i]/1000)]
df= as.data.frame(cbind(Overall.Cond= 1:180, paid.views))
df.freq= as.vector(rep(df$Overall.Cond, df$paid.views))
hist(df.freq,breaks = 300,plot = TRUE,col="green",xlim = c(0,50),main="non paid post views frequency",xlab="post viewers in 1000 views")
axis(side=2, at=c(0,5,10,15,20,25,30))
axis(side=1, at=c(0:10)*10)

#Unpaid-Posts
Unpaid.data<-read.csv(file="~/Downloads/Facebook_metrics/Unpaid.csv",header=TRUE)
Unpaid.views<-c(0)
for(i in 1:180)
   Unpaid.views[i]=0
for(i in 1:139)
Unpaid.views[floor(Unpaid.data$Lifetime.Post.Total.Reach[i]/1000)]=1+Unpaid.views[floor(Unpaid.data$Lifetime.Post.Total.Reach[i]/1000)]
df= as.data.frame(cbind(Overall.Cond= 1:180, Unpaid.views))
df.freq= as.vector(rep(df$Overall.Cond, df$Unpaid.views))
hist(df.freq,breaks = 100,plot = TRUE,col="green",xlim = c(0,50),main=
        "non paid post views frequency",xlab="post viewers in 1000 views")
axis(side=2, at=c(0,5,10,15,20,25,30))
axis(side=1, at=c(0:10)*10)

# find the citation of R.package
citation(package = "tidyverse")
citation(package = "leaps")
citation(package = "lattice")
citation(package = "car")
citation(package = "tree")
citation(package = "randomForest")
citation(package = "gbm")
citation(package = "ggplot2")
