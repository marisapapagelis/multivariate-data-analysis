fertility=read.csv("/Users/hudasaeed/Downloads/fertility.csv",header=TRUE, stringsAsFactors=T)

#identify outliers
library(tidyverse)
ggplot(fertility)+
  geom_boxplot(aes(x=Age))
ggplot(fertility)+
  geom_boxplot(aes(x=Number.of.hours.spent.sitting.per.day))
max(fertility$Number.of.hours.spent.sitting.per.day)
which(fertility$Number.of.hours.spent.sitting.per.day=="342")
fertility<-fertility[-51,] #has 342 hrs sitting per day- remove error
attach(fertility)

#Training and validation sets
n<-dim(fertility)[1]
set.seed(1)
obs<-sample(1:n,70)
train<-fertility[obs,]
test<-fertility[-obs,]

#Logistic regression - no significant predictors
logit <- glm(Diagnosis~.,data=train, family=binomial("logit")) 
summary(logit)
aic<-step(logit, direction="both", trace=0, k = 2)
summary(aic) 
bic<-step(logit, direction="both", trace=0, k = log(length(Diagnosis)))
summary(bic) 
#GOF test
# #The null hypothesis is H0: The current model fits the data adequately well and 
# the alternative hypothesis is Ha: The current model does not fit the data 
# adequately well. The test statistic is -2log(Li/L0) where Li is the likelihood 
# of the current model and L0 is the likelihood of the saturated model. This 
# statistic under the null follows a chi squared distribution with 69 degrees of 
# freedom. The observed test statistic is 53.713 and the p-value is 0.9121685 
# which is greater than .05 so I fail to reject the null; the current model 
# fits the data adequately well.
1-pchisq(53.713, 69)

#LDA
#Assumption 1: multicollinearity does not apply here since predictor is categorical
#Assumption 2: quantitative variables ~ N
qqnorm(train$Age)
qqline(train$Age)
qqnorm(train$Number.of.hours.spent.sitting.per.day)
qqline(train$Number.of.hours.spent.sitting.per.day)
#But quantitative variables are insignificant/removed so normality is violated
ggplot(train)+
  geom_boxplot(aes(x=Diagnosis, y=Age))
ggplot(train)+
  geom_boxplot(aes(x=Diagnosis, y=Number.of.hours.spent.sitting.per.day))

#CART
library("tree")
CART = tree(Diagnosis~.,data=train)
result = cv.tree(CART,K=100,FUN=prune.tree)
plot(result)
tree.new = prune.tree(CART,best=2)
plot(tree.new)
text(tree.new,label="yprob")
#choose threshold- between .05 and .3 they perform the same
library("lattice")
predict2<-predict(tree.new,test) 
threshold <- seq(from=0.05, to=max(predict2[,1]), by =0.01)
sensitivity<-rep(NA,length(threshold))
specificity<-rep(NA,length(threshold))
misclassification<-rep(NA,length(threshold))
for (i in 1:length(threshold))
{
  Y.hat<-ifelse(predict2[,1]>threshold[i],1,0)
  results<-table(Y.hat,test$Diagnosis)
  specificity[i]<-(results[1,2]/(results[1,2]+results[2,2]))
  sensitivity[i]<-(results[2,1]/(results[1,1]+results[2,1]))
  misclassification[i]<-(results[1,1]+results[2,2])/29
}
xyplot(sensitivity + specificity +misclassification ~ threshold, ylab = "", 
       xlab="Threshold", main="Threshold for CART", type = c("l","p"), auto.key = 
         list(points =T,lines = F),lty=c(1,2))
#misclassification
y<-ifelse(predict2[,1]>.3,"Altered","Normal")
sum(y!=test[,10])/29
#sensitivity
Y.hat<-ifelse(predict2[,1]>.3,1,0)
table(Y.hat,test$Diagnosis)
#ROC
library("pROC")
auc(test$Diagnosis,predict2[,1])

#Bagging
set.seed(1)
library("randomForest")
bag <- randomForest(Diagnosis~.,data=train,ntree=100,mtry=9)
Yhat.bag <- predict(bag,newdata=test,type="class")
#choose threshold between .29 and .43 sens/spec optimized and misclass minimized
library("lattice")
predict3 <- predict(bag,newdata=test,type="prob")
threshold <- seq(from=0.05, to=max(predict3[,1])-.01, by =0.01)
sensitivity<-rep(NA,length(threshold))
specificity<-rep(NA,length(threshold))
misclassification<-rep(NA,length(threshold))
for (i in 1:length(threshold))
{
  Y.hat<-ifelse(predict3[,1]>threshold[i],1,0)
  results<-table(Y.hat,test$Diagnosis)
  specificity[i]<-(results[1,2]/(results[1,2]+results[2,2]))
  sensitivity[i]<-(results[2,1]/(results[1,1]+results[2,1]))
  misclassification[i]<-(results[1,1]+results[2,2])/29
}
xyplot(sensitivity + specificity +misclassification~ threshold, ylab = "", 
       xlab="Threshold", type = c("l","p"), main="Threshold for Bagging Model",auto.key = 
         list(points =T,lines = F),lty=c(1,2))
sensitivity #1-39 max
misclassification #25-39 min 
specificity #25-44 max
threshold[25]
threshold[39]
#misclassification
y1<-ifelse(predict3[,1]>.3,"Altered","Normal")
sum(y1!=test[,10])/29
#AUC
auc(test$Diagnosis,predict3[,1])

#Random forest
set.seed(1)
random <- randomForest(Diagnosis~., data = train, ntree=100,mtry = 3)
Yhat.rf <- predict(random, newdata = test, type = "class")
#choose threshold between .3 and .41 sens/spec optimized and misclass minimized
library("lattice")
predict4<-predict(random, newdata = test, type = "prob")
threshold <- seq(from=0.05, to=max(predict4[,1])-.01, by =0.01)
sensitivity<-rep(NA,length(threshold))
specificity<-rep(NA,length(threshold))
misclassification<-rep(NA,length(threshold))
for (i in 1:length(threshold))
{
  Y.hat<-ifelse(predict4[,1]>threshold[i],1,0)
  results<-table(Y.hat,test$Diagnosis)
  specificity[i]<-(results[1,2]/(results[1,2]+results[2,2]))
  misclassification[i]<-(results[1,1]+results[2,2])/29
  sensitivity[i]<-(results[2,1]/(results[1,1]+results[2,1]))
}
xyplot(sensitivity + specificity +misclassification ~ threshold, ylab = "", 
       xlab="Threshold", type = c("l","p"), main="Threshold for Random Forest Model",auto.key = 
         list(points =T,lines = F),lty=c(1,2))
sensitivity #1-37 max
misclassification #26-37 min
specificity #26-45 max
threshold[26] 
threshold[37] 
# misclassification rate 
y2<-ifelse(predict4[,1]>.3,"Altered","Normal")
sum(y2!=test[,10])/29
# AUC 
auc(test$Diagnosis, predict4[,1])

#Boosting
#Need Diagnosis in binary
bern <- fertility
relevel(bern$Diagnosis, ref = "Normal")
bern$Diagnosis<-rep(NA, length(Diagnosis))
bern$Diagnosis[fertility$Diagnosis == "Altered"] <- 1 
bern$Diagnosis[fertility$Diagnosis == "Normal"] <- 0
bern.train<-bern[obs,]
bern.validate<-bern[-obs,]
library(gbm)
set.seed(5)
boost <- gbm(Diagnosis~., distribution = "bernoulli", data = bern.train)
#choose threshold between .38 and .5 sens/spec optimized and misclass minimized
library("lattice")
predict5<-predict(boost, newdata = bern.validate, type = "response")
threshold <- seq(from=0.05, to=max(predict5), by =0.01)
sensitivity<-rep(NA,length(threshold))
specificity<-rep(NA,length(threshold))
misclassification<-rep(NA,length(threshold))
for (i in 1:length(threshold))
{
  Y.hat<-ifelse(predict5>threshold[i],1,0)
  results<-table(Y.hat,bern.validate$Diagnosis)
  specificity[i]<-(results[1,1]/(results[1,2]+results[1,1]))
  misclassification[i]<-(results[1,2]+results[2,1])/29
  sensitivity[i]<-(results[2,2]/(results[2,1]+results[2,2]))
}
xyplot(sensitivity + specificity +misclassification ~ threshold, ylab = "", 
       xlab="Threshold", type = c("l","p"), main="Threshold for Boosting Model",auto.key = 
         list(points =T,lines = F),lty=c(1,2))
sensitivity #34-46 max
misclassification #34-46 min 
threshold[34] 
threshold[46] 
# misclassification rate 
y3<-ifelse(predict5>.5,"1","0")
sum(y3!=bern.validate[,10])/29
# AUC 
auc(bern.validate$Diagnosis, predict5)

#variable importance/relationships for advanced tree-based methods
varImpPlot(random, main="Random Forest Model Variable Importance Plot")
varImpPlot(bag, main="Bagging Model Variable Importance Plot")
boost$var.monotone
