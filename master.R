#-------Assignment 1---------------#
#--------Karan Kant (kk4ze)-------#

library(tidyverse)
library(dplyr)

setwd("C:/Users/karan/OneDrive/Documents/UVA/Fall Semester/SYS 6018/Titanic/titanic/data")
train=read_csv('train.csv')
test=read_csv('test.csv')
setwd("C:/Users/karan/OneDrive/Documents/UVA/Fall Semester/SYS 6018/Titanic/titanic")

train$Ticket <- factor(train$Ticket)
train$Pclass <-factor(train$Pclass)
train$Name<-factor(train$Name)
train$Embarked<-factor(train$Embarked)
test$Ticket <- factor(test$Ticket)
test$Pclass <-factor(test$Pclass)
test$Name<-factor(test$Name)
test$Embarked<-factor(test$Embarked)

train$Family<-train$SibSp+train$Parch
test$Family<-test$SibSp+test$Parch
train$Age[is.na(train$Age)]<-mean(train$Age,na.rm=TRUE)
train$Cabin[!is.na(train$Cabin)]<-1
train$Cabin[is.na(train$Cabin)]<-0
test$Cabin[!is.na(test$Cabin)]<-1
test$Cabin[is.na(test$Cabin)]<-0
train$Embarked[is.na(train$Embarked)]<-'S'
test$Embarked[is.na(test$Embarked)]<-'S'
test$Age[is.na(test$Age)]<-mean(test$Age,na.rm=TRUE)
train$Cabin <-factor(train$Cabin)
test$Cabin <-factor(test$Cabin)

random<-sample(1:891,size=600)

train.train<-train[random,]
train.valid<-train[-random,]

train.lg <- glm(Survived~.-Name -Ticket -SibSp -Fare -Embarked -Parch -PassengerId, data=train.train, family = "binomial")
train.lg$xlevels[["Name"]] <- union(train.lg$xlevels[["Name"]], levels(train.valid$Name))
train.lg$xlevels[["Ticket"]] <- union(train.lg$xlevels[["Ticket"]], levels(train.valid$Ticket))
train.lg$xlevels[["Name"]] <- union(train.lg$xlevels[["Name"]], levels(test$Name))
train.lg$xlevels[["Ticket"]] <- union(train.lg$xlevels[["Ticket"]], levels(test$Ticket))
summary(train.lg)

probs<-as.vector(predict(train.lg,newdata=train.valid, type="response")) #created a vector
preds <- rep(0,291)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
table(preds,train.valid$Survived)

probs<-as.vector(predict(train.lg,newdata=test, type="response")) #added the second model to my prediction dataset
preds<-rep(0,418)
preds[probs>0.5]<-1
test$Survived<-preds

test2<-test[-c(2:12)]

write.table(test2, file = "Results.csv", row.names=F, col.names=c("PassengerId","Survived"), sep=",") #output to CSV
