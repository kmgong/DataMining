#############################
# Kevin Gong
# STAT W4240 
# Homework 06, Problem 3
# May 05, 2014
#

#############################

#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine
setwd("~/Dropbox/SIPA/Data Mining/hw06")


# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.
# Use the package installer and be sure to install all dependencies
library(ISLR)
library(e1071)


#################
# Problem 3a
#################

data("OJ")
OJ


set.seed(1)

rownumbers = sample(nrow(OJ))
train_set = OJ[rownumbers[1:800],]
test_set = OJ[-rownumbers[1:800],]

dim(train_set)
dim(test_set)


#################
# Problem 3b
#################

Purchase = train_set[,1]

training = data.frame(x=train_set[,-1], y=Purchase)
svm_train = svm(y~.,data=training, kernel="linear", cost=0.01, scale=TRUE)
summary(svm_train)

??svm

#################
# Problem 3c
#################

test = data.frame(x=test_set[,-1],y=test_set[,1])
svm_test = svm(y~.,data=test, kernel="linear", cost=0.01, scale=TRUE)


train_predict = predict(svm_train,training)
test_predict = predict(svm_test,test)


train_error = sum(train_predict!=train_set[,1])/length(train_predict)
test_error = sum(test_predict!=test_set[,1])/length(test_predict)


train_error
test_error


#################
# Problem 3d
#################


#################
# Problem 3e
#################





#################
# Problem 3f
#################


svm_train_radial = svm(y~.,data=training, kernel="radial", cost=0.01, scale=TRUE)
svm_test_radial = svm(y~.,data=test, kernel="radial", cost=0.01, scale=TRUE)

train_predict_radial = predict(svm_train_radial,training)
test_predict_radial = predict(svm_test_radial,test)

train_error_radial = sum(train_predict_radial!=train_set[,1])/length(train_predict_radial)
test_error_radial = sum(test_predict_radial!=test_set[,1])/length(test_predict_radial)

summary(svm_train_radial)
train_error_radial
test_error_radial


#################
# Problem 3g
#################

svm_train_poly = svm(y~.,data=training, kernel="polynomial", cost=0.01, scale=TRUE, degree=2)
svm_test_poly = svm(y~.,data=test, kernel="polynomial", cost=0.01, scale=TRUE, degree=2)

train_predict_poly = predict(svm_train_poly,training)
test_predict_poly = predict(svm_test_poly,test)

train_error_poly = sum(train_predict_poly!=train_set[,1])/length(train_predict_poly)
test_error_poly = sum(test_predict_poly!=test_set[,1])/length(test_predict_poly)

summary(svm_train_poly)
train_error_poly
test_error_poly






