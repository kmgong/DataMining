#############################
# Kevin Gong
# STAT W4240 
# Homework 06, Problem 1
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
library(gbm)
library(glmnet)
library(randomForest)


#################
# Problem 1a
#################

data("Hitters")
Hitters

H2 <- Hitters[!is.na(Hitters$Salary),,drop=F]
H2$Salary <- log(H2$Salary)
H2$Salary
Salary
#################
# Problem 1b
#################

H2.train <- H2[1:200,]
H2.test <- H2[201:nrow(H2),]

dim(H2.train)
dim(H2.test)


#################
# Problem 1c
#################

library(gbm)
set.seed(5)

shrink.lambdas=c(0.00001,0.0001,0.001,0.01,0.1,1)
training_error=rep(NA,length(shrink.lambdas))
testing_error=rep(NA,length(shrink.lambdas))
#shrink.lambdas = sl
#interaction.depth=1, n.cores=10


for (i in 1:length(shrink.lambdas)){ 
	
	#sl = shrink.lambdas[i]
	
	boost_hitters = gbm(Salary~., data=H2.train, distribution="gaussian", n.trees=1000, shrinkage = shrink.lambdas[i]) 
	
	predictions_train = predict(boost_hitters,H2.train, n.trees=1000)
	predictions_test = predict(boost_hitters,H2.test, n.trees=1000)
	
	training_error[i]=mean((predictions_train - H2.train$Salary)^2)
	testing_error[i]=mean((predictions_test - H2.test$Salary)^2) 
	
	} 
	

plot(log(shrink.lambdas),training_error)


#################
# Problem 1d
#################


plot(log(shrink.lambdas),testing_error)


#################
# Problem 1e
#################


#boosting

boost_results = training_error[which.min(training_error)]


#best subset lm

library(leaps)

placeholder = regsubsets(Salary~., data=H2.train, nvmax=19)
placeholder_s = summary(placeholder)

a = placeholder_s$which[which.min(placeholder_s$cp),][2:20]

names = c(names,"Division","Salary")

placeholder.lm = lm(Salary~.,data=H2.train[,colnames(H2.train)%in%names])
subsetlm_prediction = predict(placeholder.lm,H2.test[,colnames(H2.train)%in%names])
bestsub = mean((subsetlm_prediction-H2.test$Salary)^2)





#best lasso

library(glmnet)

eliminate=c("League","Division","NewLeague")
reformat=model.matrix(~.,H2)[,-1]
reformat.train=reformat[1:200,]
reformat.test=reformat[201:nrow(reformat),]

fit_lasso=cv.glmnet(reformat.train[,colnames(reformat)!="Salary"],reformat.train[,"Salary"])


fit_lasso=glmnet(reformat.train[,colnames(reformat)!="Salary"],reformat.train[,"Salary"],lambda=fit$lambda.1se)



pred=predict(fit_lasso,reformat.test[,colnames(reformat)!="Salary"])
best.lasso=mean((pred[,1]-H2.test$Salary)^2)





#compare the test MSEs

boost_results
bestsub 
best.lasso 



#the lasso is the best by a really little bit on the test data, but boosting came in close.



#################
# Problem 1f
#################

boost_hitters_test = gbm(Salary~., data=H2.train, distribution="gaussian", n.trees=1000, shrinkage = shrink.lambdas[which.min(training_error)], interaction.depth=1, n.cores=10)

summary(boost_hitters_test)



#################
# Problem 1g
#################

library(randomForest)
set.seed(1)

bagging_hitters = randomForest(Salary~.,data=H2.train, mtry=ncol(H2.train)-1, importance=TRUE)
#ntree=500
#mtry=19

prediction_bag = predict(bagging_hitters,newdata=H2.test)
mean((prediction_bag-H2.test$Salary)^2)












