#############################
# Kevin Gong
# STAT W4240 
# Homework 3 , Problem 3
# 3/5/14
#
# The following code loads the eigenfaces data and
# performs a set of simple loading and plotting functions
#############################

#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine
setwd("~/Dropbox/SIPA/Data Mining/HW3")

# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.  

#################
# Problem 3a
#################

# load the data and use dist() to get a distance matrix


#----- START YOUR CODE BLOCK HERE -----#

#import the data
dataset <- read.csv(file="hw03_q3.csv")
head(dataset)

#set aside x1 and x2 in their own matrix
dataset_x_only <- dataset[,1:2]
head(dataset_x_only)

#compute the distances between x1 and x2 using dist()
distances <-as.matrix(dist(dataset_x_only))
distances
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 3b
#################

#----- START YOUR CODE BLOCK HERE -----#
dataset_y_only <- dataset[,3]
head(dataset_y_only)

#separate data into testing and training sets
test <- dataset_x_only[1,]
test
train <- dataset_x_only[2:20,]
train

#allocate empty placeholder vectors and matrices for our for loop
smallest=matrix(nrow=10,ncol=10)
MSE_test=vector()
MSE_train=vector()



#First we calculate the training MSE. We make sure to include the point itself when doing so, hence [1:k]
for(k in 1:10){
	abba <- order(distances[,1],decreasing=F)[1:k]
	smallest[k,(1:k)] = dataset[abba,3]
	yhat=sum(dataset[abba,3])/k
	MSE_train[k]=((dataset[1,3]-yhat)^2)/19
	}

#Next we calculate the testing MSE. We make sure to avoid including the testing point itself when doing so, hence [2:(k+1)]
for(k in 1:10){
	abba <- order(distances[,1],decreasing=F)[2:(k+1)] 
	smallest[k,(1:k)] = dataset[abba,3]
	yhat=sum(dataset[abba,3])/k
	MSE_test[k]=(dataset[1,3]-yhat)^2
	}
	
#examine the results. The "smallest" matrix allows us to check the y values used to approximate our test y for each k value.
MSE_test
MSE_train
smallest



#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 3c
#################

#----- START YOUR CODE BLOCK HERE -----#



#allocate empty placeholder vectors and matrices for our for loops
MSE_test=vector()
MSE_train=vector()

smallest=matrix(nrow=190,ncol=10)
MSE_test_all=NULL
MSE_train_all=NULL



#We calculate the training MSE. We make sure to include the point itself when doing so, hence [1:k]
for(i in 2:20){
	MSE_train=vector()
for(k in 1:10){
	abba <- order(distances[,i],decreasing=F)[1:k]
	smallest[k,(1:k)] = dataset[abba,3]
	yhat=sum(dataset[abba,3])/k
	MSE_train[k]=((dataset[i,3]-yhat)^2)/19
	}
	MSE_train_all=rbind(MSE_train_all,MSE_train)
}

#We calculate the testing MSE. We make sure to avoid including the testing point itself when doing so, hence [2:(k+1)]
for(i in 2:20){
	MSE_test=vector()
	MSE_train=vector()

for(k in 1:10){
	abba <- order(distances[,i],decreasing=F)[2:(k+1)]
	smallest[k,(1:k)] = dataset[abba,3]
	yhat=sum(dataset[abba,3])/k
	MSE_test[k]=(dataset[i,3]-yhat)^2
	}
	MSE_test_all=rbind(MSE_test_all,MSE_test)
}

#examine the resulting MSEs
MSE_test_all
MSE_train_all



#remane the column names to the different values of k
colnames(MSE_test_all) <-c("k=1","k=2","k=3","k=4","k=5","k=6","k=7","k=8","k=9","k=10")
colnames(MSE_train_all) <-c("k=1","k=2","k=3","k=4","k=5","k=6","k=7","k=8","k=9","k=10")

#rename the row names to the ith data point
rownames(MSE_test_all)<-c(2:20)
rownames(MSE_train_all)<-c(2:20)

#examine renamed MSE matrices
MSE_test_all
MSE_train_all


#average the mean MSEs by k value and see which k value has the lowest average mean squared error
colMeans(MSE_test_all)
colMeans(MSE_train_all)
which.min(colMeans(MSE_test_all))
which.min(colMeans(MSE_train_all))


#----- END YOUR CODE BLOCK HERE -----#

#################
# End of Script
#################


