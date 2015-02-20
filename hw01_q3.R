#############################
# Kevin Gong
# STAT W4240 
# Homework 1 , Problem 3
# 2/5/14
#
#############################

#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine
setwd("~/Dropbox/SIPA/Data Mining")

# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.
install.packages("MASS")
library(MASS)


#################
# Problem 3a
#################

#count the number of rows in Boston
?Boston


#################
# Problem 3b
#################

#creating pairwise scatterplots
pairs(Boston)


#################
# Problem 3d
#################

#range, mean, and standard deviation of Boston predictors
sapply(Boston,range,na.rm=TRUE)
sapply(Boston,mean,na.rm=TRUE)
sapply(Boston,sd,na.rm=TRUE)
summary(Boston)

#find which have highest crime rates, tax rates, and pupil-teacher ratios
which.max(Boston$crim)
which.max(Boston$tax)
which.max(Boston$ptratio)




#################
# Problem 3e
#################

#counting the number of suburbs bordering the Charles River
sum(Boston$chas==1)
sum(Boston$chas==0)
sum(is.na(Boston$chas)) #check for missing data


#################
# Problem 3f
#################

#median pupil-teacher teacher among the towns
summary(Boston$ptratio)


#################
# Problem 3g
#################

#finding the lowest median value of owner-occupied homes
which.min(Boston$medv)

#values of the other predictors for suburb 399
Boston[399,]

#compare the predictor values of suburb 399 with overall predictor ranges
summary(Boston)


#################
# Problem 3h
#################

#finding the number of suburbs averaging more than 7 rooms per dwelling
sum(Boston$rm>7)

#finding the number of suburbs averaging more than 8 rooms per dwelling
sum(Boston$rm>8)

#compare the predictor values of the suburb averaging more than 8 rooms per dwelling
which(Boston$rm>8)
Boston[c(98,164,205,225,226,227,233,234,254,258,263,268,365),]
summary(Boston)







