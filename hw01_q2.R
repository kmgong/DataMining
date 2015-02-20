#############################
# Kevin Gong
# STAT W4240 
# Homework 1 , Problem 2
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
install.packages("ISLR")
library(ISLR)


#################
# Problem 1a
#################

#importing the data
auto <- read.csv('~/Dropbox/SIPA/Data Mining/Autodata3.csv')

#examine which ones are quantitative/qualitative
summary(auto)
sapply(auto,class)

#quantitative: mpg, cylinders, displacement, horsepower, weight, acceleration, year, origin
#qualitative: name

#################
# Problem 1b
#################

#range of each quantitative predictor
sapply(auto[,1:8],range,na.rm=TRUE)


#################
# Problem 1c
#################

#mean and standard deviation of each quantitative predictor
sapply(auto[,1:8],mean,na.rm=TRUE)
sapply(auto[,1:8],sd,na.rm=TRUE)


#################
# Problem 1d
#################

#removing rows 10 through 85
auto = auto[-c(10:85),]

#range, mean, and standard deviation of remaining observations of each quantitative predictor
sapply(auto[,1:8],range,na.rm=TRUE)
sapply(auto[,1:8],mean,na.rm=TRUE)
sapply(auto[,1:8],sd,na.rm=TRUE)


#################
# Problem 1e
#################

#restore removed rows
auto <- read.csv('~/Dropbox/SIPA/Data Mining/Autodata3.csv')

#comparing relationships between predictors via scatterplots
pairs(auto)


















