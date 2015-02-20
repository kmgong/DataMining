#############################
# Kevin Gong
# STAT W4240 
# Homework 1 , Problem 1
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
college <- read.csv('http://www-bcf.usc.edu/~gareth/ISL/College.csv')
college
head(college)

#################
# Problem 1b
#################

#adding a column with university names
fix(college)
rownames(college) <- college[,1]
fix(college)

#eliminating the original column of university names
college <- college[,-1]
fix(college)

#################
# Problem 1c
#################


#
# Part i
#


#numerical summary of the variables in the data set
summary(college)


#
# Part ii
#

#scatterplot matrix of first 10 variables
pairs(college[,1:10])


#
# Part iii
#

#side-by-side boxplots of Private and Outstate
plot(college$Private,college$Outstate)

#
# Part iv
#

#generate new variable Elite based on whether more than 50% of incoming class come from the top 10% of their high school
Elite=rep("No",nrow(college ))
Elite[college$Top10perc >50]=" Yes"
Elite=as.factor(Elite)
college=data.frame(college ,Elite)

#we see there are 78 Elite universities
summary(college)

#side-by-side boxplots of Private and Outstate
plot(college$Elite,college$Outstate)


#
# Part v
#

par(mfrow=c(2,2))
hist(college$Apps, breaks=5)
hist(college$Apps, breaks=10)
hist(college$Apps, breaks=25)
hist(college$Apps, breaks=75)


par(mfrow=c(2,2))
hist(college$Accept, breaks=5)
hist(college$Accept, breaks=10)
hist(college$Accept, breaks=25)
hist(college$Accept, breaks=75)


par(mfrow=c(2,2))
hist(college$F.Undergrad, breaks=5)
hist(college$F.Undergrad, breaks=10)
hist(college$F.Undergrad, breaks=25)
hist(college$F.Undergrad, breaks=75)


par(mfrow=c(2,2))
hist(college$Expend, breaks=5)
hist(college$Expend, breaks=10)
hist(college$Expend, breaks=25)
hist(college$Expend, breaks=75)

par(mfrow=c(2,2))
hist(college$Grad.Rate, breaks=5)
hist(college$Grad.Rate, breaks=10)
hist(college$Grad.Rate, breaks=25)
hist(college$Grad.Rate, breaks=75)

#
# Part vi
#

mean(college$Expend[college$Elite=="Yes"])
summary(college$Expend,college$Elite=="No")