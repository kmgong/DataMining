#############################
# Kevin Gong
# STAT W4240 
# Homework 2 , Problem 1
# 2/19/14
#

#############################

#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine
setwd("~/Dropbox/SIPA/Data Mining/HW2")

# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.  From the
# command line, type install.packages("pixmap")
library(pdist)

#################
# Problem 1a
#################

set1 = read.csv(file="hw02_q1_p1.csv")
head(set1)
colMeans(set1,na.rm=TRUE)
rowMeans(set1,na.rm=TRUE)


#################
# Problem 1b
#################

set1_centered = scale(set1, center=TRUE)
set1_emp = cov(set1_centered)
set1_emp

#################
# Problem 1c
#################

eigen(set1_emp)


#################
# Problem 1d
#################

set1_pca = princomp(set1_emp)
set1_pca$scores
set1_pca$loadings


#################
# Problem 1e ??
#################

set1_pca$sdev^2/sum(set1_pca$sdev^2)

cumsum(set1_pca$sdev^2/sum(set1_pca$sdev^2))

plot(cumsum(set1_pca$sdev^2/sum(set1_pca$sdev^2)),type='l',xlab="# components included", ylab="proportion of variance captured")


#################
# Problem 1f
#################

set2 = read.csv(file="hw02_q1_p2.csv")
set2_centered = scale(set2, center=TRUE)
set2_pca = princomp(set2_centered)
set2_pca$scores


#################
# Problem 1g
#################

W = set1_pca$loadings[,1:2]
X = set2_centered[1:5,1:5]
Y = X*t(W)
#euclidean distances
sqrt(sum((Y-set1_centered))^2)
pdist(Y,set1_centered)
dist(Y,set1_centered)

#################
# Problem 1h
#################

errors = set2_pca$sdev-set1_pca$sdev
as.vector(errors)



















