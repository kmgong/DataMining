#############################
# Kevin Gong
# STAT W4240 
# Homework 06, Problem 4
# May 05, 2014
#
# The following code analyzes the federalist papers
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
library(tm)
library(SnowballC)
library(rpart)
library(glmnet)

# to get the svm function...
library(e1071)

# to load previous dtm code, etc.
source("../hw04/hw04.R")
setwd("~/Documents/academic/teaching/STAT_W4240_2014_SPRG/dropbox/Homework/hw04")

#################
# Problem 4
#################

##########################################
# Preprocess the data
# You should have made directiories in 
# hw04 for cleaned data; make sure these
# are in your path
##########################################

##########################################
# To read in data from the directories:
# Partially based on code from C. Shalizi
read.directory <- function(dirname) {
    # Store the infiles in a list
    infiles = list();
    # Get a list of filenames in the directory
    filenames = dir(dirname,full.names=TRUE);
    for (i in 1:length(filenames)){
        infiles[[i]] = scan(filenames[i],what="",quiet=TRUE);
         }
    return(infiles)
}
hamilton.train <- read.directory('fp_hamilton_train_clean')
hamilton.test <- read.directory('fp_hamilton_test_clean')
madison.train <- read.directory('fp_madison_train_clean')
madison.test <- read.directory('fp_madison_test_clean')
##########################################

##########################################
# Make dictionary sorted by number of times a word appears in corpus 
# (useful for using commonly appearing words as factors)
# NOTE: Use the *entire* corpus: training, testing, spam and ham
make.sorted.dictionary.df <- function(infiles){
    # This returns a dataframe that is sorted by the number of times 
    # a word appears
  
    # List of vectors to one big vetor
    dictionary.full <- unlist(infiles) 
    # Tabulates the full dictionary
    tabulate.dic <- tabulate(factor(dictionary.full)) 
    # Find unique values
    dictionary <- unique(dictionary.full) 
    # Sort them alphabetically
    dictionary <- sort(dictionary)
    dictionary.df <- data.frame(word = dictionary, count = tabulate.dic)
    sort.dictionary.df <- dictionary.df[order(dictionary.df$count,decreasing=TRUE),];
    return(sort.dictionary.df)
}
dictionary <- make.sorted.dictionary.df(c(hamilton.train,hamilton.test,madison.train,madison.test))

##########################################

##########################################
# Make a document-term matrix, which counts the number of times each 
# dictionary element is used in a document
make.document.term.matrix <- function(infiles,dictionary){
    # This takes the text and dictionary objects from above and outputs a 
    # document term matrix
    num.infiles <- length(infiles);
    num.words <- nrow(dictionary);
    # Instantiate a matrix where rows are documents and columns are words
    dtm <- mat.or.vec(num.infiles,num.words); # A matrix filled with zeros
    for (i in 1:num.infiles){
        num.words.infile <- length(infiles[[i]]);
        infile.temp <- infiles[[i]];
        for (j in 1:num.words.infile){
            ind <- which(dictionary == infile.temp[j])[[1]];
            # print(sprintf('%s,%s', i , ind))
            dtm[i,ind] <- dtm[i,ind] + 1;
            #print(c(i,j))
        }
    }
return(dtm);
}

dtm.hamilton.train <- make.document.term.matrix(hamilton.train,dictionary)
dtm.hamilton.test <- make.document.term.matrix(hamilton.test,dictionary)
dtm.madison.train <- make.document.term.matrix(madison.train,dictionary)
dtm.madison.test <- make.document.term.matrix(madison.test,dictionary)

# check if the 4 dtm datasets are correct
dim(dtm.hamilton.train)		# 35 4875
dim(dtm.hamilton.test)		# 16 4875
dim(dtm.madison.train)		# 15 4875
dim(dtm.madison.test)		# 11 4875
##########################################

##########################################
# make training and test sets w/ 
# y=0 if Madison; =1 if Hamilton
# & var names being dictionary words
dat.train <- as.data.frame(rbind(dtm.hamilton.train, dtm.madison.train))
dat.test <- as.data.frame(rbind(dtm.hamilton.test, dtm.madison.test))

names(dat.train) <- names(dat.test) <- as.vector(dictionary$word)

dat.train$y <- as.factor(c(rep(1, nrow(dtm.hamilton.train)), rep(0, nrow(dtm.madison.train))))
dat.test$y <- as.factor(c(rep(1, nrow(dtm.hamilton.test)), rep(0, nrow(dtm.madison.test))))
# note: as.factor() makes the y label as factor, which is helpful for svm later (so it will do classification)

dim(dat.train)		# 50 4876
dim(dat.test)		# 27 4876


##################################
# center and scale the data as in HW05
mean.train <- apply(dat.train[,-4876], 2, mean)		# col means of training x
sd.train <- apply(dat.train[,-4876], 2, sd)			# col sd of training x

x.train <- scale(dat.train[,-4876])					# standardize training x
x.train[,sd.train==0] <- 0							# let the var be 0 if its sd=0

x.test <- scale(dat.test[,-4876], center = mean.train, scale=sd.train)		# use training x mean & sd to standardize test x
x.test[,sd.train==0] <- 0							# let the var be 0 if its sd=0

y.train <-dat.train$y
y.test <- dat.test$y


############################
# Problem 4
############################

###########
# Part a
###########

training = data.frame(x=x.train[,1:100],y=y.train)
testing = data.frame(x=x.test[,1:100],y=y.test)

svm_1 = svm(y~.,data=training, kernel="linear", cost=10, scale=FALSE)

test_prediction = predict(svm_1,testing)
test_prediction
testing$y
sum(test_prediction==testing$y)

###########
# Part b
###########

a = seq(from=5, to=100, by=5)

train_errors = rep(NA,20)
test_errors = rep(NA,20)

for(i in 1:20) {
	
	training_data = data.frame(x=x.train[,1:a[i]],y=y.train)
	testing_data = data.frame(x=x.test[,1:a[i]], y=y.test)
	
	train_prediction = predict(svm_1,training_data)
	test_prediction = predict(svm_1,testing_data)
	
	train_errors[i] = sum(train_prediction!=y.test)/length(train_prediction)
	test_errors[i] = sum(test_prediction!=y.test)/length(test_prediction)
	
}

plot(a, test_errors, col="green", pch=5, type="b", xlab="# Words", ylab="Test Error")












###########
# Part c
###########

svm_2 = svm(y~.,data=training, kernel="radial", cost=10, scale=FALSE)

a = seq(from=5, to=100, by=5)

train_errors = rep(NA,20)
test_errors = rep(NA,20)

for(i in 1:20) {
	
	training_data = data.frame(x.train[,1:a[i]],y=y.train)
	
	testing_data = data.frame(x=x.test[,1:a[i]], y=y.test)
	
	train_prediction.2 = predict(svm_2,training_data)
	test_prediction.2 = predict(svm_2,testing_data)
	
	train_errors[i] = sum(train_prediction.2!=y.test)/length(train_prediction.2)
	test_errors[i] = sum(test_prediction.2!=y.test)/length(test_prediction.2)
	
}

plot(a, test_errors, col="red", pch=5, type="b", xlab="# Words", ylab="Test Error")







###########
# Part d
###########


features = c("upon", "depart")

features_training = data.frame(x=x.train[,features],y=y.train)

svm_features = svm(y~., data=features_training, kernel="radial", cost=10, scale=FALSE)
plot(svm_features, x.train[,features])

with(svm_features,plot(x.train[,features]))



