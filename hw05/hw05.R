#############################
# Kevin Gong
# STAT W4240 
# Homework 045
# 4/16/14
#

#############################

#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine
setwd("~/Dropbox/SIPA/Data Mining/hw05")


# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.
# Use the package installer and be sure to install all dependencies
library(tm)
library(SnowballC)
library(rpart)
library(glmnet)

#################
# Problem 5a
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

head(dictionary)

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
##########################################

##########################################

head(dtm.hamilton.train)
dim(dtm.hamilton.test)
dim(dtm.madison.train)
dim(dtm.madison.test)

vector(1,35)
sapply(1:35,character(1))

class_htrain=matrix(nrow=35, ncol=1)
class_htrain[,1]=1
class_htrain
htrain <-cbind(dtm.hamilton.train,class_htrain)

class_htest=matrix(nrow=16, ncol=1)
class_htest[,1]=1
class_htest
htest <-cbind(dtm.hamilton.test,class_htest)

class_mtrain=matrix(nrow=15, ncol=1)
class_mtrain[,1]=0
class_mtrain
mtrain <-cbind(dtm.madison.train,class_mtrain)

class_mtest=matrix(nrow=11, ncol=1)
class_mtest[,1]=0
class_mtest
mtest <-cbind(dtm.madison.test,class_mtest)

all.training = rbind(htrain,mtrain)
all.testing = rbind(htest,mtest)

all.training[,4876]

colnames(all.training) = c(as.vector(dictionary$word),"y")
colnames(all.testing) = c(as.vector(dictionary$word),"y")

colnames(all.training)

all.training.frame = as.data.frame(all.training)
all.testing.frame = as.data.frame(all.testing)



tree.gini = rpart(all.training.frame$y~.,data=all.training.frame)
pred = predict(tree.gini,all.testing.frame)
pred
pred==all.testing[,4876]

correct = sum(pred==all.testing[,4876])/length(pred)
correct
false_pos = sum(pred>all.testing[,4876] & !pred==all.testing[,4876])/length(pred)
false_pos
false_neg = sum(pred<all.testing[,4876] & !pred==all.testing[,4876])/length(pred)
false_neg


correct
false_pos
false_neg

par(xpd=TRUE)
plot(tree.gini)
text(tree.gini)


##########################################

#################
# Problem 5b
#################

##########################################

tree.info = rpart(all.training.frame$y~.,data=all.training.frame,parms=list(split="information"))
pred2 = predict(tree.info,all.testing.frame)
!pred2==all.testing[,4876]
pred2

correct = sum(pred2==all.testing[,4876])/length(pred2)
correct
false_pos = sum(pred2>all.testing[,4876] & !pred2==all.testing[,4876])/length(pred2)
false_pos
false_neg = sum(pred2<all.testing[,4876] & !pred2==all.testing[,4876])/length(pred2)
false_neg

correct
false_pos
false_neg

par(xpd=TRUE)
plot(tree.info)
text(tree.info)



##########################################

#################
# Problem 6b
#################

##########################################
dtm.hamilton.train.n <- scale(dtm.hamilton.train,center=TRUE, scale=TRUE)
dtm.hamilton.test.n <- scale(dtm.hamilton.test,center=TRUE, scale=TRUE)
dtm.madison.train.n <- scale(dtm.madison.train,center=TRUE, scale=TRUE)
dtm.madison.test.n <- scale(dtm.madison.test,center=TRUE, scale=TRUE)

dim(dtm.hamilton.train.n)
head(dtm.hamilton.train.n)

dtm.hamilton.train.n[is.nan(dtm.hamilton.train.n)]=0
dtm.hamilton.test.n[is.nan(dtm.hamilton.test.n)]=0
dtm.madison.train.n[is.nan(dtm.madison.train.n)]=0
dtm.madison.test.n[is.nan(dtm.madison.test.n)]=0

all.training.s <- scale(all.training[,1:4875],center=TRUE, scale=TRUE)
all.testing.s <- scale(all.testing[,1:4875],center=TRUE, scale=TRUE)
head(all.testing.s)


all.training.s[is.nan(all.training.s)]=0
all.testing.s[is.nan(all.testing.s)]=0



class_htrain.n=matrix(nrow=35, ncol=1)
class_htrain.n[,1]=1
class_htrain.n
htrain.n <-cbind(dtm.hamilton.train.n,class_htrain.n)

class_htest.n=matrix(nrow=16, ncol=1)
class_htest.n[,1]=1
class_htest.n
htest.n <-cbind(dtm.hamilton.test.n,class_htest.n)

class_mtrain.n=matrix(nrow=15, ncol=1)
class_mtrain.n[,1]=0
class_mtrain.n
mtrain.n <-cbind(dtm.madison.train.n,class_mtrain.n)

class_mtest.n=matrix(nrow=11, ncol=1)
class_mtest.n[,1]=0
class_mtest.n
mtest.n <-cbind(dtm.madison.test.n,class_mtest.n)

all.training.n = rbind(htrain.n,mtrain.n)
all.testing.n = rbind(htest.n,mtest.n)

all.training.n[,3]

colnames(all.training.n) = c(as.vector(dictionary$word),"y")
colnames(all.testing.n) = c(as.vector(dictionary$word),"y")

colnames(all.training.n)

#all.training.frame.n = as.data.frame(all.training.n)
#all.testing.frame.n = as.data.frame(all.testing.n)

help(glmnet)
cv.glmnet()

ridge.train <- cv.glmnet(all.training.s[,1:4875],all.training.n[,4876],family="binomial",alpha=0)

ridge.train.betas <- glmnet(all.training.n[,1:4875],all.training.n[,4876],alpha=1,lambda=ridge.train$lambda.min,family="binomial")


head(coef(ridge.train))
#s = "lambda.min"

ridge.train
ridge.train$lambda
ridge.train.betas

ridge.train[is.nan(ridge.train)]=0


ridge.predicts <- predict(ridge.train,all.testing.s[,1:4875],s = "lambda.min",type="class")
ridge.predicts


as.vector(ridge.predicts)==all.testing.n[,4876]

correct.ridge = sum(as.vector(ridge.predicts)==all.testing.n[,4876])/27
correct.ridge
falsepos.ridge = sum(as.vector(ridge.predicts)>all.testing.n[,4876] & !as.vector(ridge.predicts)==all.testing[,4876])/27
falsepos.ridge
falseneg.ridge = sum(as.vector(ridge.predicts)<all.testing.n[,4876] & !as.vector(ridge.predicts)==all.testing[,4876])/27
falseneg.ridge


correct.ridge
falsepos.ridge
falseneg.ridge




ridge.coefs <- coef(ridge.train)
ridge.coefs.v <- as.vector(ridge.coefs)
which.max[ridge.coefs.v[2:4875]
ridge.coefs.v[order(ridge.coefs.v)[1:10]]

order(ridge.coefs.v)

which(tail(sort(ridge.coefs),11))

rownames(ridge.coefs)[order(abs(ridge.coefs), decreasing=TRUE)][1:11]
ridge.coefs["upon",]


important.ridge = matrix(nrow=11,ncol=2)
important.ridge[,1]= rownames(ridge.coefs)[order(abs(ridge.coefs), decreasing=TRUE)][1:11]
important.ridge[,2]= ridge.coefs.v[order(abs(ridge.coefs.v),decreasing=TRUE)[1:11]]
important.ridge



##########################################

#################
# Problem 6c
#################

##########################################
lasso.train <- cv.glmnet(all.training.s[,1:4875],all.training.n[,4876],family="binomial",alpha=1)

#ridge.train.betas <- glmnet(all.training.n[,1:4875],all.training.n[,4876],alpha=1,lambda=ridge.train$lambda.min,family="binomial")





sort(ridge.coefs)

ridge.coefs[5,]

ridge.coefs[432]

#s = "lambda.min"

lasso.train
ridge.train$lambda
ridge.train.betas

ridge.train[is.nan(ridge.train)]=0


lasso.predicts <- predict(lasso.train,all.testing.s[,1:4875],s = "lambda.min",type="class")
lasso.predicts

as.vector(ridge.predicts)==all.testing.n[,4876]

correct.lasso = sum(as.vector(lasso.predicts)==all.testing.n[,4876])/27
correct.lasso


correct.lasso = sum(as.vector(lasso.predicts)==all.testing.n[,4876])/27
correct.lasso
falsepos.lasso = sum(as.vector(lasso.predicts)>all.testing.n[,4876] & !as.vector(lasso.predicts)==all.testing[,4876])/27
falsepos.lasso
falseneg.lasso = sum(as.vector(lasso.predicts)<all.testing.n[,4876] & !as.vector(lasso.predicts)==all.testing[,4876])/27
falseneg.lasso


correct.lasso
falsepos.lasso
falseneg.lasso


coef(lasso.train)
lasso.coefs <- coef(lasso.train)
lasso.coefs.v <- as.vector(lasso.coefs)
which.max[lasso.coefs.v[2:4875]]
lasso.coefs.v[order(lasso.coefs.v)[1:10]]
lasso.coefs.v 

lasso.coefs.v[order(abs(lasso.coefs.v),decreasing=TRUE)[1:11]]

tail(sort(abs(lasso.coefs),decreasing=TRUE),11)


rownames(lasso.coefs)[order(abs(lasso.coefs), decreasing=TRUE)][1:11]

order(lasso.coefs, decreasing=TRUE)

important.lasso = matrix(nrow=11,ncol=2)
important.lasso[,1]= rownames(lasso.coefs)[order(abs(lasso.coefs), decreasing=TRUE)][1:11]
important.lasso[,2]= lasso.coefs.v[order(abs(lasso.coefs.v),decreasing=TRUE)[1:11]]
important.lasso


##########################################

#################
# Problem 7b
#################

##########################################
make.log.pvec <- function(dtm,mu){
    # Sum up the number of instances per word
    pvec.no.mu <- colSums(dtm)
    # Sum up number of words
    n.words <- sum(pvec.no.mu)
    # Get dictionary size
    dic.len <- length(pvec.no.mu)
    # Incorporate mu and normalize
    log.pvec <- log(pvec.no.mu + mu) - log(mu*dic.len + n.words)
    return(log.pvec)
}

#set our mu value
mu=1/4875

#generate the log probabilities for the Hamilton and Madison training and testing sets
logp.all.training <- make.log.pvec(all.training[,1:4875],mu)
logp.all.testing <- make.log.pvec(all.testing[,1:4875],mu)

#double check ranges to make sure they roughly between -2 to -25
range(logp.all.training)
range(logp.all.testing)

logp.all.training.m <- as.matrix(logp.all.training)
logp.all.testing.m <- as.matrix(logp.all.testing)

ranked.words <- rownames(logp.all.training.m)[order(logp.all.training.m, decreasing=TRUE)]
ranked.words[1:]
ranked.words 
all.training[,ranked.words[1:200]]

all.training.s[,ranked.words[1]]




ranked.words[1:20]

all.training[,ranked.words[1:20]]


all.training.frame


tree.gini.200 = rpart(all.training.frame$y~.,data=all.training.frame[,ranked.words[1:200]])
pred.200 = predict(tree.gini.200,all.testing.frame[,ranked.words[1:200]])
pred.200
!pred.200==all.testing[,4876]

correct = sum(pred.200==all.testing[,4876])/length(pred)
correct
false_pos = sum(pred.200>all.testing[,4876] & !pred.200==all.testing[,4876])/length(pred)
false_pos
false_neg = sum(pred.200<all.testing[,4876] & !pred.200==all.testing[,4876])/length(pred)
false_neg

correct
false_pos
false_neg

plot(tree.gini.200)
text(tree.gini.200)




tree.info.200 = rpart(all.training.frame$y~.,data=all.training.frame[,ranked.words[1:200]],parms=list(split="information"))
pred.200 = predict(tree.info.200,all.testing.frame[,ranked.words[1:200]])
pred.200
!pred.200==all.testing[,4876]


correct = sum(pred.200==all.testing[,4876])/length(pred2)
correct
false_pos = sum(pred.200>all.testing[,4876] & !pred.200==all.testing[,4876])/length(pred)
false_pos
false_neg = sum(pred.200<all.testing[,4876] & !pred.200==all.testing[,4876])/length(pred)
false_neg


plot(tree.info.200)
text(tree.info.200)




ridge.train.200 <- cv.glmnet(all.training.s[,ranked.words[1:200]],all.training.n[,4876],family="binomial",alpha=0)

ridge.predicts.200 <- predict(ridge.train.200,all.testing.s[,ranked.words[1:200]],s = "lambda.min",type="class")
ridge.predicts.200


correct.ridge.200 = sum(as.vector(ridge.predicts.200)==all.testing.n[,4876])/27
correct.ridge.200
falsepos.ridge.200 = sum(as.vector(ridge.predicts.200)>all.testing.n[,4876] & !as.vector(ridge.predicts.200)==all.testing[,4876])/27
falsepos.ridge.200
falseneg.ridge.200 = sum(as.vector(ridge.predicts.200)<all.testing.n[,4876] & !as.vector(ridge.predicts.200)==all.testing[,4876])/27
falseneg.ridge.200


correct.ridge.200
falsepos.ridge.200
falseneg.ridge.200




lasso.train.200 <- cv.glmnet(all.training.s[,ranked.words[1:200]],all.training.n[,4876],family="binomial",alpha=1)
lasso.predicts.200 <- predict(lasso.train.200,all.testing.s[,ranked.words[1:200]],s = "lambda.min",type="class")
lasso.predicts.200

correct.lasso.200 = sum(as.vector(lasso.predicts.200)==all.testing.n[,4876])/27
correct.lasso.200
falsepos.lasso.200 = sum(as.vector(lasso.predicts.200)>all.testing.n[,4876] & !as.vector(lasso.predicts.200)==all.testing[,4876])/27
falsepos.lasso.200
falseneg.lasso.200 = sum(as.vector(lasso.predicts.200)<all.testing.n[,4876] & !as.vector(lasso.predicts.200)==all.testing[,4876])/27
falseneg.lasso.200


correct.lasso.200
falsepos.lasso.200
falseneg.lasso.200


##########################################

#################
# End of Script
#################

