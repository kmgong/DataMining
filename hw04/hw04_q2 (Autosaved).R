#############################
# Kevin Gong
# STAT W4240 
# Homework 04 
# 4/2/14
#
# The following code analyzes the federalist papers
#############################

#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine
setwd("~/Dropbox/SIPA/Data Mining/hw04")

# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.
# Use the package installer and be sure to install all dependencies
library(tm)
library(SnowballC)
library(Snowball)


#################
# Problem 1a
#################

log.prior.hamilton = log(35/50)
log.prior.madison = log(15/50)

naive.bayes <- function(logp.hamilton.train, logp.madison.train, log.prior.hamilton, log.prior.madison, dtm.test)

{
  mads_document <- vector()
  hams_document <- vector()
  mads_words <- vector()
  hams_words <- vector()
  classification <- vector() 
  
  
for (i in 1:nrow(dtm.test)){
  	
    mads_words[i] <- sum(dtm.test[i,]*logp.madison.train)
    hams_words[i] <- sum(dtm.test[i,]*logp.hamilton.train)
    mads_document[i] <- sum(mads_words[i]) + log.prior.madison  
    hams_document[i] <- sum(hams_words[i]) + log.prior.hamilton
  
 }
  
for (k in 1:nrow(dtm.test))
  
  {
  	
    if(hams_document[k] > mads_document[k]){
    	
    	classification[k] <- "Hamilton"
    	
    	}
    	
    else{
    	
    	classification[k] <- "Madison"
    	
    	}
  }
  
  results_matrix <- data.frame(log.prob.hamilton = hams_document, log.prob.madison = mads_document, classifier = classification)
  return(results_matrix)
}


ham <- naive.bayes(logp.hamilton.train, logp.madison.train, log.prior.hamilton, log.prior.madison, dtm.hamilton.test)
mad <- naive.bayes(logp.hamilton.train, logp.madison.train, log.prior.hamilton, log.prior.madison, dtm.madison.test)

true_pos = (sum(as.vector(ham$classifier)=="Hamilton"))/16
true_neg = (sum(as.vector(mad$classifier)=="Madison"))/11
false_pos = (sum(as.vector(mad$classifier)=="Hamilton"))/11
false_neg = (sum(as.vector(ham$classifier)=="Madison"))/16


true_pos
true_neg
false_pos
false_neg




#####
#####
#####
#start reading here








hamilton_numbers = 1:35
madison_numbers = 1:15

A <- sample(hamilton_numbers,7)
E <- sample(hamilton_numbers[-A],7)
J <- sample(hamilton_numbers[-c(A,E)],7)
N <- sample(hamilton_numbers[-c(A,E,J)],7)
R <- sample(hamilton_numbers[-c(A,E,J,N)],7)


A
E
J
N
R


B <- sample(madison_numbers,3)
G <- sample(madison_numbers[-B],3)
K <- sample(madison_numbers[-c(B,G)],3)
O <- sample(madison_numbers[-c(B,G,K)],3)
S <- sample(madison_numbers[-c(B,G,K,O)],3)


B
G
K
O
S




ham_testing1 <- dtm.hamilton.train[A,]
mad_testing1 <- dtm.madison.train[B,]
ham_training1 <- dtm.hamilton.train[-A,]
mad_training1 <- dtm.madison.train[-B,]

ham_testing2 <- dtm.hamilton.train[E,]
mad_testing2 <- dtm.madison.train[G,]
ham_training2 <- dtm.hamilton.train[-E,]
mad_training2 <- dtm.madison.train[-G,]

ham_testing3 <- dtm.hamilton.train[J,]
mad_testing3 <- dtm.madison.train[K,]
ham_training3 <- dtm.hamilton.train[-J,]
mad_training3 <- dtm.madison.train[-K,]

ham_testing4 <- dtm.hamilton.train[N,]
mad_testing4 <- dtm.madison.train[O,]
ham_training4 <- dtm.hamilton.train[-N,]
mad_training4 <- dtm.madison.train[-O,]

ham_testing5 <- dtm.hamilton.train[R,]
mad_testing5 <- dtm.madison.train[S,]
ham_training5 <- dtm.hamilton.train[-R,]
mad_training5 <- dtm.madison.train[-S,]

dim(ham_testing1)
dim(ham_training1)






correct <- matrix(nrow=5,ncol=5)
correct

false_pos_final <- matrix(nrow=5,ncol=5)
false_neg_final <- matrix(nrow=5,ncol=5)

false_pos_final
false_neg_final


mu_values = c(1/(10*4875), 1/4875, 10/4875, 100/4875, 1000/4875)

log.prior.hamilton = log(35/50)
log.prior.madison = log(15/50)






for(i in 1:5){
	logp.hamilton.train_temp <- vector()
	logp.madison.train_temp <- vector()
	hams1 = vector()
	mads1 = vector()
	correct1 = vector()
	false_positive1 = vector()
	false_negative1 = vector()
	hams2 = vector()
	mads2 = vector()
	correct2 = vector()
	false_positive2 = vector()
	false_negative2 = vector()
	hams3 = vector()
	mads3 = vector()
	correct3 = vector()
	false_positive3 = vector()
	false_negative3 = vector()
	hams4 = vector()
	mads4 = vector()
	correct4 = vector()
	false_positive4 = vector()
	false_negative4 = vector()
	hams5 = vector()
	mads5 = vector()
	correct5 = vector()
	false_positive5 = vector()
	false_negative5 = vector()
	
for(k in 1:5){
	
	if(k==1){
	
	logp.hamilton.train_temp <- make.log.pvec(ham_training1,mu_values[i])
	logp.madison.train_temp <- make.log.pvec(mad_training1,mu_values[i])
		
		
	hams1 <- naive.bayes(logp.hamilton.train_temp, logp.madison.train_temp, log.prior.hamilton, log.prior.madison, ham_testing1)
	mads1 <- naive.bayes(logp.hamilton.train_temp, logp.madison.train_temp, log.prior.hamilton, log.prior.madison, mad_testing1)

	correct1 = ((sum(as.vector(hams1$classifier)=="Hamilton")) + (sum(as.vector(mads1$classifier)=="Madison")))/10
	false_positive1 = (sum(as.vector(mads1$classifier)=="Hamilton"))/3
	false_negative1 = (sum(as.vector(hams1$classifier)=="Madison"))/7	
	
	
	correct[i,k] = correct1
	false_pos_final[i,k] = false_positive1
	false_neg_final[i,k] = false_negative1
		
	}	
		
		
	else if(k==2){
	
	logp.hamilton.train_temp <- make.log.pvec(ham_training2,mu_values[i])
	logp.madison.train_temp <- make.log.pvec(mad_training2,mu_values[i])
		
		
	hams2 <- naive.bayes(logp.hamilton.train_temp, logp.madison.train_temp, log.prior.hamilton, log.prior.madison, ham_testing2)
	mads2 <- naive.bayes(logp.hamilton.train_temp, logp.madison.train_temp, log.prior.hamilton, log.prior.madison, mad_testing2)

	correct2 = ((sum(as.vector(hams2$classifier)=="Hamilton")) + (sum(as.vector(mads2$classifier)=="Madison")))/10
	false_positive2 = (sum(as.vector(mads2$classifier)=="Hamilton"))/3
	false_negative2 = (sum(as.vector(hams2$classifier)=="Madison"))/7	
	
	
	correct[i,k] = correct2
	false_pos_final[i,k] = false_positive2
	false_neg_final[i,k] = false_negative2
		
	}
		
	else if(k==3){
	
	logp.hamilton.train_temp <- make.log.pvec(ham_training3,mu_values[i])
	logp.madison.train_temp <- make.log.pvec(mad_training3,mu_values[i])
		
		
	hams3 <- naive.bayes(logp.hamilton.train_temp, logp.madison.train_temp, log.prior.hamilton, log.prior.madison, ham_testing3)
	mads3 <- naive.bayes(logp.hamilton.train_temp, logp.madison.train_temp, log.prior.hamilton, log.prior.madison, mad_testing3)

	correct3 = ((sum(as.vector(hams3$classifier)=="Hamilton")) + (sum(as.vector(mads3$classifier)=="Madison")))/10
	false_positive3 = (sum(as.vector(mads3$classifier)=="Hamilton"))/3
	false_negative3 = (sum(as.vector(hams3$classifier)=="Madison"))/7	
	
	
	correct[i,k] = correct3
	false_pos_final[i,k] = false_positive3
	false_neg_final[i,k] = false_negative3
		
	}

		
	else if(k==4){
	
	logp.hamilton.train_temp <- make.log.pvec(ham_training4,mu_values[i])
	logp.madison.train_temp <- make.log.pvec(mad_training4,mu_values[i])
		
		
	hams4 <- naive.bayes(logp.hamilton.train_temp, logp.madison.train_temp, log.prior.hamilton, log.prior.madison, ham_testing4)
	mads4 <- naive.bayes(logp.hamilton.train_temp, logp.madison.train_temp, log.prior.hamilton, log.prior.madison, mad_testing4)

	correct4 = ((sum(as.vector(hams4$classifier)=="Hamilton")) + (sum(as.vector(mads4$classifier)=="Madison")))/10
	false_positive4 = (sum(as.vector(mads4$classifier)=="Hamilton"))/3
	false_negative4 = (sum(as.vector(hams4$classifier)=="Madison"))/7	
	
	
	correct[i,k] = correct4
	false_pos_final[i,k] = false_positive4
	false_neg_final[i,k] = false_negative4
		
	}	
		
	
	else {
	
	logp.hamilton.train_temp <- make.log.pvec(ham_training5,mu_values[i])
	logp.madison.train_temp <- make.log.pvec(mad_training5,mu_values[i])
		
		
	hams5 <- naive.bayes(logp.hamilton.train_temp, logp.madison.train_temp, log.prior.hamilton, log.prior.madison, ham_testing5)
	mads5 <- naive.bayes(logp.hamilton.train_temp, logp.madison.train_temp, log.prior.hamilton, log.prior.madison, mad_testing5)

	correct5 = ((sum(as.vector(hams5$classifier)=="Hamilton")) + (sum(as.vector(mads5$classifier)=="Madison")))/10
	false_positive5 = (sum(as.vector(mads5$classifier)=="Hamilton"))/3
	false_negative5 = (sum(as.vector(hams5$classifier)=="Madison"))/7	
	
	
	correct[i,k] = correct5
	false_pos_final[i,k] = false_positive5
	false_neg_final[i,k] = false_negative5
		
	}	
	
		
		
	}
	
	
	
	
	
}





correct
false_pos_final
false_neg_final

rownames(correct) <- c("1/(10*D)","1/D","10/D","100/D","1000/D")
colnames(correct) <- c("Fold 1","Fold 2", "Fold3", "Fold 4", "Fold 5")

rownames(false_pos_final) <- c("1/(10*D)","1/D","10/D","100/D","1000/D")
colnames(false_pos_final) <- c("Fold 1","Fold 2", "Fold3", "Fold 4", "Fold 5")

rownames(false_neg_final) <- c("1/(10*D)","1/D","10/D","100/D","1000/D")
colnames(false_neg_final) <- c("Fold 1","Fold 2", "Fold3", "Fold 4", "Fold 5")



correct
false_pos_final
false_neg_final

colors <- c("green", "cyan", "purple", "red", "black")
markers = 1:5
categories = c("1/(10*D)","1/D","10/D","100/D","1000/D")
matplot(t(correct), type="o",lty=1, col=colors, pch=markers ,bty="n",las=1, ylab="Proportion Correctly Classified", xlab="Fold #", main="Performance of Different Mu Values in Classifying Documents")
legend("topleft",col=colors, pch=markers,categories,lwd=1)



colors <- c("green", "cyan", "purple", "red", "black")
markers = 1:5
categories = c("1/(10*D)","1/D","10/D","100/D","1000/D")
matplot(t(false_pos_final), type="o",lty=1, col=colors, pch=markers ,bty="n",las=1, ylab="Proportion of False Positives", xlab="Fold #", main="Performance of Different Mu Values - False Positives")
legend("bottomleft",col=colors, pch=markers,categories,lwd=1)


colors <- c("green", "cyan", "purple", "red", "black")
markers = 1:5
categories = c("1/(10*D)","1/D","10/D","100/D","1000/D")
matplot(t(false_neg_final), type="o",lty=1, col=colors, pch=markers ,bty="n",las=1, ylab="Proportion of False Negatives", xlab="Fold #", main="Performance of Different Mu Values - False Negatives")
legend("topleft",col=colors, pch=markers,categories,lwd=1)












complete_correct = matrix(nrow=5,ncol=1)
complete_correct

complete_false_pos_final <- matrix(nrow=5,ncol=1)
complete_false_neg_final <- matrix(nrow=5,ncol=1)

complete_false_pos_final
complete_false_neg_final


mu_values = c(1/(10*4875), 1/4875, 10/4875, 100/4875, 1000/4875)

log.prior.hamilton = log(35/50)
log.prior.madison = log(15/50)



for(i in 1:5){
	
	
	logp.hamilton.train_temp <- make.log.pvec(dtm.hamilton.train,mu_values[i])
	logp.madison.train_temp <- make.log.pvec(dtm.madison.train,mu_values[i])
		
		
	hams1 <- naive.bayes(logp.hamilton.train_temp, logp.madison.train_temp, log.prior.hamilton, log.prior.madison, dtm.hamilton.test)
	mads1 <- naive.bayes(logp.hamilton.train_temp, logp.madison.train_temp, log.prior.hamilton, log.prior.madison, dtm.madison.test)

	correct1 = ((sum(as.vector(hams1$classifier)=="Hamilton")) + (sum(as.vector(mads1$classifier)=="Madison")))/27
	false_positive1 = (sum(as.vector(mads1$classifier)=="Hamilton"))/11
	false_negative1 = (sum(as.vector(hams1$classifier)=="Madison"))/16	
	
	
	complete_correct[i,1] = correct1
	complete_false_pos_final[i,1] = false_positive1
	complete_false_neg_final[i,1] = false_negative1
		
	}	

complete_correct
complete_false_pos_final
complete_false_neg_final


rownames(complete_correct) <- c("1/(10*D)","1/D","10/D","100/D","1000/D")
rownames(complete_false_pos_final) <- c("1/(10*D)","1/D","10/D","100/D","1000/D")
rownames(complete_false_neg_final) <- c("1/(10*D)","1/D","10/D","100/D","1000/D")

complete_correct
complete_false_pos_final
complete_false_neg_final



colors <- c("green", "cyan", "purple", "red", "black")
markers = 1:5
categories = c("1/(10*D)","1/D","10/D","100/D","1000/D")
matplot(t(complete_correct), lty=1, col=colors, pch=markers ,bty="n",las=1, ylab="Proportion Correctly Classified", xlab="Fold #", main="Performance of Different Mu Values in Classifying Documents",ylim=c(0,1))
legend("topleft",col=colors, pch=markers,categories,lwd=1)



colors <- c("green", "cyan", "purple", "red", "black")
markers = 1:5
categories = c("1/(10*D)","1/D","10/D","100/D","1000/D")
matplot(t(complete_false_pos_final), lty=1, col=colors, pch=markers ,bty="n",las=1, ylab="Proportion of False Positives", xlab="Fold #", main="Performance of Different Mu Values - False Positives",ylim=c(0,1))
legend("bottomleft",col=colors, pch=markers,categories,lwd=1)


colors <- c("green", "cyan", "purple", "red", "black")
markers = 1:5
categories = c("1/(10*D)","1/D","10/D","100/D","1000/D")
matplot(t(complete_false_neg_final), lty=1, col=colors, pch=markers ,bty="n",las=1, ylab="Proportion of False Negatives", xlab="Fold #", main="Performance of Different Mu Values - False Negatives",ylim=c(0,0.1))
legend("topleft",col=colors, pch=markers,categories,lwd=1)


percentage_error = matrix(nrow=5,ncol=6)
percentage_error
rownames(percentage_error) <- c("1/(10*D)","1/D","10/D","100/D","1000/D")
colnames(percentage_error) <- c("Fold 1","Fold 2", "Fold3", "Fold 4", "Fold 5","Average")

for(i in 1:5){
percentage_error[,i]= abs(complete_correct-correct[,i])/complete_correct
}

for(i in 1:5){
percentage_error[i,6]= mean(percentage_error[i,1:5])
}

percentage_error
