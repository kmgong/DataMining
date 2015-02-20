#############################
# Kevin Gong
# STAT W4240 
# Homework 3 , Problem 4
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
library(pixmap)
library(class)

#################
# Problem 4a
#################

views_4a = c('P00A+000E+00', 'P00A+005E+10', 'P00A+005E-10', 'P00A+010E+00' )

# load the data and save it as a matrix with the name face_matrix_4a

#----- START YOUR CODE BLOCK HERE -----#


# the list of pictures
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
views_4a = c('P00A+000E+00', 'P00A+005E+10', 'P00A+005E-10', 'P00A+010E+00' )

# preallocate an empty list
this_face_row = vector()
# initialize an empty matrix of faces data
face_matrix_4a = vector()
pic_list = c( 01,02,03,04,05,06,07,08,09,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39)


#generating a matrix of photos of the 4 desired views

for (i in 1:38) {
	
placeholder_matrix <- NULL

for (j in 1:4) {
	filename= sprintf("CroppedYale/%s/%s_%s.pgm",dir_list_1[i],dir_list_1[i],views_4a[j])
print(filename)
face=read.pnm(file=filename)
pic_data <-getChannels(face)
pic_data_vec <- as.vector(pic_data)
placeholder_matrix=rbind(placeholder_matrix,c(pic_data_vec,i,views_4a[j]))
}
face_matrix_4a=rbind(face_matrix_4a,placeholder_matrix)
}

dim(face_matrix_4a)


all_faces <- face_matrix_4a[,1:32256]
labelling <- face_matrix_4a[,32257:32258]

dim(all_faces)
dim(labelling)
colnames(labelling) <-c("subject","view")
labelling



#----- END YOUR CODE BLOCK HERE -----#

# Get the size of the matrix for use later
fm_4a_size = dim(face_matrix_4a)
# Use 4/5 of the data for training, 1/5 for testing
ntrain_4a = floor(fm_4a_size[1]*4/5) # Number of training obs
ntest_4a = fm_4a_size[1]-ntrain_4a # Number of testing obs
set.seed(1) # Set pseudo-random numbers so everyone gets the same output
ind_train_4a = sample(1:fm_4a_size[1],ntrain_4a) # Training indices
ind_test_4a = c(1:fm_4a_size[1])[-ind_train_4a] # Testing indices








#----- START YOUR CODE BLOCK HERE -----#
ind_train_4a
ind_test_4a





test_matrix=all_faces[ind_test_4a,]
dim(test_matrix)


train_matrix=all_faces[ind_train_4a,]
dim(train_matrix)



test_id = labelling[ind_test_4a,]
dim(test_id)


train_id = labelling[ind_train_4a,]
dim(train_id)

#find the rows referenced by the first 5 files of the training and testing sets
ind_train_4a[1:5]
ind_test_4a[1:5]

#find the first 5 files of the training and testing sets
labelling[ind_train_4a[1:5],]
labelling[ind_test_4a[1:5],]






#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 4b
#################

#----- START YOUR CODE BLOCK HERE -----#

#training data first

mean_face <- colMeans(train_matrix)
means_mat=as.matrix(mean_face)

#reshape this matrix into 192x168
means_mat_ref=matrix(means_mat,192,168)
means_face=pixmapGrey(means_mat_ref)
plot(means_face)



pic_mat_size = dim(train_matrix)
pic_mat_centered = mat.or.vec(pic_mat_size[1],pic_mat_size[2])

#subtract mean face from matrix
for (i in 1:pic_mat_size[1]){
	pic_mat_centered[i,] = train_matrix[i,] - mean_face
}

dim(pic_mat_centered)
pca_results <- prcomp(pic_mat_centered)

dim(pca_results)

dim(as.matrix(loading_temp))
dim(pic_mat_centered)



num_comp_mod = length(pca_results$x[,1])
scores_query = mat.or.vec(num_comp_mod,1)
trainingset=NULL
for (i in 1:121){
	placeholder = NULL
	
for(j in 1:25){
	loading_temp = pca_results$rotation[,j]
	scores_query = pic_mat_centered[i,] %*% loading_temp  
	placeholder = cbind(placeholder,scores_query)
	}
	trainingset=rbind(trainingset,placeholder)
}

dim(trainingset)








mean_face_test <- colMeans(test_matrix)
means_mat_test=as.matrix(mean_face_test)

#reshape this matrix into 192x168
means_mat_ref_test=matrix(means_mat_test,192,168)
means_face_test=pixmapGrey(means_mat_ref_test)
plot(means_face_test)



pic_mat_size_test = dim(test_matrix)
pic_mat_centered_test = mat.or.vec(pic_mat_size_test[1],pic_mat_size_test[2])

# I am using a loop, but one could use apply()
for (i in 1:pic_mat_size_test[1]){
	pic_mat_centered_test[i,] = test_matrix[i,] - mean_face
}

dim(pic_mat_centered_test)
pca_results_test <- prcomp(pic_mat_centered_test)

dim(as.matrix(pca_results_test))

dim(as.matrix(loading_temp_test))
dim(pic_mat_centered_test)



num_comp_mod_test = length(pca_results_test$x[,1])
scores_query_test = mat.or.vec(num_comp_mod_test,1)
testingset=NULL
for (i in 1:31){
	placeholder_test = NULL
	
for(j in 1:25){
	loading_temp_test = pca_results_test$rotation[,j] #note this is the same as training (I think it's supposed to be like this)
	scores_query_test = pic_mat_centered_test[i,] %*% loading_temp_test  
	placeholder_test = cbind(placeholder_test,scores_query_test)
	}
	testingset=rbind(testingset,placeholder_test)
}

dim(testingset)

#this works?
cl <- row.names(data.frame(trainingset))


cl2 <-factor(ind_train_4a)

length(indicator)
Y_test_hat <- knn(trainingset, testingset, cl2, k=1)
Y_test_hat!=cl2

sum(Y_test_hat!=cl)








#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 4c
#################

# Use different lighting conditions

views_4c = c('P00A-035E+15', 'P00A-050E+00', 'P00A+035E+15', 'P00A+050E+00')

# load your data and save the images as face_matrix_4c

#----- START YOUR CODE BLOCK HERE -----#




# the list of pictures
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
views_4c = c('P00A-035E+15', 'P00A-050E+00', 'P00A+035E+15', 'P00A+050E+00')

# preallocate an empty list
this_face_row = vector()
# initialize an empty matrix of faces data
face_matrix_4c = vector()
pic_list = c( 01,02,03,04,05,06,07,08,09,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39)


#generating a matrix of photos of the 4 desired views

for (i in 1:38) {
	
placeholder_matrix <- NULL

for (j in 1:4) {
	filename= sprintf("CroppedYale/%s/%s_%s.pgm",dir_list_1[i],dir_list_1[i],views_4c[j])
print(filename)
face=read.pnm(file=filename)
pic_data <-getChannels(face)
pic_data_vec <- as.vector(pic_data)
placeholder_matrix=rbind(placeholder_matrix,c(pic_data_vec,i,views_4c[j]))
}
face_matrix_4c=rbind(face_matrix_4c,placeholder_matrix)
}

dim(face_matrix_4c)


all_faces_4c <- face_matrix_4c[,1:32256]
labelling_4c <- face_matrix_4c[,32257:32258]




#----- END YOUR CODE BLOCK HERE -----#

fm_4c_size = dim(face_matrix_4c)
# Use 4/5 of the data for training, 1/5 for testing
ntrain_4c = floor(fm_4c_size[1]*4/5)
ntest_4c = fm_4c_size[1]-ntrain_4c
set.seed(2) # Set pseudo-random numbers
# You are resetting so that if you have used a random number in between the last use of sample(), you will still get the same output
ind_train_4c = sample(1:fm_4c_size[1],ntrain_4c)
ind_test_4c = c(1:fm_4c_size[1])[-ind_train_4c]

#----- START YOUR CODE BLOCK HERE -----#



ind_train_4c
ind_test_4c

test_matrix_4c=all_faces_4c[ind_test_4c,]
dim(test_matrix_4c)


train_matrix_4c=all_faces_4c[ind_train_4c,]
dim(train_matrix_4c)



test_id_4c = labelling_4c[ind_test_4c,]
dim(test_id_4c)


train_id_4c = labelling_4c[ind_train_4c,]
dim(train_id_4c)


#find the rows referenced by the first 5 files of the training and testing sets
ind_train_4c[1:5]
ind_test_4c[1:5]

#find the first 5 files of the training and testing sets
labelling_4c[ind_train_4c[1:5],]
labelling_4c[ind_test_4c[1:5],]





#training data first

mean_face_4c <- colMeans(train_matrix_4c)
means_mat_4c=as.matrix(mean_face_4c)

#reshape this matrix into 192x168
means_mat_ref_4c=matrix(means_mat_4c,192,168)
means_face_4c=pixmapGrey(means_mat_ref_4c)
plot(means_face_4c)



pic_mat_size_4c = dim(train_matrix_4c)
pic_mat_centered_4c = mat.or.vec(pic_mat_size_4c[1],pic_mat_size_4c[2])

#subtract mean face from matrix
for (i in 1:pic_mat_size_4c[1]){
	pic_mat_centered_4c[i,] = train_matrix_4c[i,] - mean_face_4c
}

dim(pic_mat_centered_4c)
pca_results_4c <- prcomp(pic_mat_centered_4c)

dim(pca_results_4c)

dim(as.matrix(loading_temp_4c))
dim(pic_mat_centered_4c)



num_comp_mod_4c = length(pca_results_4c$x[,1])
scores_query_4c = mat.or.vec(num_comp_mod_4c,1)
trainingset_4c=NULL
for (i in 1:121){
	placeholder_4c = NULL
	
for(j in 1:25){
	loading_temp_4c = pca_results_4c$rotation[,j]
	scores_query_4c = pic_mat_centered_4c[i,] %*% loading_temp_4c  
	placeholder_4c = cbind(placeholder_4c,scores_query_4c)
	}
	trainingset_4c=rbind(trainingset_4c,placeholder_4c)
}

dim(trainingset_4c)








mean_face_test_4c <- colMeans(test_matrix_4c)
means_mat_test_4c=as.matrix(mean_face_test_4c)

#reshape this matrix into 192x168
means_mat_ref_test_4c=matrix(means_mat_test_4c,192,168)
means_face_test_4c=pixmapGrey(means_mat_ref_test_4c)
plot(means_face_test_4c)



pic_mat_size_test_4c = dim(test_matrix_4c)
pic_mat_centered_test_4c = mat.or.vec(pic_mat_size_test_4c[1],pic_mat_size_test_4c[2])

# I am using a loop, but one could use apply()
for (i in 1:pic_mat_size_test_4c[1]){
	pic_mat_centered_test_4c[i,] = test_matrix_4c[i,] - mean_face_4c
}

dim(pic_mat_centered_test_4c)
pca_results_test_4c <- prcomp(pic_mat_centered_test_4c)

dim(as.matrix(pca_results_test_4c))

dim(as.matrix(loading_temp_test_4c))
dim(pic_mat_centered_test_4c)



num_comp_mod_test_4c = length(pca_results_test_4c$x[,1])
scores_query_test_4c = mat.or.vec(num_comp_mod_test_4c,1)
testingset_4c=NULL
for (i in 1:31){
	placeholder_test_4c = NULL
	
for(j in 1:25){
	loading_temp_test_4c = pca_results_test_4c$rotation[,j]
	scores_query_test_4c = pic_mat_centered_test_4c[i,] %*% loading_temp_test_4c  
	placeholder_test_4c = cbind(placeholder_test_4c,scores_query_test_4c)
	}
	testingset_4c=rbind(testingset_4c,placeholder_test_4c)
}

dim(testingset_4c)





#this works?
cl <- row.names(data.frame(testingset_4c))


cl2 <- factor(ind_train_4c)



length(indicator)
Y_test_hat_4c <- knn(trainingset_4c, testingset_4c, cl, k=1)
Y_test_hat_4c!=cl

sum(Y_test_hat_4c!=cl2)




#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 4d
#################

#----- START YOUR CODE BLOCK HERE -----#



#REPEAT 10 TIMES


fm_4c_size = dim(face_matrix_4c)
# Use 4/5 of the data for training, 1/5 for testing
ntrain_4c = floor(fm_4c_size[1]*4/5)
ntest_4c = fm_4c_size[1]-ntrain_4c
set.seed(2) # Set pseudo-random numbers
# You are resetting so that if you have used a random number in between the last use of sample(), you will still get the same output
ind_train_4c = sample(1:fm_4c_size[1],ntrain_4c)
ind_test_4c = c(1:fm_4c_size[1])[-ind_train_4c]



test_matrix_4c=all_faces_4c[ind_test_4c,]

train_matrix_4c=all_faces_4c[ind_train_4c,]

test_id_4c = labelling_4c[ind_test_4c,]

train_id_4c = labelling_4c[ind_train_4c,]






#training data first

mean_face_4c <- colMeans(train_matrix_4c)
means_mat_4c=as.matrix(mean_face_4c)

#reshape this matrix into 192x168
means_mat_ref_4c=matrix(means_mat_4c,192,168)


pic_mat_size_4c = dim(train_matrix_4c)
pic_mat_centered_4c = mat.or.vec(pic_mat_size_4c[1],pic_mat_size_4c[2])

#subtract mean face from matrix
for (i in 1:pic_mat_size_4c[1]){
	pic_mat_centered_4c[i,] = train_matrix_4c[i,] - mean_face_4c
}

pca_results_4c <- prcomp(pic_mat_centered_4c)


num_comp_mod_4c = length(pca_results_4c$x[,1])
scores_query_4c = mat.or.vec(num_comp_mod_4c,1)
trainingset_4c=NULL
for (i in 1:121){
	placeholder_4c = NULL
	
for(j in 1:25){
	loading_temp_4c = pca_results_4c$rotation[,j]
	scores_query_4c = pic_mat_centered_4c[i,] %*% loading_temp_4c  
	placeholder_4c = cbind(placeholder_4c,scores_query_4c)
	}
	trainingset_4c=rbind(trainingset_4c,placeholder_4c)
}




mean_face_test_4c <- colMeans(test_matrix_4c)
means_mat_test_4c=as.matrix(mean_face_test_4c)

#reshape this matrix into 192x168
means_mat_ref_test_4c=matrix(means_mat_test_4c,192,168)




pic_mat_size_test_4c = dim(test_matrix_4c)
pic_mat_centered_test_4c = mat.or.vec(pic_mat_size_test_4c[1],pic_mat_size_test_4c[2])

for (i in 1:pic_mat_size_test_4c[1]){
	pic_mat_centered_test_4c[i,] = test_matrix_4c[i,] - mean_face_4c
}

pca_results_test_4c <- prcomp(pic_mat_centered_test_4c)


num_comp_mod_test_4c = length(pca_results_test_4c$x[,1])
scores_query_test_4c = mat.or.vec(num_comp_mod_test_4c,1)
testingset_4c=NULL
for (i in 1:31){
	placeholder_test_4c = NULL
	
for(j in 1:25){
	loading_temp_test_4c = pca_results_test_4c$rotation[,j]
	scores_query_test_4c = pic_mat_centered_test_4c[i,] %*% loading_temp_test_4c  
	placeholder_test_4c = cbind(placeholder_test_4c,scores_query_test_4c)
	}
	testingset_4c=rbind(testingset_4c,placeholder_test_4c)
}



#this works?


cl2 <- factor(ind_train_4c)


Y_test_hat_4c <- knn(trainingset_4c, testingset_4c, cl2, k=1)
Y_test_hat_4c!=cl2
incorrect <- sum(Y_test_hat_4c!=cl2)
correct <- sum(Y_test_hat_4c==cl2)
rates <- matrix(c(incorrect, correct))
rownames(rates) <-c("incorrect","correct")
rates




#----- END YOUR CODE BLOCK HERE -----#

#################
# End of Script
#################


