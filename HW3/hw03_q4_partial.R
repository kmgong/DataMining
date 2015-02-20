#############################
# < Your Name Here >
# STAT W4240 
# Homework <HW Number> , Problem <Problem Number>
# < Homework Due Date >
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
placeholder_matrix=rbind(placeholder_matrix,c(pic_data_vec,i,j))
}
face_matrix_4a=rbind(face_matrix_4a,placeholder_matrix)
}

dim(face_matrix_4a)


all_faces <- face_matrix_4a[,1:32256]
labelling <- face_matrix_4a[,32257:32258]







#----- END YOUR CODE BLOCK HERE -----#

# Get the size of the matrix for use later
fm_4a_size = dim(face_matrix_4a)
# Use 4/5 of the data for training, 1/5 for testing
ntrain_4a = floor(fm_4a_size[1]*4/5) # Number of training obs
ntest_4a = fm_4a_size[1]-ntrain_4a # Number of testing obs
set.seed(1) # Set pseudo-random numbers so everyone gets the same output
ind_train_4a = sample(1:fm_4a_size[1],ntrain_4a) # Training indices
ind_test_4a = c(1:fm_4a_size[1])[-ind_train_4a] # Testing indices


ind_train_4a
ind_test_4a

test_matrix=all_faces[c(5,12,18,20,21,24,29,34,36,37,40,53,55,65,70,75,76,78,94,95,99,106,114,115,117,118,120,122,126,143,150),]
dim(test_matrix)

train_matrix=all_faces[-c(5,12,18,20,21,24,29,34,36,37,40,53,55,65,70,75,76,78,94,95,99,106,114,115,117,118,120,122,126,143,150),]
dim(train_matrix)

test_id = labelling[c(5,12,18,20,21,24,29,34,36,37,40,53,55,65,70,75,76,78,94,95,99,106,114,115,117,118,120,122,126,143,150),]
dim(test_id)

train_id = labelling[-c(5,12,18,20,21,24,29,34,36,37,40,53,55,65,70,75,76,78,94,95,99,106,114,115,117,118,120,122,126,143,150),]
dim(train_id)






#----- START YOUR CODE BLOCK HERE -----#


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

# I am using a loop, but one could use apply()
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

dim(pca_results_test)

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




#this isn't working
#row.names(scores_query)

#cl <- row.names(loading_temp)
#cl <- ind_train_4a
#names

#ind_train_4a_31 <- ind_train_4a[1:31]
#ind_train_4a_31
#cl <- ind_train_4a_25



#this works?
cl <- row.names(data.frame(trainingset))


cl2 <- vector(c("11","15","22","34","8","34","35","24","23","3","37","7","27","14","27","18","25","34","13","26","31","7","22","5","9","13","1","12","27","11"))
15,19,15,6,38,20,24,4,21,12,23,18,22,16))




indicator =c(11,15,22,34,8,34,35,24,23,3,37,7,27,14,27,18,25,34,13,26,31,7,22,5,9,13,1,12,27,11,15,19,15,6,38,20,24,4,21,12,23,18,22,16,15,33,1,34,20,38,13,28,11,36,2,3,8,32,16,10,21,7,31,36,33,30,23,17,2,51,33,17,21,36,29,35,37,23,22,18,8,27,20,26,13,4,39,28,11,35,4,1,10,21,12,19,7,37,25,9,32,5,17,26,37,3,2,6,38,14,35,32,16,33,38,1,11,28,16,12,26)

a=factor(indicator)

length(cl)
cl












length(indicator)
knn(trainingset, testingset, a, k=1)










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
placeholder_matrix=rbind(placeholder_matrix,c(pic_data_vec,i,j))
}
face_matrix_4a=rbind(face_matrix_4a,placeholder_matrix)
}

dim(face_matrix_4a)


all_faces <- face_matrix_4a[,1:32256]
labelling <- face_matrix_4a[,32257:32258]






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


#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 4d
#################

#----- START YOUR CODE BLOCK HERE -----#


#----- END YOUR CODE BLOCK HERE -----#

#################
# End of Script
#################


