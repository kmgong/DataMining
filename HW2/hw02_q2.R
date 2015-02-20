#############################
# Kevin Gong
# STAT W4240 
# Homework 2 , Problem 2
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
library(pixmap)

#################
# Problem 2a
#################

# the list of pictures
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
view_list = c( 'P00A+000E+00' , 'P00A+005E+10' , 'P00A+005E-10','P00A+010E+00')

# preallocate an empty list
this_face_row = vector()
# initialize an empty matrix of faces data
faces_matrix = vector()
pic_list = c( 01,02,03,04,05,06,07,08,09,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39)


#generating a matrix of photos of the 4 desired views

for (i in 1:38) {
	
placeholder_matrix <- NULL

for (j in 1:4) {
	filename= sprintf("CroppedYale/%s/%s_%s.pgm",dir_list_1[i],dir_list_1[i],view_list[j])
print(filename)
face=read.pnm(file=filename)
pic_data <-getChannels(face)
pic_data_vec <- as.vector(pic_data)
placeholder_matrix=rbind(placeholder_matrix,pic_data_vec)
}
faces_matrix=rbind(faces_matrix,placeholder_matrix)
}

dim(faces_matrix)


#################
# Problem 2b
#################

#computing the mean of the faces
faces.matrix_v2=as.matrix(faces_matrix)
means_col=colMeans(faces.matrix_v2)
means_mat=as.matrix(means_col)

#reshape this matrix into 192x168
means_mat_ref=matrix(means_mat,192,168)

#double check dimensions to make sure it worked
dim(means_mat_ref)

#generate image of mean face
means_face=pixmapGrey(means_mat_ref)
plot(means_face)

#save image of mean face
filename = 'hw02_02b.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

#################
# Problem 2c
#################

means_pca = prcomp(means_mat_ref)
summary(means_pca)
plot(summary(means_pca)$importance[3,], type="l", ylab="%variance explained", xlab="nth component")
means_pca$x

#################
# Problem 2d
#################

eigenfaces_matrix = matrix(summary(means_pca)$sdev[1:9], 3,3, byrow=TRUE)
eigenfaces_matrix
eigenfaces = pixmapGrey(eigenfaces_matrix)
plot(eigenfaces)


#################
# Problem 2e
#################

summary(means_pca)$loadings
means_pca

means_prin = princomp(means_mat_ref)
means_prin$loadings[1,]
means_prin$scores[,1]

dim(means_pca)


yaleB05 P00A+010E+00.pgm
face_05 = read.pnm(file = "CroppedYale/yaleB05/yaleB05_P00A+010E+00.pgm")
plot(face_05)


final_matrix=NULL
p=1

for(i in 1:5){ 
face_row=NULL 
	for(j in 1:5){ 
	
	if(i==1 && j==1){ 
		z=as.vector(means_mat_ref) 
		#z=rbind(z,z)
		}
	else { 
		means_mat=means_mat_ref+((as.matrix(means_pca$x[,p]))%*%t(as.matrix(means_pca$x[p,]))) 
		mean_face_t=t(means_mat)
		p=p+1
		face_vec=as.vector(mean_face_t)
		z=cbind(z,face_vec)
		}
		}
	final_matrix=rbind(final,z)
	}

faces = pixmapGrey(final_matrix)
plot(faces)

as.matrix(means_pca$x[,1])
t(as.matrix(means_pca[1,]))
loadings(means_pca)

head(means_pca)

dim(means_pca$x)




#################
# Problem 2e
#################

new_matrix = faces_matrix[5:152,]
dim(new_matrix)
new_pca = prcomp(new_matrix)
summary(new_pca)


final_matrix2=NULL
p=1

for(i in 1:5){ 
face_row=NULL 
	for(j in 1:5){ 
	
	if(i==1 && j==1){ 
		z=as.vector(means_mat_ref) 
		#z=rbind(z,z)
		}
	else { 
		means_mat=new_matrx+((as.matrix(new_pca$x[,p]))%*%t(as.matrix(new_pca$x[p,]))) 
		mean_face_t=t(means_mat)
		p=p+1
		face_vec=as.vector(mean_face_t)
		z=cbind(z,face_vec)
		}
		}
	final_matrix2=rbind(final,z)
	}

