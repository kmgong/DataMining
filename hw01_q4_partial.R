#############################
# Kevin Gong
# STAT W4240 
# Homework 1 , Problem 4
# 2/5/14
#
# The following code loads the eigenfaces data and
# performs a set of simple loading and plotting functions
#############################

#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine
setwd("~/Dropbox/SIPA/Data Mining")

# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.  From the
# command line, type install.packages("pixmap")
library(pixmap)

#################
# Problem 1a
#################

# paste or type in the given code here
face_01 = read.pnm(file = "CroppedYale/yaleB01/yaleB01_P00A-005E+10.pgm")

# now plot the data
plot(face_01)
# give it a nice title
title('hw01_01a: the first face')
# save the result
filename = 'hw01_01a.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

# extract the class and size

#----- START YOUR CODE BLOCK HERE -----#
class(face_01)
face_01@size
#alternative method to get size
nrow(getChannels(face_01))
ncol(getChannels(face_01))
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1b
#################

# make face_01 into a matrix with the given command
face_01_matrix = getChannels(face_01)

# load a second face
face_02 = read.pnm(file = "CroppedYale/yaleB02/yaleB02_P00A-005E+10.pgm")
face_02_matrix = getChannels(face_02)

# combine two faces into a single data matrix and make that a pixmap
faces_matrix = cbind( face_01_matrix , face_02_matrix )
faces = pixmapGrey( faces_matrix )

# plot to verify
plot(faces)

# find min and max values 

#----- START YOUR CODE BLOCK HERE -----#
range(faces_matrix)
#alternative method to get min and max values
min(faces_matrix)
max(faces_matrix)
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1c
#################

# get directory structure
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)

# find lengths

#----- START YOUR CODE BLOCK HERE -----#
length(dir_list_1)
length(dir_list_2)
#example elements
head(dir_list_1)
head(dir_list_2)
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1d
#################

# the list of pictures (note the absence of 14 means that 31 corresponds to yaleB32)
pic_list = c( 05 , 11 , 31 )
view_list = c(  'P00A-005E+10' , 'P00A-005E-10' , 'P00A-010E+00')

# preallocate an empty list
pic_data = vector("list",length(pic_list)*length(view_list))
# initialize an empty matrix of faces data
faces_matrix = vector()



face_01 = read.pnm(file = "CroppedYale/yaleB01/yaleB01_P00A-005E+10.pgm")


#----- START YOUR CODE BLOCK HERE -----#

##
pic_list = c( 05 , 11 , 31 )
view_list = c( 'P00A-005E+10' , 'P00A-005E-10' , 'P00A-010E+00')
i=1
j=3
filename = sprintf("CroppedYale/%s/%s_%s.pgm", dir_list_1[pic_list[i]] , dir_list_1[pic_list[i]] , view_list[j])

for(j in 1:3) {
	for(i in 1:3) {
	
	filename = sprintf("CroppedYale/%s/%s_%s.pgm", dir_list_1[pic_list[i]] , dir_list_1[pic_list[i]] , view_list[j])
	
	pic_data[i,j] = read.pnm(file = filename)
	
	
}
}
	
	pic_data[j] = rbind(pic_data, pic_data[i])

face_matrix = getChannels(face+i)
#pic_data=cbind(pic_data, pic_data[j])


face9 = read.pnm(file = "CroppedYale/yaleB12/yaleB12_P00A-005E+10.pgm")

face_05_p10 = read.pnm(file = "CroppedYale/yaleB05/yaleB05_P00A-005E+10.pgm")
face_05_m10 = read.pnm(file = "CroppedYale/yaleB05/yaleB05_P00A-005E-10.pgm")
face_05_00 = read.pnm(file = "CroppedYale/yaleB05/yaleB05_P00A-010E+00.pgm")

face_11_p10 = read.pnm(file = "CroppedYale/yaleB11/yaleB11_P00A-005E+10.pgm")
face_11_m10 = read.pnm(file = "CroppedYale/yaleB11/yaleB11_P00A-005E-10.pgm")
face_11_00 = read.pnm(file = "CroppedYale/yaleB11/yaleB11_P00A-010E+00.pgm")

face_31_p10 = read.pnm(file = "CroppedYale/yaleB31/yaleB31_P00A-005E+10.pgm")
face_31_m10 = read.pnm(file = "CroppedYale/yaleB31/yaleB31_P00A-005E-10.pgm")
face_31_00 = read.pnm(file = "CroppedYale/yaleB31/yaleB31_P00A-010E+00.pgm")

matrix_face_05_p10 = getChannels(face_05_p10)
matrix_face_05_m10 = getChannels(face_05_m10)
matrix_face_05_00 = getChannels(face_05_00)

matrix_face_11_p10 = getChannels(face_11_p10)
matrix_face_11_m10 = getChannels(face_11_m10)
matrix_face_11_00 = getChannels(face_11_00)

matrix_face_31_p10 = getChannels(face_31_p10)
matrix_face_31_m10 = getChannels(face_31_m10)
matrix_face_31_00 = getChannels(face_31_00)

col1 = rbind(matrix_face_05_p10, matrix_face_11_p10, matrix_face_31_p10)
col2 = rbind(matrix_face_05_m10, matrix_face_11_m10, matrix_face_31_m10)
col3 = rbind(matrix_face_05_00, matrix_face_11_00, matrix_face_31_00)

faces_matrix = cbind(col1, col2, col3)




face.matrix <- array(dim=c(192*3,168*3)) 
for(i in 1:3){ 
for (j in 1:3){ 
filename = sprintf("CroppedYale/%s/%s_%s.pgm", dir_list_1[pic_list[i]] , dir_list_1[pic_list[i]] , view_list[j]) 
face <- read.pnm(file=filename) 
matrix.face <- getChannels(face) 
face.matrix[i,j] <- face@size 
} 
}




pic_list = c( 05 , 11 , 31 )
view_list = c( 'P00A-005E+10' , 'P00A-005E-10' , 'P00A-010E+00')

faces_matrix = vector()
lign_faces_matrix= vector()


for (i in 1:3) {
	
lign_faces_matrix <- NULL

for (j in 1:3) {
	filename= sprintf("CroppedYale/%s/%s_%s.pgm",dir_list_1[pic_list[i]],dir_list_1[pic_list[i]],view_list[j])
print(filename)
face=read.pnm(file=filename)
pic_data <-getChannels(face)
lign_faces_matrix=cbind(lign_faces_matrix,pic_data)
}
faces_matrix=rbind(faces_matrix,lign_faces_matrix)
}











#----- END YOUR CODE BLOCK HERE -----#






# now faces_matrix has been built properly.  plot and save it.
faces = pixmapGrey(faces_matrix)
plot(faces)
# give it a nice title
title('hw01_01d: 3x3 grid of faces')
# save the result
filename = 'hw01_01d.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

#################
# End of Script
#################


