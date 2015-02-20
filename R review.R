print('hello world')
getwd()
ls()

a = 29
class(a)
b = as(a, "integer")
b
class(b)
c = as(a, "character")
c
class(c)

b="ch"
class(b)
c=FALSE
class(c)
c=c("log", "ic", "al")
c
class(c)
c[2]
b[2]
b[1]

a=c(1,3,5)
class(a)
a
a[2]
a[3]
a[1]
a.vec=c(1,3,5)
a[2]=67
a

b=as(a,"integer")
b
b[2]=67.1
b
class(b)
b[3]="hello"
b
class(b)

Z=matrix(c(1,2,3,11,12,13))
z=c(1,2,3,11,12,13)
z[4]
Z[4,1]
z[4,1]
Z=matrix(c(1,2,3,11,12,13), nrow=2, ncol=3)
Z
Z[2,3]
Z=matrix(c(1,2,3,11,12,13), nrow=2, ncol=3, byrow=TRUE)
Z

Z=matrix(c(1,2,3,11,12,13), nrow=2, ncol=3, byrow=TRUE, dimnames=list(c("r1", "r2"), c("c1","c2", "c3")))
Z[2,]

z=1:10
z=seq(1,10,2)
z=1:10

dim(z)=c(5,2)

z=list(1:10)
z
length(z)
z[1]
y=z[[1]]
y

Z=matrix(c(1,2,3,11,12,13), nrow=2, ncol=3, byrow=TRUE, dimnames=list(c("r1", "r2"), c("c1","c2", "c3")))
Y=cbind(c(1,2,3), c(4,5,6))
Y
Y=rbind(c(1,2,3), c(4,5,6))
Y

YY=Y*Y
YY

W=cbind(c(3,4,6,7), c(1,1,1,1))
W
Z
#both rank 2
WZ = W %*% Z
WZ

t(Z)
WZ %*% Z
WZ %*% t(Z)

names = c("phyllis", "feihan", "evan")
test1 = c(95,85,92)
names
test1
test2 = c(92,88,99)
names
rm(names)
names
names(Z)
scores=data.frame(fullnames=c("phyllis","feihan","evan"), t1=test1, t2=test2)
scores
names(scores)
scores$fullnames
scores$t1

class(scores$fullnames)
data=c(1,2,2,3,1,2,3,3,1,2,3,1)
data
fdata=factor(data)
fdata
rdata=factor(data, labels=c("I","II", "III"))
rdata
rdata=factor(data, labels=c("survive","death", "ill"))
rdata
fdata

ls()

Z
myarray=array(seq(1,24,1), dim=c(3,4,2))
#array is a multi dimensional matrix
myarray
myarray[2,4,2]
myarray[2,4,2]=TRUE
myarray

xx=FALSE
!xx

rm(x)
mean(x=1:10)
x


#how to save a graph
pdf("figure0,pdf")
plot (x,y, etc)
dev.off()

install.packages("pixmap")










