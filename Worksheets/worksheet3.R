#Solutions to Worksheet 3.
set.seed(1) #so everytime will get the same value

#Q1a
normals <- rnorm(1000,3,4)

#Q1b
hist(normals)

#Q1c
min(normals) 
max(normals)

#Q1d

hist(normals, breaks=seq(floor(min(normals)),floor(max(normals)+1),1)) #the last argument is the width

floor(min(normals)) #round up function
floor(max(normals))
seq(floor(min(normals)),floor(max(normals)+1))

hist(normals, breaks=seq(floor(min(normals)),floor(max(normals)+1),0.5)) 

hist(normals, breaks=seq(floor(min(normals)),floor(max(normals)+1),0.25))

hist(normals, breaks=seq(floor(min(normals)),floor(max(normals)+1),0.1))

##############

#hist(normals, breaks=seq(floor(min(normals)),floor(max(normals)+1),1.5))
#This may not work, as the breaks may no longer span the range of values in
#normals. In that case, expand the base of the histogram slightly:

hist(normals, breaks=seq(floor(min(normals)-2),floor(max(normals)+2),1.5))

hist(normals, breaks=seq(floor(min(normals)-4),floor(max(normals)+4),2))

#############

#Q1e

grid <- seq(floor(min(normals)),floor(max(normals)+1),1) #grid is a sequence function (a straight horizontal line)
grid
#want a smooth line(pdf) of the normal distribution: f(x)=(1/sqrt(2*pi*s^2))exp((x-mean)^2/2s^2)
really.normal <- dnorm(grid,3,4) #dnorm=normal pdf function
really.normal
    
x<-c(-1,0,2)
dnorm(x,0,1)
exp(-0.5*(x)^2)/sqrt(2*pi) #show that dnorm=pdf of normal dist 

hist(normals, breaks=seq(floor(min(normals)),floor(max(normals)+1),1))

lines(grid,really.normal)

lines(grid,1000*1*really.normal) #convert raw scale frequency

#Now you can try this out yourself on the other histograms.

#Q1f

qqnorm(normals) #errors need to be normally ditributed, use qqnorm plot to test normality
#Looks pretty linear!
    qqline(normals)

#Q1g

expo <- rexp(15,1)

mean(expo) 
var(expo)

qqnorm(expo)
qqline(expo)
#The solution to question 2 was given at the end of the worksheet.

#Excludes Point 1

betachng <- function(resp,pred,excl){
  
exc <- unique(excl)
if(min(excl)<1) {
print("Invalid Point to be Excluded - Index too small") }
else if(max(excl)>length(pred)) {
print("Invalid Point to be Excluded - Index too large") }
else {
beta <- lsfit(pred,resp)$coef
beta.red <-lsfit(pred[-exc],resp[-exc])$coef
beta - beta.red }
}

#########

betachng(Height,Weight,1)
    
names(women)<-c("Height","Weight")
colnames(women)
attach(women)
    
#Excludes Points 1,2 and 5

betachng(Height,Weight,c(1,2,5)) 
