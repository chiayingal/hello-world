hello<-function()cat("hello,world!\n")
hello()
x<-1:100/10
y=x^2y
plot(x,y,main="main title", ylab="ylab",xlab="xlab",xlim=c(5,10))
std<-sqrt(var(y))
3!=5
3==5
3<5||3>5
c<-5
c^3
c-1.96*std #lower CI
c+1.96*std #upper CI
rm(std)
BMI<-function(x,y){x/(y^2)}
t(rbind(x,y,z))