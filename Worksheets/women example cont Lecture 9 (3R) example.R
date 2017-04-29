women<-read.csv("worksheet2_women.csv",header=F)
names(women)<-c('Height','Weight')
women
attach(women)
ls(pos=2) #look at what the components are

#plot the data adn add in simple linear regression
plot(Height,Weight)
abline(lsfit(Height,Weight)$coef)

#identify the points at the edges on the plots
identify(Height,Weight)

hat(Height)
hat(Weight)
barplot(hat(Weight),names.arg=Weight)
abline(h=4/length(Weight))
barplot(hat(Height),names.arg=Height)
abline(h=4/length(Height))
