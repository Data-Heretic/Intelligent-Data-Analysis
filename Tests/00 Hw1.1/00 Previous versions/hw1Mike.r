library(stringr)
library(ggplot2)
library(gridExtra)
library(plyr)
library(reshape)

#mpg <- get(load("mpg.Rdata"))
mpg$tr <-substr(mpg$trans, 0, 1)
mpg$tr<-str_replace(mpg$tr,"^m","Manual") 
mpg$tr<-str_replace(mpg$tr,"^a","Automatic")

mpg<- mpg[mpg$fl!="c",]

#melt reshapes the dataset so its easier to manipulate through plots. 
#call View(mmpg) and View(mpg) to see the difference
mmpg<-melt(mpg,id=c("manufacturer","model","displ","year","cyl","trans","drv","fl","class","tr"))

#Q1 Automatic ~ Manual, cty ~ hwy (I dont know which one shows the result better)

g1<-ggplot(mmpg,aes(x=tr,y=value,fill=variable))+geom_boxplot()+labs(fill = "Miles",x="Type of transmission",y="NUmber of Miles")

g1.2<-ggplot(mmpg,aes(x=variable,y=value,fill=tr))+geom_boxplot()+labs(fill = "Type of Transmission",x=" ",y="NUmber of Miles")


#Q2 Fuel type 
#Changing the names of the fuel type in the dataset to look better in the plot.
#Again see the result by View(mmpg)

mmpg$fl<-factor(mpg$fl,levels=c("p","d","e","r","c"),labels=c("petrol","diesel","ethanol","regular","mistake"))
g2<-ggplot(mmpg,aes(x=variable,y=value,fill=fl))+geom_boxplot()+labs(fill = "Type of engine",x=" ",y="NUmber of Miles")

# Q2 Number of cylinders
g3<-ggplot(mmpg,aes(x=variable,y=value,fill=as.factor(cyl)))+geom_boxplot()+labs(fill = "Number of cylinders",x=" ",y="NUmber of Miles")

# HERE YOU GET THE OUTPUT All together, first line Q1, second line Q2
grid.arrange(g1,g1.2,g2,g3,nrow=2)

# Question 3,checking Engine Displacement throught the years. Histograms first

d1<-density(mpg$displ[mpg$year==1999])
d2<-density(mpg$displ[mpg$year==2008])

par(mfrow=c(1, 2))
  hist(mpg$displ[mpg$year==1999],breaks=10,probability = T,xlim=c(0, 8),ylim=c(0,0.5),col="gray", border="white",main="1999",xlab="cc")
  lines(d1,col="red",lwd=2)
  hist(mpg$displ[mpg$year==2008],breaks=10,probability = T,xlim=c(0, 8),ylim=c(0,0.5),col="gray", border="white",main="2008",xlab="cc",ylab=" ")
  lines(d2,col="red",lwd=2)
title("Engine displacement in litres per year", outer=TRUE)
# HERE YOU GET THE TOTAL OUTPUT

#Boxplots for some of the variables
  
  
g1<-ggplot(mpg,aes(x=as.factor(year),y=displ))+geom_boxplot()
g2<-ggplot(mpg,aes(x=as.factor(year),y=cyl))+geom_boxplot()
g3<-ggplot(mpg,aes(x=as.factor(year),y=cty))+geom_boxplot()
g4<-ggplot(mpg,aes(x=as.factor(year),y=hwy))+geom_boxplot()

grid.arrange(g1,g2,g3,g4,nrow=2)

