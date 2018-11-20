library("ggplot2")
library("GGally")

######################################## Reading data set #######################
setwd("E:/Masters - Data Science/Data Analysis/Chapter 1 -Descriptive Statistics and Statistical Modelling/Regression Models/Assignment 2.1")
diamonds<- read.table("HW-diamonds.txt")
names(diamonds) <- c('Weight','ColourPurity','Clarity', 'Certifier','Price')

#Creating linear model
#ggpairs(diamonds)
levels(diamonds$ColourPurity)
levels(diamonds$Clarity)
diamonds$Clarity=as.factor(diamonds$Clarity)
diamonds$ColourPurity=as.factor(diamonds$ColourPurity)


######################################## Question One ##########################
plot(diamonds$Carat ,diamonds$Price)
plot(diamonds$Carat ,log(diamonds$Price))


diamonds$ColourPurity<-relevel(diamonds$ColourPurity, ref="I")
diamonds$Clarity<-relevel(diamonds$Clarity, ref="VS2")

model1 <- lm(formula = log(Price) ~ Weight+ColourPurity+Clarity+Certifier , data=diamonds)
summary(model1)


model1$coeff[c("Weight")]


#Plotting some graphs
par(mfrow=c(2,2))
plot(model1, which=c(1:4), ask=F)

library(lmtest)
library(tseries)

#Check residuals dependancy
dwtest(model1, alternative="two.sided")

#Check normality for residuals
jarque.bera.test(model1$residuals)

#Check equal variances for residuals
bptest(model1)

## Removing Outliers
diamonds[152,]
diamonds=diamonds[-152,]
diamonds=diamonds[-214,]
diamonds=diamonds[-110,]

#Getting model after removing the outliers
modelNoOutlier <- lm(formula = log(Price) ~ Weight+ColourPurity+Clarity+Certifier , data=diamonds)
summary(modelNoOutlier)

par(mfrow=c(2,2))
plot(modelNoOutlier, which=c(1:4), ask=F)

##################################################################################
##3.A

#Stratifying Carat into groups
breaks <- c(0, 0.5, 1 ,1.1) # intervals
labels <- c("Small", "Medium", "Large") # groups names
Carat_Size <- cut(diamonds$Weight, breaks, labels) # cut income into intervals using breaks and codes each value to the corresponding interval
diamonds <- cbind(diamonds, Carat_Size) # add wealth as a column to ds
head(diamonds)

diamonds$Carat_Size=as.factor(diamonds$Carat_Size)
diamonds$Carat_Size<-relevel(diamonds$Carat_Size, ref="Small")

model2 <- lm(formula = log(Price) ~ Weight+ColourPurity+Clarity+Certifier+Carat_Size+Carat_Size*Weight , data=diamonds)
summary(model2)
plot(model2)

#Check residuals dependancy
dwtest(model2, alternative="two.sided")

#Check normality for residuals
jarque.bera.test(model2$residuals)

#Check equal variances for residuals
bptest(model2)



##Testing Colour purity differences
test=read.table("Test.txt")
names(test) <- c('Weight','ColourPurity','Clarity', 'Certifier','Carat_Size')
predict(model2,test[1,], interval = "confidence")##D
predict(model2,test[2,], interval = "confidence")##I
predict(model2,test[3,], interval = "confidence")##E

##Testing certifier differences
test2=read.table("Test2.txt")
names(test2) <- c('Weight','ColourPurity','Clarity', 'Certifier','Carat_Size')
predict(model2,test2[1,], interval = "confidence")##GIA  
predict(model2,test2[2,], interval = "confidence")##IGI
predict(model2,test2[3,], interval = "confidence")##HRD


#Check residuals dependancy
dwtest(model2, alternative="two.sided")

#Check normality for residuals
jarque.bera.test(model2$residuals)

#Check equal variances for residuals
bptest(model2)


##################################################################################
##3.B

diamonds3<- read.table("HW-diamonds.txt")
names(diamonds3) <- c('Weight','ColourPurity','Clarity', 'Certifier','Price')
sqrt_Carat_Size=(diamonds3$Weight)*2
diamonds3 <- cbind(diamonds3, sqrt_Carat_Size) # add wealth as a column to ds
head(diamonds3)
model3 <- lm(formula = log(Price) ~ Weight+ColourPurity+Clarity+Certifier+sqrt_Carat_Size , data=diamonds3)
summary(model3)
plot(model3)


#Check residuals dependancy
dwtest(model3, alternative="two.sided")

#Check normality for residuals
jarque.bera.test(model3$residuals)

#Check equal variances for residuals
bptest(model3)

