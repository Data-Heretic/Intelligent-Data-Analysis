# Importing data sets
medifis=read.table("medifis.txt")
colnames(medifis)=c("gender","height","weight","foot","arm","back","skull","knee")
hbat=read.csv("hbat.csv",header=TRUE,sep=",")
# if we have imported the id column as first column, delete it
head(hbat)
hbat=hbat[-1]

# Covariance and Correlation matrix, omitting the gender variable
cov(medifis[,-1])
r=cor(medifis[,-1])
# which variables correlate the most?
diag(r)=0
which(r==max(abs(r)), arr.ind=TRUE)

# Correlation coefficients and tests
library(Hmisc)
rcorr(as.matrix(medifis[,-1]))

# Visualizing correlations: Package corrplot
library(corrplot)
corrplot(cor(medifis[,-1]))
corrplot.mixed(cor(medifis[,-1]))
# reordering the correlation matrix: there are different methods. Sometimes it is 
# useful for minning the hidden structure and pattern in the matrix.
corrplot.mixed(cor(medifis[,-1]),order="AOE" ) # Angular Order of the Eigenvectors
corrplot.mixed(cor(medifis[,-1]),order="FPC" ) # First Principal Component
corrplot(cor(medifis[,-1]), order="hclust",hclust.method="ward.D", addrect=3) # Ward's hierarchical clustering method

#Visualizing: Package corrgram
library(corrgram)
corrgram(medifis[,-1])

corrgram(medifis[,-1],
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

corrgram(medifis[,-1], order=TRUE,
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

corrgram(medifis[,-1], order=TRUE,
         lower.panel=panel.shade, upper.panel=panel.pts,
         diag.panel=panel.minmax, text.panel=panel.txt)

#Partial correlations, package ppcor. Detailed output
library(ppcor)
pcor(medifis[,-1])
matrix.partial=pcor(medifis[,-1])$estimate

#Visualizing partial correlations
corrgram(matrix.partial,
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

corrplot.mixed(matrix.partial,order="AOE" )


#Define a function r2multv for squared multiple correlation coeficients (r-squared)

r2multv<-function(x){
  r2s=1-1/(diag(solve(cov(x)))*diag(cov(x)))
  r2s
}

#use it on data set "medifis"
r2multv(medifis)


# What is the variable "more linearly explained" by the others?
#use this function on hbat dataset
r2multv(hbat[,6:22])

#In this data set, important linear relationships are present. Let's calculate the determinant of S and R:
det(cor(hbat[,6:22]))
det(cov(hbat[,6:22]))

#Can you find the variables involved in the overall linear dependence?
eigen(cov(hbat[,6:22]))
# However, linear pairwise correlations between those variables are not very strong
cor(hbat[,c(11,17,18)])
#But R^2's are
r2multv(hbat[,c(11,17,18)])

# Effective dependence coefficient
1-det(cor(medifis[,-1]))^{1/6}
#Variables from 6 to 19 from hbat
1-det(cor(hbat[,6:19]))^{1/13}
#Variables from 6 to 22 from hbat
1-det(cor(hbat[,6:22]))^{1/16}

# Testing normality assumptions
# There are numerous tests to check a normality assumption: Shapiro-Wilk's test for small samples $n<50$. Lilliefors' test (package nortest) for bigger samples.
library(nortest)
lillie.test(hbat[,8])
shapiro.test(medifis[,3])
# Any conclusion?
#Other tests include Anderson-Darling test, Cramer Von-Misses and Jarque-Bera (tseries package, based on a joint statistics of skewness and kurtosis)
cvm.test(hbat[,8])
library(tseries)
# Just to choose one, we will definite use Jarque-Bera's test when testing normality
jarque.bera.test(hbat[,8])
# The Jarque-Bera and Shapiro-Wilk seem to work significantly better when testing normality

#### Test for skewness and kurtosis (if normality is failing, for instance)
library(moments)
anscombe.test(hbat[,6])  
#kurtosis
agostino.test(hbat[,6])
# Why is it failing normality in variable hbat[,6]? Because of the kurtosis of the distribution

#We can try a transformation on hbat[,6] but it doesn't improve normality
jarque.bera.test(hbat[,6])
jarque.bera.test(hbat[,6]^2)

# Variable transformation, new data rdoorcl and rdoorcl	
rdoorcl=read.table("rdoorcl.txt")
rdoorop=read.table("rdoorop.txt")
library(car)
qqPlot(rdoorcl$V1, dist="norm")
shapiro.test(rdoorcl$V1)
# qqPlot is designed to give us the information regarding tests on normality. IT's still possible to use
# package ggplot2 for plotting, but you get less information
# The line depicted in qqPlot has to do with the mean and standard deviation of the data
ggplot(rdoorcl, aes(sample=rdoorcl$V1))+stat_qq()+geom_abline(intercept=0.128,slope=.1)
mean(rdoorcl$V1)
sd(rdoorcl$V1)

# Power Transformations, Box-Cox transformation to improve normality, linear relationship between two variables,
# and/or constant variance. Here, we are interesting in improving normality (sometimes it is not possible)
# (package "car")
powerTransform(rdoorcl$V1)
# and more information
summary(powerTransform(rdoorcl$V1))
#We make a variable transformation using lambda=0.27
rdoorclt=bcPower(rdoorcl, lambda=0.27)
#and check if it improves normality
#Comparing both qqplots
par(mfrow=c(1,2))
qqPlot(rdoorcl$V1, dist="norm")
qqPlot(rdoorclt$V1, dist="norm")
par(mfrow=c(1,1))
#Cheking improvement of normality
jarque.bera.test(rdoorcl$V1)
jarque.bera.test(rdoorclt$V1)
#Since we cannot reject the logarithmic transformation, let's check if it works (easier to interpret)
jarque.bera.test(log(rdoorcl$V1))

#To test a particular value for \lambda, we can use the function testTransform acting on a powerTransform object

testTransform(powerTransform(rdoorcl$V1~1), lambda=1)
testTransform(powerTransform(rdoorcl$V1~1), lambda=0.3)

# Repeat the whole analysis with variable rdoorop$V1 to check if the transformation improves normality

# Bivariate Normality for the joint variable (rdoorcl$V1,rdoorop$V1)
# Estimating bivariate parameter (\lambda_1,\lambda_2)
powerTransform(cbind(rdoorcl$V1,rdoorop$V1))
summary(powerTransform(cbind(rdoorcl$V1,rdoorop$V1)~1))

# Transformations to Multinormality 
# Although We can accept the logarithmic transformation for both variables, 
# we are going to transform them with (\lambda_1,\lambda_2) values =c(0.16, 0.15).
# Defining the transformed variable with those lambdas
rdoorT=bcPower(cbind(rdoorcl$V1,rdoorop$V1), c(0.16,0.15))

# Redefining some graphical parameters to combine multiple plots into one overall graph
par(mfrow=c(1,2))

#Comparing fitting to a normal distribution before and after the transformation with Mardia test (package MVN)
library(MVN)
#Before
mvn(cbind(rdoorcl$V1,rdoorop$V1), mvnTest="mardia", multivariatePlot="qq")

# We reject normality given p values equal to 0 for skewness and kurtosis
#After
mardiaTest(rdoorT, mvnTest="mardia", multivariatePlot="qq")
#We have improved normality

## Exploring multivariate Normality for (hbat[,6],hbat[7])
mvn(cbind(hbat[,6],hbat[,7]),mvnTest="mardia", multivariatePlot="qq")
summary(powerTransform(cbind(hbat[,6],hbat[,7])~1))
mvn(cbind(hbat[,6],log(hbat[,7])), mvnTest="mardia", multivariatePlot="qq")

## Outliers

# Univariate identification with boxplots. Package car:
par(mfrow=c(1,1))
#It gives you the observation index classified as outlier
Boxplot(hbat[,7], id.method="y")

# with default built-in functions
boxplot(hbat[,7])
#It gives you the observation classified as an outlier
boxplot(hbat[,7])$out

# Multivariate outliers, "by hand" with Mahalanobis distance, non-robust
# variables hbat[,6:18]
hbat618=hbat[,6:18]
# mean vector, cov matrix and squared mahalanobis distance
meanhbat618=sapply(hbat618, mean)
covhbat618=cov(hbat618)
mahalanobis618=mahalanobis(hbat618,meanhbat618,covhbat618)
mahalanobis618
# 95th percentile of a chi-squared distribution with 13 degrees of freedom (we are using 13 variables)
#Position of outliers
which(mahalanobis618 > qchisq(0.95,df=13))
#We got 6 outliers, their rows in the data set
pos=which(mahalanobis618 > 22.36)
pos
mahalanobis618[pos]

## To plot outliers in a different color
x=rep(1, 100)
x[pos]=0
# We plot them on a scatterplot of variables 6 and 7 (they are outliers for the whole set of variables 6:18).
plot(hbat[,6],hbat[,7],col=x+2,pch=16)
# Visual identification, function qqPlot package car
qqPlot(mahalanobis618,dist="chisq",df=13, line="robust", id.method="identify")
# Multivariate outliers "by hand" ends here


## Package mvoutlier
library(mvoutlier)
##### pcout function, robust method based on principal components
hb618.out=pcout(hbat[,6:18], makeplot=TRUE)
# which potential outliers does it find?
which(hb618.out$wfinal01==0)

# plotting each point with each final combined weight. Small values indicate potential multivariate outliers.
plot(seq(1,100),hb618.out$wfinal)

# Bivariate graphic highlighting outliers in red
plot(hbat[,6],hbat[,7],pch=16,col=hb618.out$wfinal01+2) 

# Interactive chi-squared plot
chisq.plot(hbat[,6:18])

#Four graphs
aq.plot(hbat[,6:18])

# changing the default quantile
which(aq.plot(hbat[,6:18],delta=qchisq(0.95,df=13))$outliers=="TRUE")
which(aq.plot(hbat[,6:18])$outliers=="TRUE")

par(mfrow=c(1,1))
# Symbol plot, bivariate outliers (only for two variables)
symbol.plot(cbind(hbat[,16], hbat[,17]))
#distance plot, comparing Mahalanobis distance with robust Mahalanobis distance. With robust estimates, outliers 
#stick out much more.
dd.plot(cbind(hbat[,16], hbat[,17]))

#correlation plot, for two variables
corr.plot(hbat[,16], hbat[,17])
# Other plot for no more than 10 variables
uni.plot(hbat[,6:12])
## Package mvoutlier ends

## Function spm, package car. Very complete, see parameter "transform=TRUE".
spm(hbat[,6:11],reg.line=lm, diagonal="histogram", smoother=FALSE, spread=FALSE, ellipse=TRUE, transform=TRUE)

# A complement to ggplot to plot scatterplot matrices
library(GGally)
medifis$gender=as.factor(medifis$gender)
ggpairs(medifis, lower = list(discrete="facetbar",continuous="points",combo="facetdensity",mapping=aes(color=gender)))

## Explore the linear structure of the numeric variables in the set cereal. 
#calories, protein, fat, sodium, fiber, carbo, sugars, potass, vitamins
#Explore differences between correlations and partial correlations.

