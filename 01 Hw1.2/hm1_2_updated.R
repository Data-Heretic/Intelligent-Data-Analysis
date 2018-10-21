library(moments) # Skewness and kurtosis
library(car) # Pooled groups
library(tseries)#jarque bera
library(MVN)#Comparing fitting to a normal distribution before and after the transformation with Mardia test 
library(mvoutlier)

wines=read.table("wines-PCA.txt")
colnames(wines) <- c("FixAcid", "VolAcid", "CitAcid", "ResSug", "chlor"
                    , "FSo2", "TSo2", "d", "pH", "S", "A", "qual","type")

############################## 
#           1A)              #
##############################

summary(wines$TSo2)
hist(wines$TSo2)

range(wines$TSo2, na.rm=TRUE)
#Sample Variance
var(wines$TSo2, na.rm=TRUE)
#Standard Deviation
sd(wines$TSo2, na.rm=TRUE)
#Coefficient of variation, as percent
sd(wines$TSo2, na.rm=TRUE)/
  mean(wines$TSo2, na.rm=TRUE)*100
#Quantile
quantile(wines$TSo2, na.rm=TRUE)

anscombe.test(wines$TSo2) #kurtosis
agostino.test(wines$TSo2) #skewness

d<-density(wines$TSo2)
hist(wines$TSo2,breaks = 10,probability = T,col="blue", border="white",xlim = c(0,250),main="Histogram of Total Sulflur Dioxide",xlab="Total Sulflur dioxide")
lines(d,col="red",lwd=2)
abline(v=mean(wines$TSo2),col="red",lwd=2)
legend("topright", legend=c("skewness = 0.21", "kurtosis = 2.07"),box.lty=0,cex=0.8)

jarque.bera.test(wines$TSo2) # p-value > 0.05

#The skewness here is 0.2167518. 
#This value implies that the distribution of the data is slightly skewed to the right or positive skewed.
#It is skewed to the right because the computed value is positive, and is slightly,
#because the value is close to zero. 
#For the kurtosis, we have 2.073274 implying that the distribution of the data is platykurtic, 
#since the computed value is less than 3.

# Power Transformations, Box-Cox transformation to improve normality

summary(powerTransform(wines$TSo2))

# The estimated parameter for the transformation is lamda=0.5788 together with its confidence interval (0.2971,0.8658). 
# Then, it follows two tests on two specific values for lamda, lamda=0 which stands for the logarithm transformation 
# and lamda=1 which means that you don’t need a transformation to actually improve the normality of your data. 
# In this particular output, we observe a p-value very small for both lamda=0 and lamda=1 so we reject the hypothesis that lamda=0 or lamda=1 is a good transformation value. 
# We can define now a transformed variable, using lamda=0.5788 and see if the normality has been improved.

TSt=bcPower(wines$TSo2, lambda=0.5788)
jarque.bera.test(TSt)

anscombe.test(TSt) #kurtosis

agostino.test(TSt) #skewness

d<-density(TSt)
hist(TSt,breaks = 10,probability = T,col="blue", border="white",main="Histogram of Power transformation of Total Sulflur dioxide",xlab="PT of Total Sulflur dioxide")
lines(d,col="red",lwd=2)
abline(v=mean(TSt),col="red",lwd=2)
legend("topright", legend=c("skewness = -0.17", "kurtosis = 1.94"),box.lty=0,cex=0.8)

#Outliers

#It gives you the observation index classified as outlier
Boxplot(wines$TSo2, id.method="y") #No outliers in our case

# with default built-in functions
boxplot(wines$TSo2)
#It gives you the observation classified as an outlier
boxplot(wines$TSo2)$out #No outliers

#### --- Summary --- ####

# We researched the Total Sulflur Dioxide in wines in terms of of descriptive measures of center,
#dispersion, skewness and kurtosis but after a jarque bera test we found out that no normal model 
#is plausible for its distribution.We used power transformation to normalize the data and research again.
#No outliers founded


############################## 
#           1B)              #
##############################

jarque.bera.test(wines$VolAcid)
jarque.bera.test(wines$ResSug)
#Doing shapiro test to check normality of the 2 variables
shapiro.test(cbind(wines$VolAcid,wines$ResSug))

# Joint distribution per group
plot(wines$VolAcid, wines$ResSug, pch=c(4, 16), lwd=2,
     col=c("black", "blue"),
     main="Joint distribution per group",xlab = "Volatile Acidity",ylab="Residual Sugar")
# Joint distribution of two variables
dataEllipse(wines$VolAcid,wines$ResSug,  xlab="x", ylab="y", asp=1, levels=0.5, lwd=2, center.pch=16,
            col="blue", main="Joint distribution of two variables")
legend(x="bottomright", legend=c("Data", "centroid", "distribution ellipse"),
       pch=c(1, 16, NA), lty=c(NA, NA, 1), col=c("black", "blue", "blue"),cex=0.6)

#Bivariate Normality Before the transformation
mvn(cbind(wines$VolAcid, wines$ResSug), mvnTest = "mardia", multivariatePlot = "qq") # We reject normality given p values equal to 0 for skewness and kurtosis

# Power transformation for bivariate data
# Bivariate Normality for the joint variable (wines$CitAcid,wines$A)
# Estimating bivariate parameter (\lambda_1,\lambda_2)
summary(powerTransform(cbind(wines$VolAcid, wines$ResSug) ~ 1))

# Now we are going to transform them with (\lambda_1,\lambda_2) values =c(0.0584, -0.7548).
# Defining the transformed variable with those lambdas
winesT = bcPower(cbind(wines$VolAcid, wines$ResSug), c(0.0584, -0.7548))

#Bivariate Normality After the transformation
mvn(winesT, mvnTest = "mardia", multivariatePlot = "qq") #We have not improved normality


### Outliers

# Multivariate outliers, "by hand" with Mahalanobis distance, non-robust
# variables wines$VolAcid, wines$ResSug
wines24=wines[,c(2,4)]
# mean vector, cov matrix and squared mahalanobis distance
meanwines24=sapply(wines24, mean)
covwines24=cov(wines24)
mahalanobis24=mahalanobis(wines24,meanwines24,covwines24)
mahalanobis24
# 95th percentile of a chi-squared distribution with 2 degrees of freedom (we are using 2 variables)
#Position of outliers
which(mahalanobis24 > qchisq(0.95,df=2))
#We got 8 outliers, their rows in the data set
pos=which(mahalanobis24 > qchisq(0.95,df=2))
pos
mahalanobis24[pos]

## To plot outliers in a different color
x=rep(1, dim(wines)[1])
x[pos]=0
# We plot them on a scatterplot
plot(wines[,2],wines[,4],col=x+2,pch=16, xlab = "Volatile Acidity", ylab = "Residual Sugar")
# Visual identification, function qqPlot package car
qqPlot(mahalanobis24,dist="chisq",df=2, line="robust")
# Multivariate outliers "by hand" ends here


## Package mvoutlier
##### pcout function, robust method based on principal components
wines24.out=pcout(wines[,c(2,4)], makeplot=TRUE)
# which potential outliers does it find?
which(wines24.out$wfinal01==0)

# plotting each point with each final combined weight. Small values indicate potential multivariate outliers.
plot(seq(1,dim(wines)[1]),wines24.out$wfinal, xlab = "Volatile Acidity", ylab = "Residual Sugar")

# Bivariate graphic highlighting outliers in red
plot(wines[,2],wines[,4],pch=16,col=wines24.out$wfinal01+2, xlab = "Volatile Acidity", ylab = "Residual Sugar") 

# Interactive chi-squared plot
chisq.plot(wines[,c(2,4)])

#Four graphs
aq.plot(wines[,c(2,4)])

# changing the default quantile
which(aq.plot(wines[,c(2,4)],delta=qchisq(0.95,df=2))$outliers=="TRUE")
which(aq.plot(wines[,c(2,4)])$outliers=="TRUE")

par(mfrow=c(1,1))
# Symbol plot, bivariate outliers
symbol.plot(cbind(wines[,2], wines[,4]))
#distance plot, comparing Mahalanobis distance with robust Mahalanobis distance. With robust estimates, outliers 
#stick out much more.
dd.plot(cbind(wines[,2], wines[,4]))

#correlation plot, for two variables
corr.plot(wines[,2], wines[,4])
# Other plot for no more than 10 variables
uni.plot(wines[,c(2,4)])
## Package mvoutlier ends



############################## 
#           1C)              #
##############################
#C1 R matrix of pairwise correlations
dataforC=wines[,c("A","ResSug","chlor","S","qual")]


Rmatr=cor(dataforC) #R matrix with Pearson
#Moderate relation between Chloride and Sulfates
#Moderate relation between Alcohol % and quality
#Neglible relation between Alcohol % and Residual sugar

library(corrplot)
corrplot(Rmatr)
corrplot.mixed(Rmatr)
# reordering the correlation matrix: there are different methods. Sometimes it is 
# useful for minning the hidden structure and pattern in the matrix.
corrplot.mixed(Rmatr,order="AOE" ) # Angular Order of the Eigenvectors

library(corrgram)
corrgram(dataforC)

corrgram(dataforC, order=TRUE,
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

corrgram(dataforC, order=TRUE,
         lower.panel=panel.shade, upper.panel=panel.pts,
         diag.panel=panel.minmax, text.panel=panel.txt)

#Theres correlation between chlorides and S and
#C2 Matrix of partial correlations
library(ppcor)
partialmatrix=pcor(dataforC)$estimate
corrgram(partialmatrix)
corrgram(partialmatrix,
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

corrplot.mixed(partialmatrix,order="AOE" )

#C3 Coefficient of determination (function r2multv() we define in R)
r2multv<-function(x){
  r2s=1-1/(diag(solve(cov(x)))*diag(cov(x)))
  r2s
}

# What is the variable "more linearly explained" by the others?
Coefdeterm=r2multv(dataforC)
Coefdeterm
# Chlorides (chlor) is the best linearly explained by the others (R^2 = 0.358), followed by Sulphates (S, R^2 = 0.326)
# The worst linearly explained by the others is Sugar Residual (ResSug, R^2 = 0.15).

# Are any linear relationships present in this dataset? Let's calculate the determinant of S and R:
det(cor(dataforC))
det(cov(dataforC))

# Find the variables involved in the overall linear dependence
eigen(cov(dataforC))
# Assuming all values are close to 0 except the third 0.989, we can conclude that this variable has very small variance,
# it is almost constant through the observed data.

# Effective dependence coefficient
1-det(cor(dataforC))^{1/4} # Altogether, linear dependences explain 17.5% of the variability in the dataset.
