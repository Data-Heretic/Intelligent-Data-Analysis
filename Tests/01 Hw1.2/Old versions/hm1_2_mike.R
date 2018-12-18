library(moments) # Skewness and kurtosis
library(car) # Pooled groups
library(tseries)#jarque bera
library(MVN)#Comparing fitting to a normal distribution before and after the transformation with Mardia test 

Wholetable=read.table("wines.txt")
colnames(Wholetable) <- c("FixAcid", "VolAcid", "CitAcid", "ResSug", "chlor"
                    , "FSo2", "TSo2", "d", "pH", "S", "A", "qual","type")


############################## 
#           1A)              #
##############################

summary(Wholetable$TSo2)
hist(Wholetable$TSo2)

range(Wholetable$TSo2, na.rm=TRUE)
#Sample Variance
var(Wholetable$TSo2, na.rm=TRUE)
#Standard Deviation
sd(Wholetable$TSo2, na.rm=TRUE)
#Coefficient of variation, as percent
sd(Wholetable$TSo2, na.rm=TRUE)/
  mean(Wholetable$TSo2, na.rm=TRUE)*100
#Quantile
quantile(Wholetable$TSo2, na.rm=TRUE)

skewness(Wholetable$TSo2)
kurtosis(Wholetable$TSo2)

anscombe.test(Wholetable$TSo2) #kurtosis

agostino.test(Wholetable$TSo2) #skewness

d<-density(Wholetable$TSo2)
hist(Wholetable$TSo2,breaks = 50,probability = T,col="gray", border="white",xlim = c(0,250),main="Histogram of Total Sulflur Dioxide",xlab="Total Sulflur dioxide")
lines(d,col="red",lwd=2)
abline(v=mean(Wholetable$TSo2),col="red",lwd=2)

legend("topright", legend=c("skewness = 0.21", "kurtosis = 2.07"),box.lty=0,cex=0.8)

jarque.bera.test(Wholetable$TSo2) #p-value > 0.05


#The skewness here is 0.2167518. 
#This value implies that the distribution of the data is slightly skewed to the right or positive skewed.
#It is skewed to the right because the computed value is positive, and is slightly,
#because the value is close to zero. 
#For the kurtosis, we have 2.073274 implying that the distribution of the data is platykurtic, 
#since the computed value is less than 3.

# Try logarithmic transformation

l<-log(Wholetable$TSo2)
jarque.bera.test(l)

skewness(l)
kurtosis(l)

anscombe.test(l) 

agostino.test(l)

d<-density(l)
hist(l,breaks = 50,probability = T,col="gray", border="white",main="Histogram of Logarithm of Total Sulflur dioxide",xlab="Logarithm of Total Sulflur dioxide")
lines(d,col="red",lwd=2)
abline(v=mean(l),col="red",lwd=2)
#The skewness here is -0.8050182. 
#This value implies that the distribution of the data is skewed to the left or negative skewed.
#It is skewed to the left because the computed value is negative. 
#For the kurtosis, we have 2.675768 implying that the distribution of the data is platykurtic, 
#since the computed value is less than 3.


#We make a variable Power transformation using lambda=0.27
TSt=bcPower(Wholetable$TSo2, lambda=0.27)
jarque.bera.test(TSt)

skewness(TSt)
kurtosis(TSt)

anscombe.test(TSt) 

agostino.test(TSt)

d<-density(TSt)
hist(TSt,breaks = 50,probability = T,col="gray", border="white",main="Histogram of Power transformation of Total Sulflur dioxide",xlab="PT of Total Sulflur dioxide")
lines(d,col="red",lwd=2)
abline(v=mean(TSt),col="red",lwd=2)

#Outliers

Boxplot(Wholetable$TSo2, id.method="y")#No outliers

#### --- Summary --- ####

# We researched the Total Sulflur Dioxide in wines in temrs of of descriptive measures of center,
#dispersion, skewness and kurtosis but after a jarque bera test we found out that no normal model 
#is plausible for its distribution.We used logarithmic and power transformation to normalize the data and researched again.
#No outliers founded


############################## 
#           1B)              #
##############################

mvn(cbind(Wholetable$CitAcid, Wholetable$A), mvnTest="mardia", multivariatePlot="qq")

jarque.bera.test(Wholetable$CitAcid)
jarque.bera.test(Wholetable$A)
#Doing shapiro test to check normality of the 2 variables
shapiro.test(cbind(Wholetable$CitAcid, Wholetable$A))

# Joint distribution per group
plot(Wholetable$CitAcid, Wholetable$A, pch=c(4, 16), lwd=2,
     col=c("black", "blue"),
     main="Joint distribution per group",xlab = "Citric Acid",ylab="Alcohol")
# Joint distribution of two variables
dataEllipse(Wholetable$A,Wholetable$CitAcid,  xlab="x", ylab="y", asp=1, levels=0.5, lwd=2, center.pch=16,
            col="blue", main="Joint distribution of two variables")
legend(x="bottomright", legend=c("Data", "centroid", "distribution ellipse"),
       pch=c(1, 16, NA), lty=c(NA, NA, 1), col=c("black", "blue", "blue"),cex=0.6)

#Outliers
Boxplot(Wholetable$CitAcid, id.method="y")

Boxplot(Wholetable$A, id.method="y") #Here we find outliers



############################## 
#           1C)              #
##############################
#Create a new table with alcohol percentage, residual sugar, chloride, sulphates
#and quality
#C1
dataforC=Wholetable[,c("A","ResSug","chlor","S","qual")]


Rmatr=cor(dataforC) #R matrix
#C2
library(ppcor)
partialcorrelationmatrix=pcor(dataforC)
#C3
r2multv<-function(x){
  r2s=1-1/(diag(solve(cov(x)))*diag(cov(x)))
  r2s
}
Coefdeterm=r2multv(dataforC)

#C4
detR=det(Rmatr)
