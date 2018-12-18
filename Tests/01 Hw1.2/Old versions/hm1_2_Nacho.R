Wholetable=read.table("D:/Universidad/Intelligent Data Analisis/Homework 1.2/wines-PCA.txt")
colnames(Wholetable) <- c("FixAcid", "VolAcid", "CitAcid", "ResSug", "chlor"
                    , "FSo2", "TSo2", "d", "pH", "S", "A", "qual","type")

#Create a new table with alcohol percentage, residual sugar, chloride, sulphates
#and quality
#C1 R matrix of pairwise correlations
dataforC=Wholetable[,c("A","ResSug","chlor","S","qual")]


Rmatr=cor(dataforC) #R matrix with Pearson
#Moderate relation between Chloride and Sulfates
#Moderate relation between Alcohol % and quality
#Neglible relation between Alcohol % and Residual sugar
library(corrgram)
corrgram(dataforC)

#Theres correlation between chlorides and S and
#C2 Matrix of partial correlations
library(ppcor)
partialmatrix=pcor(dataforC)$estimate
corrgram(partialmatrix)
#C3 Coefficient of determination (function r2multv() we define in R)
r2multv<-function(x){
  r2s=1-1/(diag(solve(cov(x)))*diag(cov(x)))
  r2s
}
Coefdeterm=r2multv(dataforC)

#C4 The determinant of R (correlation matrix) as an overall measure of linear relationships.
detR=det(Rmatr)


#C5 An eigenanalysis of matrix R, looking for really small eigenvalues.
eigenvalues=eigen(cov(dataforC))

eigenvalues[["values"]][5] #value aproximately to 0
eigenvalues[["vectors"]][,5]#this is its eigenvector, assuming 

