library(FactoMineR) #load FactoMineR and perform a PCA analysis on matrix R.
library(factoextra)
library(ggplot2) 
library(GGally)
library(forcats) # for merging

#Same as hw1.2
wines=read.table("wines.txt")
colnames(wines) <- c("FixAcid", "VolAcid", "CitAcid", "ResSug", "chlor"
                          , "FSo2", "TSo2", "d", "pH", "S", "A", "qual","type")
wns$type=as.factor(wns$type)
wns<-wines
quality<-wns$qual
wns<-wns[,-12]


# Before starting with PCA, a nice scatterplot matrix as shown in pca.r(lab4)
ggpairs(wns, lower = list(continuous="points",combo="facetdensity",mapping=aes(color=type)))

######   A    ####
# DOT 1 : We will use the R matrix (correlation) because variables are on different scales
#You tend to use the covariance matrix (S) when the variable scales are similar and 
#the correlation matrix (R) when variables are on different scales
#scale.unit=TRUE bases the PCA on the correlation matrix

w_pca=PCA(wns,quali.sup=12,scale.unit=TRUE, graph=FALSE)

#DOT 2 AND 3 : This can answer the first two questions 
summary(w_pca)

# (DOT 2: )We can see that the first 4 components reflect 71.7% of the variation of data;
#we can see that components 1 and 2 are the components that 
#mostly reflect the variation of data with a value of 38%,
#so we can use those values to represent the data.
#the DOT 3 is answered on an attached jpeg file.
#We should put something like that on shiny and the pdf.

write.table(w_pca$var$coord, "wines.xlsx", sep="\t")

#DOT 4: PCA 
plot(w_pca, cex=0.8,shadow=TRUE, habillage=12)
# Ploting the 5 variables that contribute the most to the representation 
plot(w_pca, shadow=TRUE,choix="var", select="contrib 5" )
# selecting variables by their contributions, quality of representation greater than 0.7
plot(w_pca, shadow=TRUE,choix="var", select="cos2 0.7" )
# control variable colors using their contribution
fviz_pca_var(w_pca, col.var="contrib",fill.ind=5, fill.var=5,alpha.ind = 2)+
  scale_color_gradient2(low="green",mid = "green" ,
                        high="red")+theme_light()


#DOT 4 :PCA according to type value
fviz_pca_ind(w_pca,  label="none", habillage=12)
#We can asssume that Dim2 separates wines according to type more that Dim1


# DOT 5 : Change the variable quality to low,medium,high
quality<-fct_collapse(as.factor(quality), low = c("4","5"), medium = c("6"), high = c("7","8"))
#plotting the PCA according to quality type after doing the required transformation
fviz_pca_ind(w_pca,  label="none", habillage=quality,cex=0.8,addEllipses=TRUE)

wines$qual<-fct_collapse(as.factor(wines$qual), low = c("4","5"), medium = c("6"), high = c("7","8"))
wines_pca=PCA(wines,quali.sup=12,scale.unit=TRUE, graph=FALSE)

fviz_pca_ind(wines_pca,  label="none", habillage=quality,cex=0.8,addEllipses=TRUE)

# DOT 6 FINAL DOT :  Plot with circle of correlations
fviz_pca_var(w_pca)

