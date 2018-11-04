source("~/Documents/EIT Digital Masterschool/UPM/1st semester/Intelligent Data Analysis/Homeworks/1.3/lumping_errors.R")
source("~/Documents/EIT Digital Masterschool/UPM/1st semester/Intelligent Data Analysis/Homeworks/1.3/splitting_errors.R")

cars <- read.table("~/Documents/EIT Digital Masterschool/UPM/1st semester/Intelligent Data Analysis/Homeworks/1.3/cars-PCA.txt", header=TRUE, as.is=TRUE, na.strings="-1")
colnames(cars) <- c('mpg','cylinders','engine_displacement','horsepower','weight','acceleration','model_year','origin','car_name')

library("dplyr")
cars <- cars[-which(cars$cylinders==3),] # remove the unique entry with 3 number of cylinders
cars$origin = as.factor(cars$origin)
cars$cylinders = as.factor(cars$cylinders)
cars$car_name = as.factor(cars$car_name)
cars$model_year = as.factor(cars$model_year)

library(FactoMineR)
library(factoextra)

# As supplementary qualitative individuals, the coordinates of origin, car_name,
# cylinders and model_year variables will be predicted also. They can be used to color individuals by groups.
cars_pca=PCA(cars,quali.sup=c(2,7,8,9),ncp=5,scale.unit=TRUE, graph=FALSE)
summary(cars_pca)

# The eigenvalues measure the amount of variation retained by each principal component. 
# Eigenvalues are large for the first PCs and small for the subsequent PCs.
# We examine the eigenvalues to determine the number of principal components to be considered.
# In our case about 95% of the variation is explained by the first 2 Pcs. 
eig.val <- get_eigenvalue(cars_pca)
eig.val

# scree plot
# An alternative method to determine the number of principal components is to look at a Scree Plot, 
# which is the plot of eigenvalues ordered from largest to the smallest.
# The number of component is determined at the point, beyond which the remaining eigenvalues are all relatively small and of comparable size.
fviz_eig(cars_pca, addlabels = TRUE, ylim = c(0, 85))

###
# The correlation between a variable and a principal component (PC) is used as the coordinates of the variable on the PC. 
# The representation of variables differs from the plot of the observations: The observations are represented by their projections, 
# but the variables are represented by their correlations.
###

## Working on variables, circle of correlations
# This kind of plot shows the relationships between all variables. It can be interpreted as follow:
# - Positively correlated variables are grouped together.
# - Negatively correlated variables are positioned on opposite sides of the plot origin (opposed quadrants).
# - The distance between variables and the origin measures the quality of the variables on the factor map. Variables that are away from the origin are well represented on the factor map.
fviz_pca_var(cars_pca)

# Ploting the 2 variables that contribute the most to the representation 
plot(cars_pca, shadow=TRUE,choix="var", select="contrib 2" )

# Quality of representation
# A high cos2 indicates a good representation of the variable on the principal component. In this case the variable is positioned close to the circumference of the correlation circle.
# A low cos2 indicates that the variable is not perfectly represented by the PCs. In this case the variable is close to the center of the circle
# In our case every variable is very well represented by only 2 PCs.
# The sum of cos2 for the acceleration variable equals 1 which indicates perfect representation by the 2 PCs.
fviz_pca_var(cars_pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

# Contribution of variables
# The contributions of variables in accounting for the variability in a given principal component are expressed in percentage.
# - Variables that are correlated with PC1 and PC2 are the most important in explaining the variability in the data set.
# - Variables that are not correlated with any PC or correlated with the last dimensions are variables with low contribution and might be removed to simplify the overall analysis.

# Contributions of variables to PC1 and PC2
fviz_contrib(cars_pca, choice = "var", axes = 1:2, top = 10)
fviz_pca_var(cars_pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# Quality and contribution plots on individuals
fviz_pca_ind(cars_pca, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# Color individuals by either origin or number of cylinders
# This function draws confidence ellipses around the categories of each supplementary categorical variable. The objective is to investigate 
# whether the categories of the categorical variable are significantly different from each other.
fviz_pca_ind(cars_pca,
             geom.ind = "point", # show points only
             col.ind = cars$origin, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.level = 0.95,
             legend.title = "Number of Cylinders"
)
fviz_pca_ind(cars_pca,
             geom.ind = "point", # show points only
             col.ind = cars$cylinders, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.level = 0.95,
             legend.title = "Number of Cylinders"
)

# Biplot
# The coordinate of individuals and variables are not constructed on the same space. Therefore, in the biplot, 
# we should mainly focus on the direction of variables but not on their absolute positions on the plot.

# A biplot can be interpreted as follow:
# - an individual that is on the same side of a given variable has a high value for this variable.
# - an individual that is on the opposite side of a given variable has a low value for this variable.
fviz_pca_biplot(cars_pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

# change the color of individuals by either origin or number of cylinders
fviz_pca_biplot(cars_pca, 
                col.ind = cars$origin, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Origin") 
fviz_pca_biplot(cars_pca, 
                col.ind = cars$cylinders, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Number of Cylinders") 

## Partitioning clustering
# A K-means clustering with k = 3:

# Note that the stability of the result can be improved by increasing the maximum number 
# of iterations and using multiple random starts:

# cars.scores <- scale(cars_pca$ind$coord, center=FALSE)
cars.scores <- cars_pca$ind$coord
set.seed(12345)
cars.k3 <- kmeans(cars.scores, centers=3, iter.max=100, nstart=25)
cars.k3
pairs(cars.scores, col=cars.k3$cluster)
pairs(cars.scores, col=cars$origin)
pairs(cars.scores, col=cars$cylinders)

# change the color of individuals by their cluster
fviz_pca_ind(cars_pca,
             geom.ind = "point", # show points only
             col.ind = as.factor(cars.k3$cluster), # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.level = 0.95,
             legend.title = "Cluster"
)


# Origin of the cars
table(cars.k3$cluster, cars$origin)
lumping_errors(cars$origin, cars.k3$cluster)
splitting_errors(cars$origin, cars.k3$cluster)

# Here we explore visually how well the clustering recovers the actual origin of the cars
newdf=data.frame(col=as.factor(cars.k3$cluster),shape=cars$origin, 
                 PC1=cars_pca$ind$coord[,1],PC2=cars_pca$ind$coord[,2])

ggplot(data=newdf, aes(x=PC1, y=PC2, colour=col, shape=shape))+
  geom_text(aes(label=rownames(newdf)), hjust=1.5)+geom_jitter()+labs(title = "Clustering Results vs Origin of the Cars", colour = "Origin", shape = "Cluster")


# Number of Cylinders
table(cars.k3$cluster, cars$cylinders)
lumping_errors(cars$cylinders, cars.k3$cluster)
splitting_errors(cars$cylinders, cars.k3$cluster)

# Here we explore visually how well the clustering recovers the number of cylinders of the cars
newdf=data.frame(col=as.factor(cars.k3$cluster),shape=cars$cylinders, 
                 PC1=cars_pca$ind$coord[,1],PC2=cars_pca$ind$coord[,2])

ggplot(data=newdf, aes(x=PC1, y=PC2, colour=col, shape=shape))+
  geom_text(aes(label=rownames(newdf)), hjust=1.5)+geom_jitter()+
  labs(title = "Clustering Results vs Number of Cylinders of the Cars", 
       colour = "Cylinders", 
       shape = "Cluster",
       xlab = "PC1 (82.055%)",
       ylab = "PC2 (12.842%)")

