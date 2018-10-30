###################################################
##########       Data preprocessing      ##########
###################################################

# Wines
wns = read.table(str_c(hw3_path, "wines.txt"))
colnames(wns) <- c("FixAcid", "VolAcid", "CitAcid", "ResSug", "chlor", "FSo2", "TSo2", "d", "pH", "S", "A", "qual", "type")

# Cars
cars = read.table(str_c(hw3_path, "cars.txt"))
colnames(cars) <- c("mpg", "cyl", "displ", "hpower", "weight", "acc", "model", "origin", "name")


quality <- wns$qual

wns <- wns[, -12]
wns$type = as.factor(wns$type)

######   A    ####
#We will use the R matrix (correlation) because variables are on different scales
#You tend to use the covariance matrix (S) when the variable scales are similar and 
#the correlation matrix (R) when variables are on different scales
#scale.unit=TRUE bases the PCA on the correlation matrix

w_pca = PCA(wns, quali.sup = 12, scale.unit = TRUE, graph = FALSE)
