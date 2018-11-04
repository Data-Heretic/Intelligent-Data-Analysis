###################################################
##########       Data preprocessing      ##########
###################################################

# Wines

# We use "wines" as the complete dataset and "wns" the one without the quality variable.
wns = read.table(str_c(hw3_path, "wines.txt"))
colnames(wns) <- c("FixAcid", "VolAcid", "CitAcid", "ResSug", "chlor", "FSo2", "TSo2", "d", "pH", "S", "A", "qual", "type")

wns$type=as.factor(wns$type)
wns<-wines
quality<-wns$qual
quality<-fct_collapse(as.factor(quality), low = c("4","5"), medium = c("6"), high = c("7","8"))
wns<-wns[,-12]

#We will use the R matrix (correlation) because variables are on different scales
#You tend to use the covariance matrix (S) when the variable scales are similar and 
#the correlation matrix (R) when variables are on different scales
#scale.unit=TRUE bases the PCA on the correlation matrix

w_pca = PCA(wns, quali.sup = 12, scale.unit = TRUE, graph = FALSE)
wines$qual <- fct_collapse(as.factor(wines$qual), low = c("4", "5"), medium = c("6"), high = c("7", "8"))
wines_pca = PCA(wines, quali.sup = 12, scale.unit = TRUE, graph = FALSE)

# Cars

cars <- read.table(str_c(hw3_path, "cars.txt"), header = TRUE, as.is = TRUE, na.strings = "-1")
colnames(cars) <- c('mpg', 'cylinders', 'engine_displacement', 'horsepower', 'weight', 'acceleration', 'model_year', 'origin', 'car_name')

cars <- cars[-which(cars$cylinders == 3),] # remove the unique entry with 3 number of cylinders
cars$origin = as.factor(cars$origin)
cars$cylinders = as.factor(cars$cylinders)
cars$car_name = as.factor(cars$car_name)
cars$model_year = as.factor(cars$model_year)