load("~/Documents/EIT Digital Masterschool/UPM/1st semester/Intelligent Data Analysis/Homeworks/1.2/RestaurantTips.rda")

RestaurantTips <- RestaurantTips[-which(RestaurantTips$PctTip>30),]

plot(RestaurantTips$Bill~RestaurantTips$PctTip)

# Correlation coefficients and tests
library(Hmisc)
rcorr(as.matrix(RestaurantTips[,c(1,7)]), type="pearson")

library(MXM)
permcor(RestaurantTips$Bill,RestaurantTips$PctTip, R = 10000)

# A test on a correlation coefficient.
r.obt <- rcorr(as.matrix(RestaurantTips[,c(1,7)]), type="pearson")$r[1,2]
cat("The obtained correlation is ",r.obt,'\n')
nreps <- 10000
r.random <- replicate(nreps, {Y <- RestaurantTips$PctTip; X <- sample(RestaurantTips$Bill, length(RestaurantTips$Bill), replace = FALSE); cor(X,Y)})
prob <- length(r.random[r.random >= r.obt])/nreps
cat("Probability randomized r >= r.obt",prob)
hist(r.random, breaks = 50, main =  expression(paste("Distribution around p = 0")), xlab = "r from randomized samples")


