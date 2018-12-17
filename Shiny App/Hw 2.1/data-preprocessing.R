###################################################
##########       Data preprocessing      ##########
###################################################

#####################
######    1     #####
#####################

diamonds <- read.table(str_c(hw2.1_path, "HW-diamonds.txt"))
names(diamonds) <- c('Weight', 'ColourPurity', 'Clarity', 'Certifier', 'Price')

#####################
######    2     #####
#####################

#####################
######   3.A    #####
#####################

breaks <- c(0, 0.5, 1, 1.1) # intervals
labels <- c("Small", "Medium", "Large") # groups names
Carat_Size <- cut(diamonds$Weight, breaks, labels) # cut income into intervals using breaks and codes each value to the corresponding interval
diamonds3A <- cbind(diamonds, Carat_Size) # add wealth as a column to ds

diamonds3A$Carat_Size = as.factor(diamonds3A$Carat_Size)
diamonds3A$Carat_Size <- relevel(diamonds3A$Carat_Size, ref = "Small")

model3a <- lm(formula = log(Price) ~ Weight + ColourPurity + Clarity + Certifier + Carat_Size + Carat_Size * Weight, data = diamonds3A)

# Testing Colour purity differences
test = read.table(str_c(hw2.1_path, "Test.txt"))
names(test) <- c('Weight', 'ColourPurity', 'Clarity', 'Certifier', 'Carat_Size')

# Testing certifier differences
test2 = read.table(str_c(hw2.1_path, "Test2.txt"))
names(test2) <- c('Weight', 'ColourPurity', 'Clarity', 'Certifier', 'Carat_Size')

#####################
######   3.B    #####
#####################

diamonds3B <- read.table(str_c(hw2.1_path, "HW-diamonds.txt"))
names(diamonds3B) <- c('Weight', 'ColourPurity', 'Clarity', 'Certifier', 'Price')
sqrt_Carat_Size = (diamonds3B$Weight) * 2
diamonds3B <- cbind(diamonds3B, sqrt_Carat_Size) # add wealth as a column to ds

model3b <- lm(formula = log(Price) ~ Weight + ColourPurity + Clarity + Certifier + sqrt_Carat_Size, data = diamonds3B)
