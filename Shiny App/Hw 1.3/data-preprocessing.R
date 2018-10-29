###################################################
##########       Data preprocessing      ##########
###################################################

# Wines
wines = read.table(str_c(hw3_path, "wines.txt"))
colnames(wines) <- c("FixAcid", "VolAcid", "CitAcid", "ResSug", "chlor", "FSo2", "TSo2", "d", "pH", "S", "A", "qual", "type")

# Cars
cars = read.table(str_c(hw3_path, "cars.txt"))
colnames(cars) <- c("mpg", "cyl", "displ", "hpower", "weight", "acc", "model", "origin", "name")
