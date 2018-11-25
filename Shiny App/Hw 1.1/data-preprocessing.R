###################################################
##########       Data preprocessing      ##########
###################################################

mpg <- get(load(str_c(hw1.1_path, "mpg.Rdata")))

mpg$tr <- substr(mpg$trans, 0, 1)
mpg$tr <- str_replace(mpg$tr, "^m", "Manual")
mpg$tr <- str_replace(mpg$tr, "^a", "Automatic")

mpg <- mpg[mpg$fl != "c",]

mpg$year = as.factor(mpg$year)
mpg$drv = as.factor(mpg$drv)
mpg$cyl = as.factor(mpg$cyl)
mpg$fl = as.factor(mpg$fl)
mpg$class = as.factor(mpg$class)