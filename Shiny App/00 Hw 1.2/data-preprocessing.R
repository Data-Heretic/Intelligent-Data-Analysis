###################################################
##########       Data preprocessing      ##########
###################################################

wines = read.table(str_c(hw2_path, "wines.txt"))
colnames(wines) <- c("FixAcid", "VolAcid", "CitAcid", "ResSug", "chlor", "FSo2", "TSo2", "d", "pH", "S", "A", "qual", "type")
