###################################################
##########       Data preprocessing      ##########
###################################################

Elks.table = data.frame(expand.grid(Action = c("Crossing", "Retreat"), Traffic = c("Low", "High"), Vehicle = c("Car", "Truck")), counts = c(287, 57, 237, 52, 40, 42, 57, 12))

Elks.partial <- xtabs(counts ~ Action + Vehicle + Traffic, data = Elks.table)
Elks.partial

Elks.podds <- oddsratio(Elks.partial, log = FALSE)
Elks.podds

# Marginal table of Action by Vehicle, ignoring Traffic
Elks.marginal <- xtabs(counts ~ Action + Vehicle, data = Elks.table)
