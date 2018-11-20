library("vcd")
library(grid)

##Creating the contingency table
ElkCrossing.table=data.frame(expand.grid(Vehicle=c("Car", "Truck"), Action=c("Crossing", "Retreat"), 
                                   Traffic=c("High", "Low")), count=c(287,40,57,42,237,57,52,12))
# I want to order the levels, first "white" and first "yes"
ElkCrossing.table$Vehicle=ordered(ElkCrossing.table$Vehicle, levels=c("Car", "Truck"))
ElkCrossing.table$Action=ordered(ElkCrossing.table$Action, levels=c("Crossing", "Retreat"))
ElkCrossing.table$Traffic=ordered(ElkCrossing.table$Traffic, levels=c("High", "Low"))

## Marginal table 
ElkCrossing.table.marginal=xtabs(count~Vehicle+Action, ElkCrossing.table)
ElkCrossing.table.marginal

#calculating oddsratio for vehicle and action , not considering the traffic
oddsr_marginal=oddsratio(ElkCrossing.table.marginal, log=FALSE )
oddsr_marginal
mosaic(ElkCrossing.table.marginal)


## Partial table 
ElkCrossing.table.partial=xtabs(count~Vehicle+Action+Traffic, ElkCrossing.table)
dim(ElkCrossing.table.partial)
ElkCrossing.table.partial


#Calculating the oddsratio for Vehicle and action , given high traffice
oddsratio(ElkCrossing.table.partial[,,1], log=FALSE)


#Calculating the oddsratio for Vehicle and action , given low traffice
oddsratio(ElkCrossing.table.partial[,,2], log=FALSE)

#Calculating the odds ration for every traffic level
oddsr=oddsratio(ElkCrossing.table.partial, log=FALSE )
oddsr
oddsrintervals=confint(oddsr, log=FALSE)
cbind(odds=exp(oddsr$coefficients),oddsrintervals)

#To calcuate the estimated odds ration taking into account the third variable
mantelhaen.test(ElkCrossing.table.partial)

fourfold(ElkCrossing.table.partial)
mosaic(ElkCrossing.table.partial )

#To calcuate the estimated odds ration WITHOUT taking into account the third variable
mantelhaen.test(ElkCrossing.table.partial)



# test the hypothesis of homogeneous association  for elks crossing roads
woolf_test(ElkCrossing.table.partial)

