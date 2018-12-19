# The goal of the experiment is to determine how the odds of crossing (Y) change from car to truck (X), adjusting for low vs. high traffic (Z).
# a) Obtain the estimated odds ratio and confidence intervals of crossing for car vs. truck at each traffic location. 
# Interpret them and use the fourfold display to help you understand the output. Use also the mosaic function to interpret partial tables.
# b) Obtain the estimated odds ratio of crossing vs. retreat without taking into account the third (control) variable. 
# Would it be correct to exclude the effect of that third variable?
# c) Test the homogeneous association between X and Y controlling for Z (function woolf_test).
# d) Are X and Y conditional independent given Z?

library("vcd")
library("vcdExtra")

setwd("C:/Users/Panagiotis/Documents/EIT Digital Masterschool/UPM/1st semester/Intelligent Data Analysis/1. Descriptive Statistics and Statistical Modelling/4. Regression models")

Elks.table = data.frame(expand.grid(Action=c("Crossing", "Retreat"), Traffic=c("Low", "High"), Vehicle=c("Car", "Truck")), counts=c(287,57,237,52,40,42,57,12))

Elks.partial = xtabs(counts ~ Action + Vehicle + Traffic, data = Elks.table)
Elks.partial

Elks.podds=oddsratio(Elks.partial, log=FALSE)
Elks.podds

### Table of Action by Vehicle given Traffic, Traffic = Low
#odds ratio >1
oddsratio(Elks.partial[,,1], log=FALSE)

### Table of Action by Vehicle given Traffic, Traffic = High
#odds ratio <1
oddsratio(Elks.partial[,,2], log=FALSE)

# Question 2

# Marginal table of Action by Vehicle, ignoring Traffic
Elks.marginal=xtabs(counts~Action+Vehicle, data=Elks.table)
Elks.marginal

# Let's estimate the odds ratio of crossing vs. retreat without taking into account the control variable.
# The odds of “success” (crossing) for car vehicles are higher than for trucks.
oddsratio(Elks.marginal, log=FALSE)
# Taking into account (controlling for) traffic, the odds of crossing are lower for car vehicles than for trucks in case of high traffic. 
# Just the reverse direction that the marginal table showed.
# Simpson’s Paradox: The result that a marginal association can have a different direction from each conditional association.
# Moral: It can be dangerous to “collapse” contingency tables over a third control variable.

# The odds ratio is greater than 1 for low traffic which means that the odds of crossing for cars are higher than the odds for trucks.
# But in case of high traffic the odds ratio is less than 1 which means that the odds of crossing for cars are lower than the odds for trucks. 
confint(Elks.podds, log=FALSE)
plot(Elks.podds)

# In the following fourfold display, we can see a graphical representation for the odds ratio of each traffic level.
# It includes confidence rings. If they don't overlap, that indicates an association between crossing and cars, every odds ration is significantly different from 0. 
# In the display we can see a strong positive association for low traffic (5.286842). 
# The rings don’t overlap and the non-principal diagonal sectors have less area than the principal diagonal ones (odds ratio greater than 1).
# On the other hand the odds of crossing is essentially identical for cars and trucks in case of high traffic (odds ration almost 1). 
# This result is an example of Simpson's paradox.
fourfold(Elks.partial)

# In the Elks marginal table, where the odds ratio was 2.676251, we observe a strong positive association (odds ratio greater than 1).
fourfold(Elks.marginal)

# From the mosaic plot below We see that there is no systematic association between different actions and the type of vehicle - except among the action of trucks under low traffic.
# The tiles show that there are relatively less trucks in low traffic with crossing that the hypothesis of independence would predict.
mosaic(Elks.partial, shade = T, split_vertical=TRUE)

# Question 3

# Let’s test the hypothesis of homogeneous association (or homogeneity of odds ratio).
woolf_test(Elks.partial)
# The very low p-value (0.0001) indicates that we can reject the Homogeneity of the Odds Ratio through the two levels of traffic.
# We cannot assume that the conditional relationship between any pair of variables given the third one is the same at each level of the third variable.

# Question 4

# Let's test the hypothesis of conditional independence
mantelhaen.test(Elks.partial)
# The very low p-value (7.868e-07) indicates that we can reject the conditional independence of the Odds Ratio through the two levels of traffic.
