###################################################
##########          Reactives           ###########
###################################################

# Shared


# Question 1a

wines_TS02 <- reactive({ bcPower(wines$TSo2, lambda = 0.5788) })

# Question 1b

wines24 <- reactive({ wines[, c(2, 4)] })
# mean vector, cov matrix and squared mahalanobis distance
mahalanobis24 <- reactive({ mahalanobis(wines24(), sapply(wines24(), mean), cov(wines24())) })

# Question 1c

# Question 2
