###################################################
##########          Reactives           ###########
###################################################

# Question 2

cars.pca <- reactive({ PCA(cars, quali.sup = c(2, 7, 8, 9), ncp = 5, scale.unit = TRUE, graph = FALSE) })
cars.scores <- reactive({ cars.pca()$ind$coord })
cars.k3 <- reactive({
    set.seed(12345)
    return(kmeans(cars.scores(), centers = 3, iter.max = 100, nstart = 25))
})