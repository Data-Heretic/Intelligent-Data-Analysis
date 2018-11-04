###################################################
##########         Question 2        ##############
###################################################

# UI

hw3_q2_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Q2"),
            h2(HTML("<b> Cars analysis </b>")),
            h4("Perform a Principal Component Analysis on the cars data set (on the set of 5 quantitative variables)."),
            h4("We will use the R matrix (correlation) because variables are on different scales
            You tend to use the covariance matrix (S) when the variable scales are similar and 
            the correlation matrix (R) when variables are on different scales
            scale.unit=TRUE bases the PCA on the correlation matrix"),
            fluidPage(
                fluidRow(
                    h3("Interpreting the results"),
                    box(h5("We will use the R matrix (correlation) because variables are on different scales
                        You tend to use the covariance matrix (S) when the variable scales are similar and 
                        the correlation matrix (R) when variables are on different scales
                        scale.unit=TRUE bases the PCA on the correlation matrix"), width = 12),
                    box(h5("As supplementary qualitative individuals, the coordinates of origin, car_name, cylinders and model_year variables will be predicted also. They can be used to color individuals by groups."),
                        verbatimTextOutput(ns("summary.pca.cars"))
                    ),
                    box(h5("The eigenvalues measure the amount of variation retained by each principal component. Eigenvalues are large for the first PCs and small for the subsequent PCs. We examine the eigenvalues to determine the number of principal components to be considered. In our case about 95% of the variation is explained by the first 2 Pcs. "),
                        verbatimTextOutput(ns("summary.eig.cars")),
                        h5("An alternative method to determine the number of principal components is to look at a Scree Plot, which is the plot of eigenvalues ordered from largest to the smallest. The number of   component is determined at the point, beyond which the remaining eigenvalues are all relatively small and of comparable size."),
                        plotOutput(ns("plot.scree.cars"))
                    ),
                    box(h5("The correlation between a variable and a principal component (PC) is used as the coordinates of the variable on the PC. The representation of variables differs from the plot of the observations: The observations are represented by their projections, but the variables are represented by their correlations."), width = 12)
                )
            ),
            fluidPage(
                fluidRow(
                    h3("Circle of correlations"),
                    box(h5("This kind of plot shows the relationships between all variables. It can be interpreted as follow:"),
                        div(HTML("<ul><li>Positively correlated variables are grouped together.</li>
                                <li>Negatively correlated variables are positioned on opposite sides of the plot origin (opposed quadrants).</li>
                                <li>The distance between variables and the origin measures the quality of the variables on the factor map. Variables that are away from the origin are well represented on the factor map.</li></ul>")),
                        plotOutput(ns("plot.circle.cars.pca_1")),
                        width = 4
                    ),
                    box(h5("Ploting the 2 variables that contribute the most to the representation"),
                        plotOutput(ns("plot.circle.cars.pca_2")),
                        width = 4
                    ),
                    box(h4("Quality of representation"),
                        h5("A high cos2 indicates a good representation of the variable on the principal component. In this case the variable is positioned close to the circumference of the correlation circle. A low cos2 indicates that the variable is not perfectly represented by the PCs. In this case the variable is close to the center of the circle. In our case every variable is very well represented by only 2 PCs. The sum of cos2 for the acceleration variable equals 1 which indicates perfect representation by the 2 PCs."),
                        plotOutput(ns("plot.circle.cars.pca_3")),
                        width = 4
                    )
                ),
                fluidRow(
                    box(h4("Contribution of variables"),
                        div(HTML("The contributions of variables in accounting for the variability in a given principal component are expressed in percentage.</br>
                            <ul><li>Variables that are correlated with PC1 and PC2 are the most important in explaining the variability in the data set.</li>
                            <li>Variables that are not correlated with any PC or correlated with the last dimensions are variables with low contribution and might be removed to simplify the overall analysis.</li></ul>")),
                        width = 12),
                    box(h5("Contributions of variables to PC1 and PC2"),
                        plotOutput(ns("plot.bars.contrib_cars.pca")),
                        plotOutput(ns("plot.circle.contrib_cars.pca")),
                        width = 4
                    ),
                    box(h5("Quality and contribution plots on individuals"),
                        plotOutput(ns("plot.points.cars.pca")),
                        width = 4
                    ),
                    box(h5("Color individuals by either origin or number of cylinders. This function draws confidence ellipses around the categories of each supplementary categorical variable. The objective is to investigate whether the categories of the categorical variable are significantly different from each other."),
                        plotOutput(ns("plot.points.ellipsis.cars.pca.origin")),
                        plotOutput(ns("plot.points.ellipsis.cars.pca.cylinders")),
                        width = 4
                    )
                ),
                fluidRow(
                    box(h4("Biplot"),
                        h5("The coordinate of individuals and variables are not constructed on the same space. Therefore, in the biplot, we should mainly focus on the direction of variables but not on their absolute positions on the plot."),
                        div(HTML("A biplot can be interpreted as follow:</br>
                            <ul><li>an individual that is on the same side of a given variable has a high value for this variable.</li>
                            <li>an individual that is on the opposite side of a given variable has a low value for this variable.</li></ul>")),
                        width = 12),
                    box(plotOutput(ns("plot.biplot.cars.pca")), width = 4),
                    box(h5("Change the color of individuals by either origin or number of cylinders"), width = 8),
                    box(plotOutput(ns("plot.biplot.cars.pca.origin")), width = 4),
                    box(plotOutput(ns("plot.biplot.cars.pca.cylinders")), width = 4)
                )
            ),
            fluidPage(
                fluidRow(
                    h3("Partitioning clustering: A K-means clustering with k = 3:"),
                    box(h5("Note that the stability of the result can be improved by increasing the maximum number of iterations and using multiple random starts."),
                        verbatimTextOutput(ns("summary.kmeans.cars_scores")),
                        width = 12
                    ),
                    box(plotOutput(ns("plot.scatter.matrix.cars_scores.cluster")), width = 4),
                    box(plotOutput(ns("plot.scatter.matrix.cars_scores.origin")), width = 4),
                    box(plotOutput(ns("plot.scatter.matrix.cars_scores.cylinders")), width = 4)
                ),
                fluidRow(
                    box(h5("Change the color of individuals by their cluster"),
                        plotOutput(ns("plot.points.ellipsis.cars.pca.cluster")),
                        width = 4),
                    box(h4("Origin of the cars"),
                        verbatimTextOutput(ns("summary.table.cluster_origin")),
                        verbatimTextOutput(ns("summary.lumping_errors.cluster_origin")),
                        verbatimTextOutput(ns("summary.splitting_errors.cluster_origin")),
                        h5("Here we explore visually how well the clustering recovers the actual origin of the cars"),
                        plotOutput(ns("plot.mapping.cluster_origin")),
                        width = 4),
                    box(h4("Number of Cylinders"),
                        verbatimTextOutput(ns("summary.table.cluster_cylinders")),
                        verbatimTextOutput(ns("summary.lumping_errors.cluster_cylinders")),
                        verbatimTextOutput(ns("summary.splitting_errors.cluster_cylinders")),
                        h5("Here we explore visually how well the clustering recovers the number of cylinders of the cars"),
                        plotOutput(ns("plot.mapping.cluster_cylinders")),
                        width = 4)
                )
            )
    )
}

# Server

hw3_q2_server <- function(input, output, session, cars, cars.pca, cars.scores, cars.k3) {

    # Summary: Pca cars
    output$summary.pca.cars <- renderPrint({
        summary(cars.pca())
    })

    # Summary: Eigen values cars.pca
    output$summary.eig.cars <- renderPrint({
        get_eigenvalue(cars.pca())
    })

    # Plot: Scree cars.pca
    output$plot.scree.cars <- renderPlot({
        fviz_eig(cars.pca(), addlabels = TRUE, ylim = c(0, 85))
    })

    # Plot: Circle pca 1
    output$plot.circle.cars.pca_1 <- renderPlot({
        fviz_pca_var(cars.pca())
    })

    # Plot: Circle pca 2
    output$plot.circle.cars.pca_2 <- renderPlot({
        plot(cars.pca(), shadow = TRUE, choix = "var", select = "contrib 2")
    })

    # Plot: Circle pca 3
    output$plot.circle.cars.pca_3 <- renderPlot({
        fviz_pca_var(cars.pca(), col.var = "cos2",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     repel = TRUE # Avoid text overlapping
        )
    })

    # Plot: Bars contributions cars.pca
    output$plot.bars.contrib_cars.pca <- renderPlot({
        fviz_contrib(cars.pca(), choice = "var", axes = 1:2, top = 10)
    })

    # Plot: Circle contributions cars.pca
    output$plot.circle.contrib_cars.pca <- renderPlot({
        fviz_pca_var(cars.pca(), col.var = "contrib",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
        )
    })

    # Plot: Points cars.pca
    output$plot.points.cars.pca <- renderPlot({
        fviz_pca_ind(cars.pca(), col.ind = "cos2", pointsize = "cos2",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     repel = TRUE # Avoid text overlapping (slow if many points)
        )
    })

    # Plot: Points with ellipsis. cars pca -> origin
    output$plot.points.ellipsis.cars.pca.origin <- renderPlot({
        fviz_pca_ind(cars.pca(),
                     geom.ind = "point", # show points only
                     col.ind = cars$origin, # color by groups
                     palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     addEllipses = TRUE, # Concentration ellipses
                     ellipse.level = 0.95,
                     legend.title = "Number of Cylinders"
        )
    })

    # Plot: Points with ellipsis. cars.pca -> cylinders
    output$plot.points.ellipsis.cars.pca.cylinders <- renderPlot({
        fviz_pca_ind(cars.pca(),
                     geom.ind = "point", # show points only
                     col.ind = cars$cylinders, # color by groups
                     palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     addEllipses = TRUE, # Concentration ellipses
                     ellipse.level = 0.95,
                     legend.title = "Number of Cylinders"
        )
    })

    # Plot: Biplot cars.pca
    output$plot.biplot.cars.pca <- renderPlot({
        fviz_pca_biplot(cars.pca(), repel = TRUE,
                        col.var = "#2E9FDF", # Variables color
                        col.ind = "#696969" # Individuals color
        )
    })

    # Plot: Biplot cars.pca -> origin
    output$plot.biplot.cars.pca.origin <- renderPlot({
        fviz_pca_biplot(cars.pca(),
                col.ind = cars$origin, palette = "jco",
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Origin")
    })

    # Plot: Biplot cars.pca -> cylinders
    output$plot.biplot.cars.pca.cylinders <- renderPlot({
        fviz_pca_biplot(cars.pca(),
                col.ind = cars$cylinders, palette = "jco",
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Number of Cylinders")
    })

    # Summary: Kmeans cars_scores
    output$summary.kmeans.cars_scores <- renderPrint({
        return(cars.k3())
    })

    # Plot: Scatter matrix. Cars_scores -> cluster
    output$plot.scatter.matrix.cars_scores.cluster <- renderPlot({
        pairs(cars.scores(), col = cars.k3()$cluster)
    })

    # Plot: Scatter matrix. Cars_scores -> origin
    output$plot.scatter.matrix.cars_scores.origin <- renderPlot({
        pairs(cars.scores(), col = cars$origin)
    })

    # Plot: Scatter matrix. Cars_scores -> cylinders
    output$plot.scatter.matrix.cars_scores.cylinders <- renderPlot({
        pairs(cars.scores(), col = cars$cylinders)
    })

    # Plot: Points with ellipsis. cars.pca -> cluster
    output$plot.points.ellipsis.cars.pca.cluster <- renderPlot({
        fviz_pca_ind(cars.pca(),
             geom.ind = "point", # show points only
             col.ind = as.factor(cars.k3()$cluster), # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.level = 0.95,
             legend.title = "Cluster"
        )
    })

    # Summary: Table. Cluster - origin
    output$summary.table.cluster_origin <- renderPrint({
        table(cars.k3()$cluster, cars$origin)
    })

    # Summary: Lumping errors. Cluster - origin
    output$summary.lumping_errors.cluster_origin <- renderPrint({
        lumping_errors(cars$origin, cars.k3()$cluster)
    })

    # Summary: Splitting errors. Cluster - origin
    output$summary.splitting_errors.cluster_origin <- renderPrint({
        splitting_errors(cars$origin, cars.k3()$cluster)
    })

    # Plot: Mapping. Cluster - origin
    output$plot.mapping.cluster_origin <- renderPlot({
        newdf = data.frame(col = as.factor(cars.k3()$cluster), shape = cars$origin, PC1 = cars.pca()$ind$coord[, 1], PC2 = cars.pca()$ind$coord[, 2])

        ggplot(data = newdf, aes(x = PC1, y = PC2, colour = col, shape = shape)) +
            geom_text(aes(label = rownames(newdf)), hjust = 1.5) + geom_jitter() + labs(colour = "Origin", shape = "Cluster")
    })

    # Summary: Table. Cluster - cylinders
    output$summary.table.cluster_cylinders <- renderPrint({
        table(cars.k3()$cluster, cars$cylinders)
    })

    # Summary: Lumping errors. Cluster - cylinders
    output$summary.lumping_errors.cluster_cylinders <- renderPrint({
        lumping_errors(cars$cylinders, cars.k3()$cluster)
    })

    # Summary: Splitting errors. Cluster - cylinders
    output$summary.splitting_errors.cluster_cylinders <- renderPrint({
        splitting_errors(cars$cylinders, cars.k3()$cluster)
    })

    # Plot: Mapping. Cluster - cylinders
    output$plot.mapping.cluster_cylinders <- renderPlot({
        newdf = data.frame(col = as.factor(cars.k3()$cluster), shape = cars$cylinders, PC1 = cars.pca()$ind$coord[, 1], PC2 = cars.pca()$ind$coord[, 2])

        ggplot(data = newdf, aes(x = PC1, y = PC2, colour = col, shape = shape)) +
            geom_text(aes(label = rownames(newdf)), hjust = 1.5) + geom_jitter() + labs(colour = "Cylinders", shape = "Cluster")
    })
}