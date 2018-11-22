###################################################
##########         Question 1a       ##############
###################################################

# UI

hw1.2_q1a_ui <- function(id) {
    ns <- NS(id)
    tabPanel(title = "Section a)",
        h3("Choose a quantitative variable and explore its distribution in terms of descriptive measures of center, dispersion, skewness and kurtosis. Is a normal model a plausible one for its distribution? If the answer is no, can you think of a transformation of the variable that improves normality. Are there any outliers?"),
        fluidRow(
            column(6,
                fluidRow(
                    box(title = h5("Summary"), width = 12, verbatimTextOutput(ns("summary"))),
                    box(title = h5("Sample variance"), width = 12, verbatimTextOutput(ns("sample_variance"))),
                    box(title = h5("Standard deviation"), width = 12, verbatimTextOutput(ns("standard_deviation"))))),
            column(6,
                fluidRow(
                    box(title = h5("Quantile"), width = 12, verbatimTextOutput(ns("quantile"))),
                    box(title = h5("Coefficient variation"), width = 12, verbatimTextOutput(ns("coefficient_variation")))))),
        fluidRow(
            box(width = 12, p("The skewness here is 0.2167518. This value implies that the distribution of the data is slightly skewed to the right or positive skewed. It is skewed to the right because the computed value is positive, and is slightly, because the value is close to zero. For the kurtosis, we have 2.073274 implying that the distribution of the data is platykurtic, since the computed value is less than 3.")),
            box(verbatimTextOutput(ns("test.kurtosis")), verbatimTextOutput(ns("test.skewness")), verbatimTextOutput(ns("test.jarque_bera"))),
            box(plotOutput(ns("histogram_2"))),
            box(width = 12, p("The estimated parameter for the transformation is lamda=0.5788 together with its confidence interval (0.2971,0.8658). Then, it follows two tests on two specific values for lamda, lamda=0 which stands for the logarithm transformation and lamda=1 which means that you don?t need a transformation to actually improve the normality of your data. In this particular output, we observe a p-value very small for both lamda = 0 and lamda = 1 so we reject the hypothesis that lamda=0 or lamda=1 is a good transformation value. We can define now a transformed variable, using lamda=0.5788 and see if the normality has been improved."))),
        fluidRow(
            box(width = 12, h5("Power Transformations, Box-Cox transformation to improve normality"), verbatimTextOutput(ns("power_transform"))),
            box(verbatimTextOutput(ns("test.kurtosis_transformed")),  verbatimTextOutput(ns("test.skewness_transformed")), verbatimTextOutput(ns("test.jarque_bera_transformed"))),
            box(plotOutput(ns("histogram_3"))),
            box(width = 12, h3("Outliers"), p("We can see that there are not apparent outliers")),
            box(plotOutput(ns("boxplot"))))
    )
}

# Server

hw1.2_q1a_server <- function(input, output, session, wines_TS02) {

    # summary
    output$summary <- renderPrint({
        return(summary(wines$TSo2))
    })

    
    # sample variance
    output$sample_variance <- renderPrint({
        return(var(wines$TSo2, na.rm = TRUE))
    })

    # standard deviation
    output$standard_deviation <- renderPrint({
        return(sd(wines$TSo2, na.rm = TRUE))
    })

    # coefficient of variation (percentage)
    output$coefficient_variation <- renderPrint({
        return(sd(wines$TSo2, na.rm = TRUE) / mean(wines$TSo2, na.rm = TRUE) * 100)
    })

    # quantile
    output$quantile <- renderPrint({
        return(quantile(wines$TSo2, na.rm = TRUE))
    })

    # anscombe glynn test (kurtosis)
    output$test.kurtosis <- renderPrint({
        return(anscombe.test(wines$TSo2))
    })

    # d'agostino test (skewness)
    output$test.skewness <- renderPrint({
        return(agostino.test(wines$TSo2))
    })

    # histogram 2
    output$histogram_2 <- renderPlot({
        d <- density(wines$TSo2)
        hist(wines$TSo2, breaks = 10, probability = T, col = "blue", border = "white", xlim = c(0, 250), main = "Histogram of Total Sulflur Dioxide", xlab = "Total Sulflur dioxide")
        lines(d, col = "red", lwd = 2)
        abline(v = mean(wines$TSo2), col = "red", lwd = 2)
        legend("topright", legend = c("skewness = 0.21", "kurtosis = 2.07"), box.lty = 0, cex = 0.8)
    })

    # jarque bera test
    output$test.jarque_bera <- renderPrint({
        return(jarque.bera.test(wines$TSo2))
    })

    # power transform
    output$power_transform <- renderPrint({
        return(summary(powerTransform(wines$TSo2)))
    })

    # transformed: anscombe glynn test (kurtosis) 
    output$test.kurtosis_transformed <- renderPrint({
        return(anscombe.test(wines_TS02()))
    })

    # transformed: d'agostino test (skewness)
    output$test.skewness_transformed <- renderPrint({
        return(agostino.test(wines_TS02()))
    })

    # transformed: jarque bera test
    output$test.jarque_bera_transformed <- renderPrint({
        return(jarque.bera.test(wines_TS02()))
    })

    # histogram 3
    output$histogram_3 <- renderPlot({
        d <- density(wines_TS02())
        hist(wines_TS02(), breaks = 10, probability = T, col = "blue", border = "white", main = "Histogram of Power transformation of Total Sulflur dioxide", xlab = "PT of Total Sulflur dioxide")
        lines(d, col = "red", lwd = 2)
        abline(v = mean(wines_TS02()), col = "red", lwd = 2)
        legend("topright", legend = c("skewness = -0.17", "kurtosis = 1.94"), box.lty = 0, cex = 0.8)
    })

    # Outliers: boxplot
    output$boxplot <- renderPlot({ # Both shows the same plot
        #return(boxplot(wines$TSo2, id.method = "y"))
        #return(boxplot(wines$TSo2)$out)
        return(boxplot(wines$TSo2,col="green",main="No outliers detected",xlab="Sulflur Dioxide"))
    })
}