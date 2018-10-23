###################################################
##########         Question 1b       ##############
###################################################

# UI

hw2_q1b_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Q1b"),
            h2(HTML("<b> Wines Analysis </b>")),
            h3("Choose two quantitative variables and describe its joint bivariate distribution. Does it seem to be Normal? Are there any outliers?"),
            h4("The variables chosen are: Volatile acidity and residual sugar"),
            fluidPage(
                fluidRow(
                    box(verbatimTextOutput(ns("test.jarque_bera.volAcid")),
                        verbatimTextOutput(ns("test.jarque_bera.resSug")),
                        h5("Doing shapiro test to check normality of the two variables"),
                        verbatimTextOutput(ns("test.shapiro.volAcid_resSug"))),
                    box(plotOutput(ns("plot.scatterplot.volAcid_resSug")))
                ),
                fluidRow(
                  h5("Power transformation for bivariate data. Bivariate Normality for the joint variable (wines$CitAcid,wines$A). Estimating bivariate parameter (\\lambda_1,\\lambda_2)"), verbatimTextOutput(ns("summary.transformation.volAcid_resSug"))
                ),
                fluidRow(
                    
                    box(h5("Bivariate normality before the transformation."), plotOutput(ns("test.mardia.before_transf")), h5("We reject normality given p values equal to 0 for skewness and kurtosis.")),
                    box(h5("Bivariate normality after the transformation. Now we are going to transform them with (\\lambda_1,\\lambda_2) values =c(0.0584, -0.7548). Defining the transformed variable with those lambdas."), plotOutput(ns("test.mardia.after_transf")), h5("We have not improved normality."))
                ),
                h3("Outliers"),
                fluidRow(
                    box(h5("Multivariate outliers, 'by hand' with Mahalanobis distance, non-robust. Variables: wines$VolAcid, wines$ResSug"),
                        h5("Mahalanobis:"),
                        verbatimTextOutput(ns("summary.mahalanobis")),
                        h5("95th percentile of a chi-squared distribution with 2 degrees of freedom (we are using 2 variables). Position of outliers"),
                        verbatimTextOutput(ns("summary.mahalanobis.position")),
                        h5("We got 8 outliers, which are:"),
                        verbatimTextOutput(ns("summary.mahalanobis.outliers"))
                    ),
                    box(plotOutput(ns("plot.scatterplot.outliers")), plotOutput(ns("plot.qq.outliers")))
                )
            )
    )
}

# Server

hw2_q1b_server <- function(input, output, session, mahalanobis24) {

    # Jarque bera test: wines$volAcid
    output$test.jarque_bera.volAcid <- renderPrint({
        return(jarque.bera.test(wines$VolAcid))
    })

    # Jarque bera test: wines$resSug
    output$test.jarque_bera.resSug <- renderPrint({
        return(jarque.bera.test(wines$ResSug))
    })

    # Shapiro test: normality of VolAcid and ResSug
    output$test.shapiro.volAcid_resSug <- renderPrint({
        return(shapiro.test(cbind(wines$VolAcid, wines$ResSug)))
    })

    # Plot: Joint distribution per group
    output$plot.scatterplot.volAcid_resSug <- renderPlot({
        ScatterHist(wines,"VolAcid", "ResSug",
             title = "Joint distribution per group",contour = TRUE)
    })

    

    # Fit: Bivariate Normality before the transformation
    # We reject normality given p values equal to 0 for skewness and kurtosis
    output$test.mardia.before_transf <- renderPlot({
        mvn(cbind(wines$VolAcid, wines$ResSug), mvnTest = "mardia", multivariatePlot = "qq")
    })

    # Summary: Power transformation for bivariate data
    # Bivariate Normality for the joint variable (wines$CitAcid,wines$A)
    # Estimating bivariate parameter (\lambda_1,\lambda_2)
    output$summary.transformation.volAcid_resSug <- renderPrint({
        return(summary(powerTransform(cbind(wines$VolAcid, wines$ResSug) ~ 1)))
    })

    # Fit: Bivariate Normality after the transformation
    # Now we are going to transform them with (\lambda_1,\lambda_2) values =c(0.0584, -0.7548).
    # Defining the transformed variable with those lambdas
    # We have not improved normality
    output$test.mardia.after_transf <- renderPlot({
        winesT <- bcPower(cbind(wines$VolAcid, wines$ResSug), c(0.0584, -0.7548))
        mvn(winesT, mvnTest = "mardia", multivariatePlot = "qq")
    })

    # Mahalanobis
    output$summary.mahalanobis <- renderPrint({
        return(mahalanobis24())
    })

    # Mahalanobis position
    output$summary.mahalanobis.position <- renderPrint({
        return(which(mahalanobis24() > qchisq(0.95, df = 2)))
    })

    # Mahalanobis outliers
    output$summary.mahalanobis.outliers <- renderPrint({
        pos <- which(mahalanobis24() > qchisq(0.95, df = 2))
        return(mahalanobis24()[pos])
    })

    # Plot: Scatterplot of outliers
    output$plot.scatterplot.outliers <- renderPlot({
        pos <- which(mahalanobis24() > qchisq(0.95, df = 2))
        x = rep(1, dim(wines)[1])
        x[pos] = 0
        plot(wines[, 2], wines[, 4], col = x + 2, pch = 16, xlab = "Volatile Acidity", ylab = "Residual Sugar")
    })

    # Plot: QQ plot of outliers
    output$plot.qq.outliers <- renderPlot({
        qqPlot(mahalanobis24(), dist = "chisq", df = 2, line = "robust")
    })
}