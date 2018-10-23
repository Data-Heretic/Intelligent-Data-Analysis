###################################################
##########         Question 1c       ##############
###################################################

# UI

hw2_q1c_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Q1c"),
            h3("Choose a subset of 4 or 5 quantitative variables and explore linear relationships."),
            fluidPage(
                h1("R matrix of pairwise correlations"),
                div(HTML("<ul><li>Moderate relation between Chloride and Sulfates which makes sense as both together regulate pH, the first is used to control the saltiness (chemical precipitation of salts) and the second controls the precipitation of acids. </li><li>Very Moderate relation between Alcohol % and quality perceived by judges </li><li>Neglible relation between Alcohol % and Residual sugar which is logical as Alcohol is made out from the fermentation of sugar molecules</li></ul>")),
                fluidRow(
                    column(4, box(h5("Reordering the correlation matrix: there are different methods. Sometimes it is useful for minning the hidden structure and pattern in the matrix. "),
                    plotOutput(ns("plot.corrplot.mixed_rmatrix_AOE")), width = 12)
                        
                    ),
                    column(4,
                        box(h5("Other times, using correlation plots with the aid of piecharts can help us interpretated information more quickly"),plotOutput(ns("plot.corrgram.wines_pie")), width = 12)
                    ),
                    column(4,
                        box(h5("You can even use scatterplots in the correlation plot, to visualize the linear regression between the variables "), plotOutput(ns("plot.corrgram.wines_pts")), width = 12)
                    )
                ),
                h1("Matrix of partial correlations"),
                div(HTML("<ul>Partial conrrelation between 2 given variables, removes the external influences of the rest of the variables, by comparing this matrix with the R matrix we can reveal hidden relations between 2 variables alone , e.g. in our example we can see that the relations commented before are only enhanced so there are not hidden relations revealed. The Pearson coefficient between Quality perceived and Alcohol percentage is still on the same order on magnitude.
                    This also applies to the relationship between Chlorides and Sulfites,BUT Alcohol and Residual Sugar is enhanced which makes the most sense as the the fermentation of sugar molecules creates ethanol molecules that increase the Alcohol percentage,. ")),
                fluidRow(
                    column(4, 
                    box(plotOutput(ns("plot.corrplot.mixed_partial_matrix")), width = 12)),
                    column(4, box(plotOutput(ns("plot.corrgram.partial_matrix_pie")), width = 12))
                ),
                fluidRow(
                    box(h5("Coefficient of determination (function r2multv() we define in R)"),
                        h5("Chlorides (chlor) is the best linearly explained by the others (R^2 = 0.358), followed by Sulphates (S, R^2 = 0.326). The worst linearly explained by the others is Sugar Residual (ResSug, R^2 = 0.15)."),
                        verbatimTextOutput(ns("summary.coefficient_determination")),
                        h5("Are any linear relationships present in this dataset? Let's calculate the determinant of S and R:"),
                        verbatimTextOutput(ns("summary.determinant.cor")),
                        verbatimTextOutput(ns("summary.determinant.cov")),  
                        h5("Effective dependence coefficient"),
                        verbatimTextOutput(ns("summary.effective_depend_coefficient")),
                        h5("This explains that the linear dependances between all variables selected have a low influence (17%) but still some, on the variability of the dataset.")
                    ),
                    box(h5("Find the variables involved in the overall linear dependence"),
                        h5("Assuming all values are close to 0 except the third 0.989, we can conclude that this variable has very small variance, it is almost constant through the observed data."),
                        verbatimTextOutput(ns("summary.eigen"))
                        
                    )
                )
            )
    )
}

# Server

hw2_q1c_server <- function(input, output, session, wines_for_correlations) {

    

    # Plot: Corrplot mixed rmatrix AOE #####
    output$plot.corrplot.mixed_rmatrix_AOE <- renderPlot({
        rmatrix <- cor(wines_for_correlations()) #R matrix with Pearson
        corrplot.mixed(rmatrix, order = "AOE")
    })


    # Plot: Corrgram wines. Pie format ·####
    output$plot.corrgram.wines_pie <- renderPlot({
        corrgram(wines_for_correlations(), order = TRUE,
         lower.panel = panel.shade, upper.panel = panel.pie,
         diag.panel = panel.minmax, text.panel = panel.txt)
    })

    # Plot: Corrgram wines. Pts format ####
    output$plot.corrgram.wines_pts <- renderPlot({
        corrgram(wines_for_correlations(), order = TRUE,
         lower.panel = panel.shade, upper.panel = panel.pts,
         diag.panel = panel.minmax, text.panel = panel.txt)
    })


    # Plot: Corrgram partial matrix. Pie format. ####
    output$plot.corrgram.partial_matrix_pie <- renderPlot({
        partialmatrix <- pcor(wines_for_correlations())$estimate
        corrgram(partialmatrix,
         lower.panel = panel.shade, upper.panel = panel.pie,
         diag.panel = panel.minmax, text.panel = panel.txt)
    })

    # Plot: Corrplot mixed partial matrix. Order AOE ###
    output$plot.corrplot.mixed_partial_matrix <- renderPlot({
        partialmatrix <- pcor(wines_for_correlations())$estimate
        corrplot.mixed(partialmatrix, order = "AOE")
    })

    # Summary: Coefficient of determination
    output$summary.coefficient_determination <- renderPrint({
        r2multv <- function(x) {
            r2s = 1 - 1 / (diag(solve(cov(x))) * diag(cov(x)))
            r2s
        }
        return(r2multv(wines_for_correlations()))
    })

    # Summary: Determinant correlation
    output$summary.determinant.cor <- renderPrint({
        return(det(cor(wines_for_correlations())))
    })

    # Summary: Determinant covariance
    output$summary.determinant.cov <- renderPrint({
        return(det(cov(wines_for_correlations())))
    })

    # Summary: eigen value
    output$summary.eigen <- renderPrint({
        return(eigen(cov(wines_for_correlations())))
    })

    # Summary: Effective dependence coefficient
    output$summary.effective_depend_coefficient <- renderPrint({
        return(1 - det(cor(wines_for_correlations())) ^ { 1 / 4 })
    })
}