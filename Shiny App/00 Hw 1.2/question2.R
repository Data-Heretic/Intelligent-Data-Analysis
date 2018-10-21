###################################################
##########         Question 2        ##############
###################################################

# UI

hw2_q2_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Q2"),
            h3("Permutation test"),
            fluidPage(
                  fluidRow(box(h5("Correlation coefficients"), verbatimTextOutput(ns("correlation_coefficients")),
                               h5("Permutation based p-value for the Pearson correlation coefficient"), verbatimTextOutput(ns("permutation"))),
                           box(plotOutput(ns("plot_tips_pctTip")))
                  ),
                  fluidRow(box(h5("Correlation"), verbatimTextOutput(ns("correlation")),
                               h5("Probability randomized"), verbatimTextOutput(ns("probability_randomized"))),
                           box(plotOutput(ns("histogram_prob_random")))
                  )
            )
    )
}

# Server

hw2_q2_server <- function(input, output, session) {

    # plot tips ~ pctTip
    output$plot_tips_pctTip <- renderPlot({
        return(plot(restaurantTips$Bill ~ restaurantTips$PctTip))
    })

    # Correlation coefficients and tests
    output$correlation_coefficients <- renderPrint({
        return(rcorr(as.matrix(restaurantTips[, c(1, 7)]), type = "pearson"))
    })

    # Permutation based p-value for the Pearson correlation coefficient
    output$permutation <- renderPrint({
        return(permcor(restaurantTips$Bill, restaurantTips$PctTip, R = 10000))
    })

    # Correlation
    output$correlation <- renderPrint({
        r.obt <- rcorr(as.matrix(restaurantTips[, c(1, 7)]), type = "pearson")$r[1, 2]
        return(cat("The obtained correlation is ", r.obt, '\n'))
    })

    # Probability randomized
    output$probability_randomized <- renderPrint({
        nreps <- 10000
        r.random <- replicate(nreps, { Y <- restaurantTips$PctTip; X <- sample(restaurantTips$Bill, length(restaurantTips$Bill), replace = FALSE); cor(X, Y) })
        prob <- length(r.random[r.random >= r.obt]) / nreps
        return(cat("Probability randomized r >= r.obt", prob))
    })

    # Histogram
    output$histogram_prob_random <- renderPlot({
        return(hist(r.random, breaks = 50, main = expression(paste("Distribution around p = 0")), xlab = "r from randomized samples"))
    })
}