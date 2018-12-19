###################################################
##########         Question 1        ##############
###################################################

# UI

hw2.1_q1_ui <- function(id) {
    ns <- NS(id)
    tabPanel(title = "Question 1",
        h3("Plot price vs. caratage and log(price) vs. caratage. Decide on which response variable is better to use."),
        fluidRow(
                 p("For linear regression models, we want to make sure that there is a linear
                    relationship between the input and output variables. Taking the log to the price
                       makes the relationship between price and carat looks more linear. This is our
                    main objective for linear regression, As we can see in the below graphs,
                    plotting the log price gives a better representation of the variables."),
             box(status = "warning", plotOutput(ns("plot.lm1"))),
             box(status = "warning", plotOutput(ns("plot.lm2")))
            ),
        h3("The parameters in the given model are represented as below"),
        
        fluidRow(
                 box(status = "primary", verbatimTextOutput(ns("summary1"))),
                 p("As we can see in the above summary, the overall model has a Multiple R-squared of 0.9723, so the model is able to predict any Y using X with accuracy of 97%."),
                 p("Test of significance for the overall model is 2.2e-16, We can reject the null hypothesis which assumes that all the Bi is equal to 1. We can conclude that the variables are significant for the model."),
                 p("For the weight coefficient: Since the dependent variable is expressed in log, then we can say either: - In log scale: If you increase one unit of cartage , log(price) will increase by 2.855 In Price scale: If you increase one unit of cartage the price of diamond is multiplied by a factor of exp (2.885) =17.37466"),
                 p("For the color purity coefficient: Since the dependent variable is expressed in log and reference category is “I”, then we can say that the price of “ColourPurityD” is exp(0.41)=1.53 times the price of “ColourPurityI” , we can conclude that “ColourPurityD” is much better than the reference category"),
                 p(" For the certifiers coefficient, we can see that the HRD and GIA are better than IGI but with a very small difference")
                 

            )
        )
}

# Server

hw2.1_q1_server <- function(input, output, session) {
    output$plot.lm1 <- renderPlot({
    req(model1)
    plot(model1, which = c(1:4), ask = F)
    })
    output$plot.lm2 <- renderPlot({ plot(diamo2$Carat, diamo2$Price)
                                  })
    output$plot.lm3 <- renderPlot({ plot(diamo2$Carat, log(diamo2$Price))
                                  })
    output$summary1 <- renderPrint({ summary(model1) })
    

    output$summary1 <- renderPrint({
        req(model1)
        summary(model1)
    })
}
