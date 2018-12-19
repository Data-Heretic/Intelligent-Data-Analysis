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
             box(status = "warning", plotOutput(ns("plot.lm2"))),
             box(status = "warning", plotOutput(ns("plot.lm3")))
            )
        
        )
}

# Server

hw2.1_q1_server <- function(input, output, session) {
    
    output$plot.lm2 <- renderPlot({ plot(diamo2$Carat, diamo2$Price)
                                  })
    output$plot.lm3 <- renderPlot({ plot(diamo2$Carat, log(diamo2$Price))
                                  })
    
    

}
