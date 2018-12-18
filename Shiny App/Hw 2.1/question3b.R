###################################################
##########         Question 3b        ##############
###################################################

# UI

hw2.1_q3b_ui <- function(id) {
    ns <- NS(id)
    tabPanel(title = "Section b)",
        h3("The regression model is satisfactory as the overall test of significance for the model is 2.2e-16 and R2 = 0.9953 (there is an obvious improvement in the model compared to model 1 )"),
        p("Afterwards, we started plotting a graph using the plot function for the model, as in the below picture."),
        fluidRow(
            box(width = 12, status = "warning", plotOutput(ns("plot.lm3b")))),
        h3("Interpretation of the Plots:"),
        h4("Statistical tests:"),
        fluidRow(
            box(status = "primary",
                verbatimTextOutput(ns("dwtest3b")),
                verbatimTextOutput(ns("bptest3b"))),
            box(status = "primary",
                verbatimTextOutput(ns("jarque.bera3b"))))
                )
}

# Server

hw2.1_q3b_server <- function(input, output, session) {

    output$plot.lm3b <- renderPlot({
        req(model3b)
        plot(model3b)
    })

    output$dwtest3b <- renderPrint({
        req(model3b)
        dwtest(model3b, alternative = "two.sided")
    })

    output$jarque.bera3b <- renderPrint({
        req(model3b)
        jarque.bera.test(model3b$residuals)
    })

    output$bptest3b <- renderPrint({
        req(model3b)
        bptest(model3b)
    })
}