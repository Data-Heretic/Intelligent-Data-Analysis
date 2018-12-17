###################################################
##########         Question 3a        ##############
###################################################

# UI

hw2.1_q3a_ui <- function(id) {
    ns <- NS(id)

    tabPanel(title = "Section a)",
        h3("After we modified the Carat Size variable and turned it into a categorical one we present the results of the linear model here: "),
        fluidRow(
            box(status = "primary", verbatimTextOutput(ns("summary3a"))),
            p("The regression model is satisfactory as the overall test of significance for the model is 2.2e-16 and R2 = 0.9953 (there is an obvious improvement in the model compared to model 1 )"),
            p("Afterwards, we started plotting a graph using the plot function for the model, as in the below picture."),
            box(status = "warning", plotOutput(ns("plot.lm3a")))),
        h3("Interpretation of the Plots"),
        h4("Statistical tests"),
        fluidRow(
            box(status = "primary",
                verbatimTextOutput(ns("dwtest3a")),
                verbatimTextOutput(ns("bptest3a"))),
            box(status = "primary",
                verbatimTextOutput(ns("jarque.bera3a")))),
        fluidRow(
            column(4,
                    h4("Testing Colour purity differences"),
                    box(status = "primary", plotOutput(ns("predictColour")))),
            column(4,
                    h4("[[ Pending ]]"),
                    box(status = "primary", plotOutput(ns("predictCertifier")))),
            column(4,
                    h4("Doing statistical tests"),
                    box(status = "primary", verbatimTextOutput(ns("stats"))))))
}

# Server

hw2.1_q3a_server <- function(input, output, session) {

    output$summary3a <- renderPrint({
        req(model3a)
        print("---> model3a")
        summary(model3a)
    })

    output$plot.lm3a <- renderPlot({
        req(model3a)
        plot(model3a)
    })

    output$dwtest3a <- renderPrint({
        req(model3a)
        dwtest(model3a, alternative = "two.sided")
    })

    output$jarque.bera3a <- renderPrint({
        req(model3a)
        jarque.bera.test(model3a$residuals)
    })

    output$bptest3a <- renderPrint({
        req(model3a)
        bptest(model3a)
    })

    output$predictColour <- renderPlot({
        req(model3a)
        predict(model3a, test2[1,], interval = "confidence") ##GIA  
        predict(model3a, test2[2,], interval = "confidence") ##IGI
        predict(model3a, test2[3,], interval = "confidence") ##HRD
    })

    output$predictCertifier <- renderPlot({
        req(model3a)
        predict(model3a, test2[1,], interval = "confidence") ##GIA  
        predict(model3a, test2[2,], interval = "confidence") ##IGI
        predict(model3a, test2[3,], interval = "confidence") ##HRD
    })

    output$stats <- renderPrint({
        req(model2)

        #Check residuals dependancy
        dwtest(model2, alternative = "two.sided")

        #Check normality for residuals
        jarque.bera.test(model2$residuals)

        #Check equal variances for residuals
        bptest(model2)
    })
}