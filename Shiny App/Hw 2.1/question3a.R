###################################################
##########         Question 3a        ##############
###################################################

# UI

hw2.1_q3a_ui <- function(id) {
    ns <- NS(id)
    tabPanel(title = "Section a)",
        h3("After we modified the Carat Size variable and turned it into a categorical one we present the results of the linear model here."),
        fluidRow(
            box(status = "primary", verbatimTextOutput(ns("summary3a"))),
            box(p("The regression model is satisfactory as the overall test of significance for the model is 2.2e-16 and R2 = 0.9953 (there is an obvious improvement in the model compared to model 1 )"),
                p("Afterwards, we started plotting a graph using the plot function for the model, as in the below picture.")),
            box(status = "warning", plotOutput(ns("plot.lm3a")))),
        h3("Interpretation of the Plots"),
        tags$ol(
          tags$li("Residuals vs Fitted: we can see that the residuals do follow somehow a linear relationship"),
          tags$li("Normal Q-Q: Residuals come from a normal distribution which is a good"),
          tags$li("In the scale location plot: we can see that the residuals are divided into 2 groups along with the range of the predictor and there is a gap between those 2 groups."),
          tags$li("Residuals vs Leverage: We can see that there are some outliers like: 32, 33 and 210 that are seen far away from the concentrated data on the left")),
        h4("Statistical tests"),
        fluidRow(
            box(verbatimTextOutput(ns("dwtest3a"))),
            box(p("Durbin Watson test: The p-value < 2.2e-16, which means that the residuals have a correlation (not good interpretation for linear regression models)"))),
        fluidRow(
            box(verbatimTextOutput(ns("bptest3a"))),
            box(p("Jarque-Bera: The p-value = 0.172, which means accepting the null hypothesis, so residuals follow a normal distribution (good interpretation about the model)"))),
        fluidRow(
            box(verbatimTextOutput(ns("jarque.bera3a"))),
            box(p("Breusch-Pagan: The p-value equals to 0.0007143, so we will reject the null hypothesis that means variances are not constant.")),
            box(width = 12, p("We can interpret that the interaction term for 'med*weight' is significant as p-value is less than 0.05 (5% level).
             Since the dependent variable (price) is expressed in logs, then we can say that if all the variables are equal, the increase in price for a diamond of medium size is 
             exp(-2.04)= 0.13 times the increase in the price of a small diamond. This means that the price of diamond for small ones increases faster than medium ones, when you increase the carat by one"))),
        fluidRow(
            column(4,
                h4("Testing colour purity differences"),
                verbatimTextOutput(ns("predict.colour"))),
                
            column(4,
                h4("Testing certifier purity differences"),
                verbatimTextOutput(ns("predict.certifier"))),
            column(4,
                h4("Doing statistical tests"),
                verbatimTextOutput(ns("stats")))),
        fluidRow(
              h5("Color Purity is more valued"),
              h5("Fit D value = 9.342298, Fit I value = 8.906038, Fit E value = 9.25695"),
              h5("Average price distance between D and I is: 0.436. Average price distance between D and E is: 0.085"),
              h5("Fit GIA value = 9.342298. Fit IGI value = 9.31861. Fit HRD value = 9.336692"),
              h5("Prices do not differ a lot among different certifiers")
            
        ))
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
      par(mfrow = c(2, 2))
      plot(model3a,which = c(1,2,3,5), ask = F)
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

    output$predict.colour <- renderPrint({
        req(model3a)

        # TODO: Check this print

        predict(model3a, test2[1,], interval = "confidence") ##GIA  
        predict(model3a, test2[2,], interval = "confidence") ##IGI
        predict(model3a, test2[3,], interval = "confidence") ##HRD
    })

    output$predict.certifier <- renderPrint({
        req(model3a)

        # TODO: Check this print

        predict(model3a, test2[1,], interval = "confidence") ##GIA  
        predict(model3a, test2[2,], interval = "confidence") ##IGI
        predict(model3a, test2[3,], interval = "confidence") ##HRD
    })

    output$stats <- renderPrint({
        req(model3a)

        #Check residuals dependancy
        dwtest(model3a, alternative = "two.sided")

        #Check normality for residuals
        jarque.bera.test(model3a$residuals)

        #Check equal variances for residuals
        bptest(model3a)
    })
}