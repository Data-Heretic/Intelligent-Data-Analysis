###################################################
##########         Question 2        ##############
###################################################

# UI

hw2.1_q2_ui <- function(id) {
    ns <- NS(id)
    tabPanel(title = "Question 2",
        column(10,
            h2(hw2.1_title),
            h6("Find a suitable way to include, besides caratage, the other categorical information
available: clarity, color and certicate. Use the worst level of each categorical variables
as the reference category. Comment on the model tted, and perform a basic analysis
of the residuals.."),
            fluidPage(
                    h5("The parameters in the given model are represented as below"),

        fluidRow(column(12,
                 box(status = "primary", verbatimTextOutput(ns("summary1"))),
                 p("As we can see in the above summary, the overall model has a Multiple R-squared of 0.9723, so the model is able to predict any Y using X with accuracy of 97%."),
                 p("Test of significance for the overall model is 2.2e-16, We can reject the null hypothesis which assumes that all the Bi is equal to 1. We can conclude that the variables are significant for the model."),
                 p("For the weight coefficient: Since the dependent variable is expressed in log, then we can say either: - In log scale: If you increase one unit of cartage , log(price) will increase by 2.855 In Price scale: If you increase one unit of cartage the price of diamond is multiplied by a factor of exp (2.885) =17.37466"),
                 p("For the color purity coefficient: Since the dependent variable is expressed in log and reference category is “I”, then we can say that the price of “ColourPurityD” is exp(0.41)=1.53 times the price of “ColourPurityI” , we can conclude that “ColourPurityD” is much better than the reference category"),
                 p(" For the certifiers coefficient, we can see that the HRD and GIA are better than IGI but with a very small difference"))


    ),
            fluidRow(column(12,
                     h5("Plot Analysis"),
                     p("In order to make some interpretation on the residuals, we started by plotting a graph using the plot function for the model."),
                     p("From the graph we can see that:"),
                     p("1. The residuals don’t behave nicely , there is a nonlinear relationship between the outcome and predictor (seem to be following a curve plot not a linear plot)"),
                     p("2. Residuals come from a normal distribution which is a good"),
                     p("3. In the scale location plot, we can see that the residuals are spread somehow equally along with the range of the predictor."),
                     p("In Cooks distance plot, we can see that there are 3 influential values , which are: 110, 214 and 223"))),
            fluidRow(column(12,
                     box(status = "warning", plotOutput(ns("plot.lm1"))))),
            fluidRow(column(12,
                     h5("Statistical Analysis"),
                     p("We can test residuals dependency using “Durbin Watson test” , 
                     the p-value for the test is p-value ( < 2.2e-16 ) , so we can reject the null 
                     hypothesis which means that the residuals are dependent and have correlation (not a good interpretation, residuals should be independent)"))),
fluidRow(column(12,
                     box(status = "primary", verbatimTextOutput(ns("dwtest2"))))),
fluidRow(column(12,
                     p("Jarque-Bera test has been used to check for residuals normality and the 
                     p-value of the residuals is p-value = 0.1952 , which means accepting the 
                     null hypothesis, so residuals follow a normal distribution (good 
                     interpretation about the model)"))),
fluidRow(column(12,
                     box(status = "primary", verbatimTextOutput(ns("jarqueber2"))))),
                     p("To check for variance equality , Breusch-Pagan test have been used
                     , the p-value equals to 0.3507 , so we will accept the null hypothesis that
                     means variances are constant (good interpretation about the model)"),
fluidRow(column(12,
                     box(status = "primary", verbatimTextOutput(ns("bptest"))))),
fluidRow(column(12,
                     p("We can see that there are 3 values that can be considered as outliers: 152,214,110"))),
fluidRow(column(12,
                     box(status = "primary", verbatimTextOutput(ns("outlier1"))))),
fluidRow(column(12,
                     box(status = "primary", verbatimTextOutput(ns("outlier2"))))),
fluidRow(column(12,
                     box(status = "primary", verbatimTextOutput(ns("outlier3"))))),
fluidRow(column(12,
                     p("After removing the above outliers from the model, there was no enhancement done and other outliers appeared, we can see that from the below"))),
                     fluidRow(column(12,
                     box(status = "primary", verbatimTextOutput(ns("summary2")))))
                     )
                    

            )
        )
    
}

# Server

hw2.1_q2_server <- function(input, output, session) {
    output$summary1 <- renderPrint({ summary(model1) })

    output$plot.lm1 <- renderPlot({
    req(model1)
    par(mfrow = c(2, 2))
    plot(model1, which = c(1:4), ask = F)})

    output$dwtest2 <- renderPrint({ dwtest(model1, alternative = "two.sided") })
    output$jarqueber2 <- renderPrint({ jarque.bera.test(model1$residuals) })
    output$bptest <- renderPrint({ bptest(model1) })

    output$summary1 <- renderPrint({ summary(model1) })
    output$outlier1 <- renderPrint({ diamonds[152,] })
    output$outlier2 <- renderPrint({ diamonds[214,] })
    output$outlier3 <- renderPrint({ diamonds[110,] })
    output$summary2 <- renderPrint({ summary(modelNoOutlier) })

}