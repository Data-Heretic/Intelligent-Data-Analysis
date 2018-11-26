###################################################
##########         Question 3a        ##############
###################################################

# UI

hw2.1_q3a_ui <- function(id) {
    ns <- NS(id)
    tabPanel(title = "Section a)",
            h2(HTML("<b> Caterage </b>")),
            h3("Description."),
            fluidPage(
              h4("After we modified the Carat Size variable and turned it into a categorical one we present the results of the linear model here: "),
              box(status = "primary",plotOutput(ns("summary3a"))),
           
              h4("The regression model is satisfactory as the overall test of significance for the model is 2.2e-16 and R2 = 0.9953 
                 (there is an obvious improvement in the model compared to model 1 )"),
              h4("Afterwards, we started plotting a graph 
                 using the plot function for the model, as in the below picture."),
              box(status = "warning",plotOutput(ns("plot.lm3a"))),
              h4("Interpretation of the Plots:"),
              box(),
              h4("Statistical tests:"),
              box(status = "primary",
                  plotOutput(ns("dwtest3a")),
                  plotOutput(ns("jarque.bera3a")),
                  plotOutput(ns("bptest3a"))
                )
            ),
            fluidRow(
              column(4,
                     h4("Testing Colour purity differences"),
                     box(status="primary",
                         plotOutput(ns("predictColour"))
                     )  
              ),
              column(4,
                     h4("Testing Colour purity differences"),
                     box(status="primary",
                         plotOutput(ns("predictColour"))
                     )  
              ),
              column(4,
                     h4("Doing statistical tests"),
                     box(status="primary",
                         plotOutput(ns("stats"))
                     )  
              )
              
            )
    )
}

# Server

hw2.1_q3a_server <- function(input, output, session) {
  output$summary3a <- renderPlot({
    summary(model3a)
  })
  output$plot.lm3a <- renderPlot({
    plot(model3a)
    
      })
  output$dwtest3a <- renderPlot({
    dwtest(model3a, alternative="two.sided")
    
  })
  output$jarque.bera3a <- renderPlot({
    jarque.bera.test(model3a$residuals)
  })
  output$bptest3a <- renderPlot({
    bptest(model3a)
    
  })
  output$predictColour <- renderPlot({
    predict(model3a,test2[1,], interval = "confidence")##GIA  
    predict(model3a,test2[2,], interval = "confidence")##IGI
    predict(model3a,test2[3,], interval = "confidence")##HRD
  })
  output$predictCertifier <- renderPlot({
    predict(model3a,test2[1,], interval = "confidence")##GIA  
    predict(model3a,test2[2,], interval = "confidence")##IGI
    predict(model3a,test2[3,], interval = "confidence")##HRD
    
  })
  output$stats <- renderPlot({
    #Check residuals dependancy
    dwtest(model2, alternative="two.sided")
    
    #Check normality for residuals
    jarque.bera.test(model2$residuals)
    
    #Check equal variances for residuals
    bptest(model2)
    
  })
}