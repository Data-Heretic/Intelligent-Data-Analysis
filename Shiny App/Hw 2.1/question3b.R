###################################################
##########         Question 3b        ##############
###################################################

# UI

hw2.1_q3b_ui <- function(id, options) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Q3b"),
            h2(HTML("<b> Caterage </b>")),
            h4("Description."),
            fluidPage(
              h4("The regression model is satisfactory as the overall test of significance for the model is 2.2e-16 and R2 = 0.9953 
                 (there is an obvious improvement in the model compared to model 1 )"),
              h4("Afterwards, we started plotting a graph 
                 using the plot function for the model, as in the below picture."),
              box(status = "warning",plotOutput(ns("plot.lm3b"))),
              h4("Interpretation of the Plots:"),
              box(),
              h4("Statistical tests:"),
              box(status = "primary",
                  plotOutput(ns("dwtest3b")),
                  plotOutput(ns("jarque.bera3b")),
                  plotOutput(ns("bptest3b"))
              )
            )
            
    )
}

# Server

hw2.1_q3b_server <- function(input, output, session) {
  output$plot.lm3b <- renderPlot({
    plot(model3b)
    
  })
  output$dwtest3b <- renderPlot({
    dwtest(model3b, alternative="two.sided")
    
  })
  output$jarque.bera3b <- renderPlot({
    jarque.bera.test(model3b$residuals)
  })
  output$bptest3b <- renderPlot({
    bptest(model3b)
    
  })
}