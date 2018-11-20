###################################################
##########         Question 3a        ##############
###################################################

# UI

hw2.1_q3a_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Q3a"),
            h2(HTML("<b> Caterage </b>")),
            h3("Description."),
            fluidPage(
              h4("After we modified the Carat Size variable and turned it into a categorical one we present the results of the linear model here: "),
              box(status = "primary",plotOutput(ns("summary3a")))
            ),
            fluidPage(
              h4("The regression model is satisfactory as the overall test of significance for the model is 2.2e-16 and R2 = 0.9953 
                 (there is an obvious improvement in the model compared to model 1 )"),
              h4("Afterwards, we started plotting a graph 
                 using the plot function for the model, as in the below picture."),
              box(status = "warning",plotOutput(ns("plot.lm3a"))),
              h4("Interpretation of the Plots:"),
              box()
            )
    )
}

# Server

hw2.1_q3a_server <- function(input, output, session) {
  output$summary3a <- renderPlot({
    summary(model2)
  })
  output$plot.lm3a <- renderPlot({
    summary(model2)
  })
}