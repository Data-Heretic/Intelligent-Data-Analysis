###################################################
##########         Question 1        ##############
###################################################

# UI

hw3_q1_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Q1"),
            h2(HTML("<b> Wines | Cars Analysis </b>")),
            h3("Perform a Principal Component Analysis on the wine data set ( 11 quantitative variables, don't include quality, but you might want to include type as a supplementary categorical variable)."),
            fluidPage(
                fluidRow(
                  h4("We will use the R matrix (correlation) because variables are on different scales
You tend to use the covariance matrix (S) when the variable scales are similar and 
the correlation matrix (R) when variables are on different scales
scale.unit=TRUE bases the PCA on the correlation matrix")
                ),
                fluidRow(
                    plotOutput(ns("plot.pairs")), type = getOption("spinner.type", default = 1)
                ),
                fluidRow(
                  column(6,
                  box(h5("First 4 components"),
                    verbatimTextOutput(ns("f4comp")),
                    h5("First 4 cumulative components"),
                    verbatimTextOutput(ns("fc4comp")),
                    align = "center",width = 12),offset=3
                 ) 
                ),
                box(verbatimTextOutput(ns("Sum.pca")))
            )
    )
}

# Server

hw3_q1_server <- function(input, output, session, wines, cars) {

    output$plot.pairs <- renderPlot({
      dat <- data.frame(x = numeric(0), y = numeric(0))
      
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Making plot", value = 0)
      
      # Number of times we'll go through the loop
      n <- 30
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        progress$inc(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
      
      ggpairs(wns, lower = list(continuous = "points", combo = "facetdensity", mapping = aes(color = type)))
      
      
    })

    output$f4comp <- renderPrint({
        #First 4 components 
        w_pca$eig[1:4,2]
    })
    output$fc4comp <- renderPrint({
      #This can answer the first two questions 
      w_pca$eig[1:4,3]
    })
    output$Sum.pca <- renderPrint({
      #This can answer the first two questions 
      summary(w_pca)
    })
}