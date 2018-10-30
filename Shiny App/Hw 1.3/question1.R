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
                    plotOutput(ns("plot.pairs"))
                ),
                box(verbatimTextOutput(ns("Sum.pca")))
            )
    )
}

# Server

hw3_q1_server <- function(input, output, session, wines, cars) {

    output$plot.pairs <- renderPlot({
        ggpairs(wns, lower = list(continuous = "points", combo = "facetdensity", mapping = aes(color = type)))
    })

    output$Sum.pca <- renderPrint({
        #This can answer the first two questions 
        summary(w_pca)
    })
}