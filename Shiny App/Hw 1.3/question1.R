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

                )
            )
    )
}

# Server

hw3_q1_server <- function(input, output, session, wines, cars) {



}