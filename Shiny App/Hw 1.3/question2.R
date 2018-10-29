###################################################
##########         Question 2        ##############
###################################################

# UI

hw3_q2_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Q2"),
            h2(HTML("<b> Wines | Cars Analysis </b>")),
            h3("Perform a Principal Component Analysis on the cars data set (on the set of 5 quantitative variables)."),
            fluidPage(
                fluidRow(

                )
            )
    )
}

# Server

hw3_q2_server <- function(input, output, session, wines, cars) {



}