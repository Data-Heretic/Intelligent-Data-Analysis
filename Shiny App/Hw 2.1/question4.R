###################################################
##########         Question 4        ##############
###################################################

# UI

hw2.1_q4_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Q4"),
            h2(HTML("<b> Caterage </b>")),
            h4("Description."),
            fluidPage(

            )
    )
}

# Server

hw2.1_q4_server <- function(input, output, session) {

}