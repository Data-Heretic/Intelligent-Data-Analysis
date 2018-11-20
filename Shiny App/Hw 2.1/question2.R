###################################################
##########         Question 2        ##############
###################################################

# UI

hw2.1_q2_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Q2"),
            h2(HTML("<b> Caterage </b>")),
            h4("Description."),
            fluidPage(

            )
    )
}

# Server

hw2.1_q2_server <- function(input, output, session) {

}