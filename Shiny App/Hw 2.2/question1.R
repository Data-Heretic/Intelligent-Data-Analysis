###################################################
##########         Question 1        ##############
###################################################

# UI

hw2.2_q1_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Q1"),
            h2(HTML("<b> Caterage </b>")),
            h4("Description."),
            fluidPage(

            )
    )
}

# Server

hw2.2_q1_server <- function(input, output, session) {

}