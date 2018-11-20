###################################################
##########         Question 3a        ##############
###################################################

# UI

hw2.1_q3a_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Q3a"),
            h2(HTML("<b> Caterage </b>")),
            h4("Description."),
            fluidPage(

            )
    )
}

# Server

hw2.1_q3a_server <- function(input, output, session) {

}