###################################################
##########         Question 3b        ##############
###################################################

# UI

hw2.1_q3b_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Q3b"),
            h2(HTML("<b> Caterage </b>")),
            h4("Description."),
            fluidPage(

            )
    )
}

# Server

hw2.1_q3b_server <- function(input, output, session) {

}