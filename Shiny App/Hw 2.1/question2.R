###################################################
##########         Question 2        ##############
###################################################

# UI

hw2.1_q2_ui <- function(id) {
    ns <- NS(id)
    tabPanel(title = "Question 2",
        column(12,
            h2(hw2.1_title),
            h4("Description."),
            fluidPage(

            )))
}

# Server

hw2.1_q2_server <- function(input, output, session) {

}