###################################################
##########         Question 3b        ##############
###################################################

# UI

hw2.1_q3b_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Section b)",
        column(10,
            h4("Description."),
            fluidPage(

            )
        ),
        column(2, box(width = 12, class = 'well box-options', options))
    )
}

# Server

hw2.1_q3b_server <- function(input, output, session) {

}