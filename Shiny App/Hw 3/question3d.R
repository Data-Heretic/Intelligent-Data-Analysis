###################################################
##########         Question 3d        ##############
###################################################

# UI

hw3_q3d_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Section d)",
        column(10,
            h2("See you in January."),
            h4("Can't wait..."),
            fluidPage(

            )
        ),
        column(2, box(width = 12, class = 'well box-options', options))
    )
}

# Server

hw3_q3d_server <- function(input, output, session) {

}