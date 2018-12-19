###################################################
##########         Question 3c        ##############
###################################################

# UI

hw3_q3c_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Section c)",
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

hw3_q3c_server <- function(input, output, session) {

}