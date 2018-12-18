###################################################
##########         Question 3a        ##############
###################################################

# UI

hw3_q3a_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Section a)",
        column(10,
            h2(hw3_title),
            h4("Can't wait..."),
            fluidPage(

            )
        ),
        column(2, box(width = 12, class = 'well box-options', options))
    )
}

# Server

hw3_q3a_server <- function(input, output, session) {

}