###################################################
##########         Question 1        ##############
###################################################

# UI

hw3_q1_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Question 1",
        column(10,
            h2(hw3_title),
            h4("See you in January."),
            fluidPage(

            )
        ),
        column(2, box(width = 12, class = 'well box-options', options))
    )
}

# Server

hw3_q1_server <- function(input, output, session) {

}