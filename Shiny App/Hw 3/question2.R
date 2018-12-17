###################################################
##########         Question 2        ##############
###################################################

# UI

hw3_q2_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Question 2",
        column(10,
            h2(hw3_title),
            h4("Description."),
            fluidPage(

            )
        ),
        column(2, box(width = 12, class = 'well box-options', options))
    )
}

# Server

hw3_q2_server <- function(input, output, session) {

}