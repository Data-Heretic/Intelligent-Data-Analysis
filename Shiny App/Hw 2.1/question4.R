###################################################
##########         Question 4        ##############
###################################################

# UI

hw2.1_q4_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Question 4",
        column(10,
            h2(hw2.1_title),
            h4("Description."),
            fluidPage(

            )
        ),
        column(2, box(width = 12, class = 'well box-options', options))
    )
}

# Server

hw2.1_q4_server <- function(input, output, session) {

}