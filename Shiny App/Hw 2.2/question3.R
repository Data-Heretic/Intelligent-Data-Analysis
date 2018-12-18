###################################################
##########         Question 3        ##############
###################################################

# UI

hw2.2_q3_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Question 3",
        column(10,
            h2(hw2.2_title),
            h4("Test the homogeneous association between X and Y controlling for Z."),
            fluidPage(

            )
        ),
        column(2, box(width = 12, class = 'well box-options', options))
    )
}

# Server

hw2.2_q3_server <- function(input, output, session) {

}