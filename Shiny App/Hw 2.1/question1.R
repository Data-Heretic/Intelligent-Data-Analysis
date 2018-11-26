###################################################
##########         Question 1        ##############
###################################################

# UI

hw2.1_q1_ui <- function(id) {
    ns <- NS(id)
    tabPanel(title = "Question 1",
        column(10,
            h2(hw2.1_title),
            h4("Description."),
            fluidPage(
                
            )
        ),
        column(2, box(width = 12, class = 'well box-options', h5("Options"), "Nothing yet"))
    )
}

# Server

hw2.1_q1_server <- function(input, output, session) {

}