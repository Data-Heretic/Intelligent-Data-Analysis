###################################################
##########         Question 2        ##############
###################################################

# UI

hw2.2_q2_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Question 2",
        column(10,
            h2(hw2.2_title),
            h4("Estimated odds ratio of crossing vs. retreat without taking into account the third (control) variable."),
            fluidPage(

            )
        ),
        column(2, box(width = 12, class = 'well box-options', options))
    )
}

# Server

hw2.2_q2_server <- function(input, output, session) {

}