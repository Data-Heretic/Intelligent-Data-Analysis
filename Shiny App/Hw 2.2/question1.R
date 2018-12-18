###################################################
##########         Question 1        ##############
###################################################

# UI

hw2.2_q1_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Question 1",
        column(10,
            h2(hw2.2_title),
            h4("Estimated odds ratio and confidence intervals of crossing for car vs. truck at each traffic location."),
            fluidPage(

            )
        ),
        column(2, box(width = 12, class = 'well box-options', options))
    )
}

# Server

hw2.2_q1_server <- function(input, output, session) {

}