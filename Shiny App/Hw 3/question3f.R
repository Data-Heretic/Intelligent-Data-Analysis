###################################################
##########         Question 3f        ##############
###################################################

# UI

hw3_q3f_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Section f)",
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

hw3_q3f_server <- function(input, output, session) {

}