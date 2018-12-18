###################################################
##########         Question 3h        ##############
###################################################

# UI

hw3_q3h_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Section h)",
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

hw3_q3h_server <- function(input, output, session) {

}