###################################################
##########         Question 3g        ##############
###################################################

# UI

hw3_q3g_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Section g)",
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

hw3_q3g_server <- function(input, output, session) {

}