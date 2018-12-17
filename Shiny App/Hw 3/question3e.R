###################################################
##########         Question 3e        ##############
###################################################

# UI

hw3_q3e_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Section e)",
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

hw3_q3e_server <- function(input, output, session) {

}