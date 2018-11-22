###################################################
##########         Question 3a        ##############
###################################################

# UI

hw2.1_q3a_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Section a)",
        fluidRow(
            column(10,
                h4("Description."),
                fluidPage(

                )
            ),
            column(2, box(width = 12, class = 'well box-options', options))
        )
    )
}

# Server

hw2.1_q3a_server <- function(input, output, session) {

}