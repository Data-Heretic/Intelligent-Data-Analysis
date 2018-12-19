###################################################
##########         Question 4        ##############
###################################################

# UI

hw2.2_q4_ui <- function(id) {
    ns <- NS(id)
    tabPanel(title = "Question 4",
        column(12,
            h2(hw2.2_title),
            h3("Are X and Y conditional independent given Z?."),
            fluidRow(
                box(width = 12,
                    h4("Let's test the hypothesis of conditional independence"),
                    verbatimTextOutput(ns("test.mantelhaen.partial")),
                    p("The very low p-value (7.868e-07) indicates that we can reject the conditional independence of the Odds Ratio through the two levels of traffic.")))))
}

# Server

hw2.2_q4_server <- function(input, output, session) {

    output$test.mantelhaen.partial <- renderPrint({
        req(Elks.partial)
        mantelhaen.test(Elks.partial)
    })
}