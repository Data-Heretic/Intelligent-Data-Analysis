###################################################
##########         Question 1        ##############
###################################################

# UI

hw2.2_q1_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Question 1",
        column(10,
            h2(hw2.2_title),
            h3("Estimated odds ratio and confidence intervals of crossing for car vs. truck at each traffic location."),
            fluidRow(
                box(h4("Table of Action by Vehicle given Traffic, Traffic = Low."),
                    p("Odds ratio > 1"),
                    verbatimTextOutput(ns("oddsratio.traffic_low"))),
                box(h4("Table of Action by Vehicle given Traffic, Traffic = High."),
                    p("Odds ratio < 1"),
                    verbatimTextOutput(ns("oddsratio.traffic_high"))))),
        column(2, box(width = 12, class = 'well box-options', options)))
}

# Server

hw2.2_q1_server <- function(input, output, session) {

    output$oddsratio.traffic_low <- renderPrint({
        req(Elks.partial)
        oddsratio(Elks.partial[,, 1], log = FALSE)
    })

    output$oddsratio.traffic_high <- renderPrint({
        req(Elks.partial)
        oddsratio(Elks.partial[,, 2], log = FALSE)
    })
}