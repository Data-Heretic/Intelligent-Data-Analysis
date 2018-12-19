###################################################
##########         Question 3        ##############
###################################################

# UI

hw2.2_q3_ui <- function(id) {
    ns <- NS(id)
    tabPanel(title = "Question 3",
        column(12,
            h2(hw2.2_title),
            h3("Test the homogeneous association between X and Y controlling for Z."),
            fluidRow(
                box(width = 12,
                    h4("Let’s test the hypothesis of homogeneous association (or homogeneity of odds ratio)."),
                    verbatimTextOutput(ns("test.woolf.partial")),
                    p("The very low p-value (0.0001) indicates that we can reject the Homogeneity of the Odds Ratio through the two levels of traffic. We cannot assume that the conditional relationship between any pair of variables given the third one is the same at each level of the third variable.")))))
}

# Server

hw2.2_q3_server <- function(input, output, session) {

    output$test.woolf.partial <- renderPrint({
        req(Elks.partial)
        woolf_test(Elks.partial)
    })
}