###################################################
##########         Question 2        ##############
###################################################

# UI

hw2.2_q2_ui <- function(id) {
    ns <- NS(id)
    tabPanel(title = "Question 2",
        column(12,
            h2(hw2.2_title),
            h3("Estimated odds ratio of crossing vs. retreat without taking into account the third (control) variable."),
            fluidRow(
                box(width = 12, h4("Let's estimate the odds ratio of crossing vs. retreat without taking into account the control variable.")),
                box(p("The odds of 'success' (crossing) for car vehicles are higher than for trucks."),
                    verbatimTextOutput(ns("oddsratio.marginal")),
                    p("Taking into account (controlling for) traffic, the odds of crossing are lower for car vehicles than for trucks in case of high traffic. Just the reverse direction that the marginal table showed."),
                    p("Simpson's Paradox: The result that a marginal association can have a different direction from each conditional association."),
                    p("Moral: It can be dangerous to 'collapse' contingency tables over a third control variable.")),
                box(p("In the Elks marginal table, where the odds ratio was 2.676251, we observe a strong positive association (odds ratio greater than 1)."),
                    plotOutput(ns("plot.fourfold.marginal"))))))
}

# Server

hw2.2_q2_server <- function(input, output, session) {

    output$oddsratio.marginal <- renderPrint({
        req(Elks.marginal)
        oddsratio(Elks.marginal, log = FALSE)
    })

    output$plot.fourfold.marginal <- renderPlot({
        req(Elks.marginal)
        fourfold(Elks.marginal)
    })
}