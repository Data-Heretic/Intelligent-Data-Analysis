###################################################
##########         Question 2        ##############
###################################################

# UI

hw2.2_q2_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Question 2",
        column(10,
            h2(hw2.2_title),
            h3("Estimated odds ratio of crossing vs. retreat without taking into account the third (control) variable."),
            fluidRow(
                box(width = 6,
                    h4("Let's estimate the odds ratio of crossing vs. retreat without taking into account the control variable."),
                    p("The odds of “success” (crossing) for car vehicles are higher than for trucks."),
                    verbatimTextOutput(ns("oddsratio.marginal")),
                    p("Taking into account (controlling for) traffic, the odds of crossing are lower for car vehicles than for trucks in case of high traffic. Just the reverse direction that the marginal table showed."),
                    p("Simpson’s Paradox: The result that a marginal association can have a different direction from each conditional association."),
                    p("Moral: It can be dangerous to “collapse” contingency tables over a third control variable.")),
                box(width = 6,
                    h4("The odds ratio is greater than 1 for low traffic which means that the odds of crossing for cars are higher than the odds for trucks."),
                    p("But in case of high traffic the odds ratio is less than 1 which means that the odds of crossing for cars are lower than the odds for trucks."),
                    verbatimTextOutput(ns("confint.podds")),
                    plotOutput(ns("plot.podds")))),
            fluidRow(
                box(width = 4,
                    p("In the following fourfold display, we can see a graphical representation for the odds ratio of each traffic level. It includes confidence rings. If they don't overlap, that indicates an association between crossing and cars, every odds ration is significantly different from 0. In the display we can see a strong positive association for low traffic (5.286842). The rings don’t overlap and the non-principal diagonal sectors have less area than the principal diagonal ones (odds ratio greater than 1). On the other hand the odds of crossing is essentially identical for cars and trucks in case of high traffic (odds ration almost 1)."),
                    p("This result is an example of Simpson's paradox."),
                    plotOutput(ns("plot.fourfold.partial"))),
                box(width = 4,
                    p("In the Elks marginal table, where the odds ratio was 2.676251, we observe a strong positive association (odds ratio greater than 1)."),
                    plotOutput(ns("plot.fourfold.marginal"))),
                box(width = 4,
                    p("From the mosaic plot below We see that there is no systematic association between different actions and the type of vehicle - except among the action of trucks under low traffic. The tiles show that there are relatively less trucks in low traffic with crossing that the hypothesis of independence would predict."),
                    plotOutput(ns("plot.mosaic.partial"))))),
        column(2, box(width = 12, class = 'well box-options', options))
    )
}

# Server

hw2.2_q2_server <- function(input, output, session) {

    output$oddsratio.marginal <- renderPrint({
        req(Elks.marginal)
        oddsratio(Elks.marginal, log = FALSE)
    })

    output$confint.podds <- renderPrint({
        req(Elks.podds)
        confint(Elks.podds, log = FALSE)
    })

    output$plot.podds <- renderPlot({
        req(Elks.podds)
        plot(Elks.podds)
    })

    output$plot.fourfold.partial <- renderPlot({
        req(Elks.partial)
        fourfold(Elks.partial)
    })

    output$plot.fourfold.marginal <- renderPlot({
        req(Elks.marginal)
        fourfold(Elks.marginal)
    })

    output$plot.mosaic.partial <- renderPlot({
        req(Elks.partial)
        mosaic(Elks.partial, shade = T, split_vertical = TRUE)
    })
}