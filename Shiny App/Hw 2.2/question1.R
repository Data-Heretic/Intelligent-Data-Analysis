###################################################
##########         Question 1        ##############
###################################################

# UI

hw2.2_q1_ui <- function(id) {
    ns <- NS(id)
    tabPanel(title = "Question 1",
        column(12,
            h2(hw2.2_title),
            h3("Estimated odds ratio and confidence intervals of crossing for car vs. truck at each traffic location."),
            fluidRow(
                box(h4("Table of Action by Vehicle given Traffic, Traffic = Low."),
                    p("Odds ratio > 1"),
                    verbatimTextOutput(ns("oddsratio.traffic_low"))),
                box(h4("Table of Action by Vehicle given Traffic, Traffic = High."),
                    p("Odds ratio < 1"),
                    verbatimTextOutput(ns("oddsratio.traffic_high")))),
            fluidRow(
                box(width = 12, h3("Interpretations and fourfold display.")),
                box(plotOutput(ns("plot.podds"))),
                box(p("The odds ratio is greater than 1 for low traffic which means that the odds of crossing for cars are higher than the odds for trucks."),
                    p("But in case of high traffic, since the confidence interval contains the value 1, we are going to assume that the odds ratio of crossing for cars is comparable to the odds ratio of crossing for trucks."),
                    verbatimTextOutput(ns("confint.podds")))),
            fluidRow(
                box(p("In the following fourfold display, we can see a graphical representation for the odds ratio of each traffic level. It includes confidence rings. If they don't overlap, that indicates an association between crossing and cars, every odds ration is significantly different from 0. In the display we can see a strong positive association for low traffic (5.286842). The rings don't overlap and the non-principal diagonal sectors have less area than the principal diagonal ones (odds ratio greater than 1). On the other hand the odds of crossing is essentially identical for cars and trucks in case of high traffic (odds ration almost 1)."),
                    p("This result is an example of Simpson's paradox.")),
                box(class = "hw2_2_q2_fourfold", plotOutput(ns("plot.fourfold.partial")))),
            fluidRow(
                box(width = 12, h3("Partial tables interpretations")),
                box(width = 12, p("From the mosaic plot below We see that there is no systematic association between different actions and the type of vehicle - except among the action of trucks under low traffic. The tiles show that there are relatively less trucks in low traffic with crossing that the hypothesis of independence would predict."),
                    plotOutput(ns("plot.mosaic.partial"))))))
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

    output$plot.mosaic.partial <- renderPlot({
        req(Elks.partial)
        mosaic(Elks.partial, shade = T, split_vertical = TRUE)
    })
}