###################################################
##########         Question 4        ##############
###################################################

# UI

hw2.1_q4_ui <- function(id) {
    ns <- NS(id)
    tabPanel(title = "Question 4",
        column(10,
            h2(hw2.1_title),
            h4("Summary and comparison of the two remedial actions."),
            fluidPage(
                h5("In both models we have a good R2 and a good p-value, the main difference in the models can be seen here:"),
                tags$ol(
                   tags$li("The linearity of residuals , model2 is more linear than model3"),
                   tags$li("Normality , model2 behaves better compared to model3"),
                   tags$li("Variance equality, model 3 is better than model2")),
                h5("Therefore we conclude that the first remedial is better.We add some additional info in the table below:"),
                tags$table(
                  tags$tr(
                    tags$th(""),
                    tags$th("Model 2"),
                    tags$th("Model 3")),
                  tags$tr(
                    tags$td("R^_{2}"),
                    tags$td("0.9952"),
                    tags$td("0.9723")),
                  tags$tr(
                    tags$td("Durbin-Watson test p-value (linearity)"),
                    tags$td("2.2e-16"),
                    tags$td("NA")),
                  tags$tr(
                    tags$td("Jarque Bera Test p-value"),
                    tags$td("0.1723"),
                    tags$td("0.01775")),
                  tags$tr(
                    tags$td("Breusch-Pagan test p-value"),
                    tags$td("0.0007143"),
                    tags$td("7.195e-06"))))
        ),
        column(2, box(width = 12, class = 'well box-options', h5("Options"), "Nothing yet"))
    )
}

# Server

hw2.1_q4_server <- function(input, output, session) {

}