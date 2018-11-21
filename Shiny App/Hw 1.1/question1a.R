###################################################
##########         Question 1a       ##############
###################################################

# UI

hw1.1_q1a_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Section a)",
        fluidRow(
            column(10,
                h3("Is there a significant difference in city mileage between automatic and manual transmission cars?"),
                h4("Boxplot and density plot of City Mileage vs type of transmision."),
                p("The analysis is done with a new variable that differs between automatic and manual cars.
                   The objective is to determine whether the mean between both populations is equal."),
                fluidRow(
                    box(plotOutput(ns("plot.box.cty"))),
                    box(plotOutput(ns("plot.density.cty")))),
                h4("In order to test if there is a difference between the means, we propose to use One-Way ANOVA."),
                p("Before we test the assumptions of normality of the errors and Homoscedasticity with Jarque bera and Levene tests, respectively.
                   Finally we use One-Way ANOVA if the test assumptions are met otherwsise we apply the non-parametric Kruskall-Wallis test.
                   There is also an option of performing the whole process removing the outliers from the City Mileage Data.
                   We can Conclude by using the log of the data that there is a difference."),
                fluidRow(
                    box(plotOutput(ns("plot.qq.cty_errors"))),
                    box(verbatimTextOutput(ns("test.jarque_bera.cty")), verbatimTextOutput(ns("test.levene.cty")))),
                fluidRow(
                    box(width = 6, verbatimTextOutput(ns("summary.aov.cty"))),
                    box(width = 6, verbatimTextOutput(ns("test.kruskal.cty")))
                )
            ),
            column(2, box(width = 12, class = 'well box-options', options,
                selectInput(ns("cty_variable_type"), "Logarithmic or normal variable?", c("Original Data" = "cty", "Log Data" = "log(cty)"))))
            )
        )
}

# Server

hw1.1_q1a_server <- function(input, output, session, data, dataM) {

    # Plot: Boxplot cty
    output$plot.box.cty <- renderPlot({
        ggplot(data = data(), aes(x = tr, y = cty, fill = tr)) +
        geom_boxplot() +
        labs(fill = "Miles", x = "Type of transmission", y = "Miles per gallon") +
        ggtitle("City Mileage") +
        theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()
    })

    # Plot: Density cty
    output$plot.density.cty <- renderPlot({
        ggplot(data = data(),
             aes(x = eval(parse(text = input$cty_variable_type)), fill = tr, colour = tr)) +
             geom_density(alpha = 0.2) + theme_minimal() + xlab(if (input$cty_variable_type == "cty") { "City Mileage" } else { "log(City Mileage)" })
    })

    # Plot: QQplot 
    output$plot.qq.cty_errors <- renderPlot({
        c <- if (input$cty_variable_type == "cty") {
            qqnorm(data()$cty - mean(data()$cty), main = "QQ-Plot of Errors City Mileage")
            qqline(data()$cty - mean(data()$cty))
        }
        else {
            qqnorm(log(data()$cty) - mean(log(data()$cty)), main = "QQ-Plot of Errors of log(City Mileage)")
            qqline(log(data()$cty) - mean(log(data()$cty)))
        }
        return(c)
    })

    # Test: Jarque bera cty
    output$test.jarque_bera.cty <- renderPrint({
        c <- if (input$cty_variable_type == "cty") { jarque.bera.test(data()$cty) }
        else { jarque.bera.test(log(data()$cty)) }
        return(c)
    })

    # Test: Levene cty
    output$test.levene.cty <- renderPrint({
        c <- if (input$cty_variable_type == "cty") { leveneTest(cty ~ tr, data = data()) }
        else { leveneTest(log(cty) ~ tr, data = data()) }
        return(c)
    })

    # Summary: Aov cty
    output$summary.aov.cty <- renderPrint({
        c <- if (input$cty_variable_type == "cty") { summary(aov(cty ~ tr, data())) }
        else { summary(aov(log(cty) ~ tr, data())) }
        return(c)
    })

    # Test: Kruskal cty
    output$test.kruskal.cty <- renderPrint({
        c <- if (input$cty_variable_type == "cty") { kruskal.test(cty ~ as.factor(tr), data = data()) }
        else { kruskal.test(log(cty) ~ as.factor(tr), data = data()) }
        return(c)
    })
}