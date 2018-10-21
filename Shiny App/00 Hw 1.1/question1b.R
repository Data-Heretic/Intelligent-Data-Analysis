###################################################
##########         Question 1b       ##############
###################################################

# UI

hw1_q1b_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Q1b"),
            h3("What about for highway mileage?"),
            div(HTML("<ol start='1'><li> Boxplot and density plot of Highway Mileage vs type of transmision. <br>
                     The analysis is done with a new variable that differs between automatic and manual cars. <br>
                     The objective is to determine whether the mean between both populations is equal <br></li></ol>")),
            fluidPage(
              fluidRow(box(plotOutput(ns("plot_hwy"))),
                       box(plotOutput(ns("plot2_hwy")),
                           selectInput(ns("XPlot1_hwy"), "Logarithmic or normal variable?", c("Original Data" = "hwy", "Log Data" = "log(hwy)")))
              ),
              div(HTML("<ol start='2'><li> In order to test if there is a difference between the means, we propose to use One-Way ANOVA. <br>
                       Before we test the assumptions of normality of the errors and Homoscedasticity with Jarque bera and Levene tests, respectively.<br>
                       Finally we use One-Way ANOVA if the test assumptions are met otherwsise we apply the non-parametric Kruskall-Wallis test. <br>
                       There is also an option of performing the whole process removing the outliers from the Highway Mileage Data. <br>
                       We can Conclude by using the log of the data that there is a difference <br></li></ol> ")),
              fluidRow(box(plotOutput(ns("Errors2"))), box(verbatimTextOutput(ns("Errors2_jar")), verbatimTextOutput(ns("Errors2_Lev")))),
              fluidRow(
                verbatimTextOutput(ns("text21")),
                verbatimTextOutput(ns("text22"))
              )
            )
    )
}

# Server

hw1_q1b_server <- function(input, output, session, data, dataM) {

    # plot_hwy
    output$plot_hwy <- renderPlot({
        ggplot(data = data(), aes(x = tr, y = hwy, fill = tr)) +
        geom_boxplot() +
        labs(fill = "Miles", x = "Type of transmission", y = "Miles per gallon") +
        ggtitle("Highway Mileage") +
        theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()
    })

    # plot2_hwy
    output$plot2_hwy <- renderPlot({
        ggplot(data = data(),
             aes(x = eval(parse(text = input$XPlot1_hwy)), fill = tr, colour = tr)) +
        geom_density(alpha = 0.2) + theme_minimal() + xlab(if (input$XPlot1_hwy == "hwy") { "Highway Mileage" } else { "log(Highway Mileage)" })
    })

    # Errors2_jar
    output$Errors2_jar <- renderPrint({
        c <- if (input$XPlot1_hwy == "hwy") { jarque.bera.test(data()$hwy) }
        else { jarque.bera.test(log(data()$hwy)) }
        return(c)
    })

    # Errors2
    output$Errors2 <- renderPlot({
        c <- if (input$XPlot1_hwy == "hwy") {
            qqnorm(data()$hwy - mean(data()$hwy), main = "QQ-Plot of Errors Highway Mileage")
            qqline(data()$hwy - mean(data()$hwy))
        }
        else {
            qqnorm(log(data()$hwy) - mean(log(data()$hwy)), main = "QQ-Plot of Errors of log(City Mileage)")
            qqline(log(data()$hwy) - mean(log(data()$hwy)))
        }
        return(c)
    })

    # Errors2_Lev
    output$Errors2_Lev <- renderPrint({
        c <- if (input$XPlot1_hwy == "hwy") { leveneTest(hwy ~ tr, data = data()) }
        else { leveneTest(log(hwy) ~ tr, data = data()) }
        return(c)
    })

    # text21
    output$text21 <- renderPrint({
        c <- if (input$XPlot1_hwy == "hwy") { summary(aov(hwy ~ tr, data())) }
        else { summary(aov(log(hwy) ~ tr, data())) }
        return(c)
    })

    # text22
    output$text22 <- renderPrint({
        c <- if (input$XPlot1_hwy == "hwy") { kruskal.test(hwy ~ as.factor(tr), data = data()) }
        else { kruskal.test(log(hwy) ~ as.factor(tr), data = data()) }
        return(c)
    })
}