###################################################
##########         Question 1a       ##############
###################################################

# UI

hw1_q1a_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Q1a"),
            h3("Is there a significant difference in city mileage between automatic and manual transmission cars?"),
            div(HTML("<ol start='1'><li> Boxplot and density plot of City Mileage vs type of transmision. <br>
                     The analysis is done with a new variable that differs between automatic and manual cars. <br>
                     The objective is to determine whether the mean between both populations is equal <br></li></ol>")),
            fluidPage(
              fluidRow(box(plotOutput(ns("plot_cty"))),
                       box(plotOutput(ns("plot2_cty")),
                           selectInput(ns("XPlot1_cty"), "Logarithmic or normal variable?", c("Original Data" = "cty", "Log Data" = "log(cty)"))
                       )),
              div(HTML("<ol start='2'><li> In order to test if there is a difference between the means, we propose to use One-Way ANOVA. <br>
                       Before we test the assumptions of normality of the errors and Homoscedasticity with Jarque bera and Levene tests, respectively.<br>
                       Finally we use One-Way ANOVA if the test assumptions are met otherwsise we apply the non-parametric Kruskall-Wallis test. <br>
                       There is also an option of performing the whole process removing the outliers from the City Mileage Data. <br>
                       We can Conclude by using the log of the data that there is a difference <br></li></ol> ")),
              fluidRow(box(plotOutput(ns("Errors1"))),
                       box(verbatimTextOutput(ns("Errors1_jar")), verbatimTextOutput(ns("Errors1_Lev")))),
              fluidRow(
                verbatimTextOutput(ns("text1")),
                verbatimTextOutput(ns("text2"))
              )
            )
        )
}

# Server

hw1_q1a_server <- function(input, output, session, data, dataM) {

    # plot_cty
    output$plot_cty <- renderPlot({
        ggplot(data = data(), aes(x = tr, y = cty, fill = tr)) +
        geom_boxplot() +
        labs(fill = "Miles", x = "Type of transmission", y = "Miles per gallon") +
        ggtitle("City Mileage") +
        theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()
    })

    # plot2_cty
    output$plot2_cty <- renderPlot({
        ggplot(data = data(),
             aes(x = eval(parse(text = input$XPlot1_cty)), fill = tr, colour = tr)) +
        geom_density(alpha = 0.2) + theme_minimal() + xlab(if (input$XPlot1_cty == "cty") { "City Mileage" } else { "log(City Mileage)" })
    })

    # Errors1_jar
    output$Errors1_jar <- renderPrint({
        c <- if (input$XPlot1_cty == "cty") { jarque.bera.test(data()$cty) }
        else { jarque.bera.test(log(data()$cty)) }
        return(c)
    })

    # Errors1
    output$Errors1 <- renderPlot({
        c <- if (input$XPlot1_cty == "cty") {
            qqnorm(data()$cty - mean(data()$cty), main = "QQ-Plot of Errors City Mileage")
            qqline(data()$cty - mean(data()$cty))
        }
        else {
            qqnorm(log(data()$cty) - mean(log(data()$cty)), main = "QQ-Plot of Errors of log(City Mileage)")
            qqline(log(data()$cty) - mean(log(data()$cty)))
        }
        return(c)
    })

    # Errors1_Lev
    output$Errors1_Lev <- renderPrint({
        c <- if (input$XPlot1_cty == "cty") { leveneTest(cty ~ tr, data = data()) }
        else { leveneTest(log(cty) ~ tr, data = data()) }
        return(c)
    })

    # text1
    output$text1 <- renderPrint({
        c <- if (input$XPlot1_cty == "cty") { summary(aov(cty ~ tr, data())) }
        else { summary(aov(log(cty) ~ tr, data())) }
        return(c)
    })

    # text2
    output$text2 <- renderPrint({
        c <- if (input$XPlot1_cty == "cty") { kruskal.test(cty ~ as.factor(tr), data = data()) }
        else { kruskal.test(log(cty) ~ as.factor(tr), data = data()) }
        return(c)
    })
}