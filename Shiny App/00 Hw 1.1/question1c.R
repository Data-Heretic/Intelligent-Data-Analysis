###################################################
##########         Question 1c       ##############
###################################################

# UI

hw1_q1c_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Q1c"),
            h3("What about engine displacement in litres?"),
            div(HTML("<ol start='1'><li> Boxplot and density plot of Engine displacement vs type of transmision. <br>
                     The analysis is done with a new variable that differs between automatic and manual cars. <br>
                     The objective is to determine whether the mean between both populations is equal <br></li></ol>")),
            fluidPage(fluidRow(box(plotOutput(ns("plot_displ"))),
                               box(plotOutput(ns("plot2_displ")),
                                   selectInput(ns("XPlot1_displ"), "Logarithmic or normal variable?", c("Original Data" = "displ", "Log Data" = "log(displ)")))
            ),
            div(HTML("<ol start='2'><li> In order to test if there is a difference between the means, we propose to use One-Way ANOVA. <br>
                     Before we test the assumptions of normality of the errors and Homoscedasticity with Jarque bera and Levene tests, respectively.<br>
                     Finally we use One-Way ANOVA if the test assumptions are met otherwsise we apply the non-parametric Kruskall-Wallis test. <br>
                     There is also an option of performing the whole process removing the outliers from the Engine Displacement Data. <br>
                     We can Conclude by using the log of the data that there is a difference <br></li></ol> ")),
            fluidRow(box(plotOutput(ns("Errors3"))),
                     box(verbatimTextOutput(ns("Errors3_jar")), verbatimTextOutput(ns("Errors3_Lev")))),
            fluidRow(
              verbatimTextOutput(ns("text31")),
              verbatimTextOutput(ns("text32"))
            )
          )
    )
}

# Server

hw1_q1c_server <- function(input, output, session, data, dataM) {

    # plot_displ
    output$plot_displ <- renderPlot({
        ggplot(data(), aes(x = tr, y = displ, fill = tr)) +
            geom_boxplot() +
            labs(fill = "Litres", x = "Type of transmission", y = "Litres") +
            ggtitle("Engine Displacement") +
            theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()
    })

    # plot2_displ
    output$plot2_displ <- renderPlot({
        ggplot(data(),
             aes(x = eval(parse(text = input$XPlot1_displ)), fill = tr, colour = tr)) +
        geom_density(alpha = .3, bw = 0.4) + theme_minimal() + xlab(if (input$XPlot1_displ == "displ") { "Engine Displacement" } else { "log(Engine Displacement)" })
    })

    # Errors3
    output$Errors3 <- renderPlot({
        c <- if (input$XPlot1_displ == "displ") {
            qqnorm(data()$displ - mean(data()$displ), main = "QQ-Plot of Errors Engine Displacement")
            qqline(data()$displ - mean(data()$displ))
        }
        else {
            qqnorm(log(data()$displ) - mean(log(data()$displ)), main = "QQ-Plot of Errors of log(Engine Displacement)")
            qqline(log(data()$displ) - mean(log(data()$displ)))
        }
        return(c)
    })

    # Errors3_jar
    output$Errors3_jar <- renderPrint({
        c <- if (input$XPlot1_displ == "displ") { jarque.bera.test(data()$displ) }
        else { jarque.bera.test(log(data()$displ)) }
        return(c)

    })

    # Errors3_Lev
    output$Errors3_Lev <- renderPrint({
        c <- if (input$XPlot1_displ == "displ") { leveneTest(displ ~ tr, data = data()) }
        else { leveneTest(log(displ) ~ tr, data = data()) }
        return(c)
    })

    # text31
    output$text31 <- renderPrint({
        c <- if (input$XPlot1_displ == "displ") { summary(aov(displ ~ tr, data())) }
        else { summary(aov(log(displ) ~ tr, data())) }
        return(c)
    })

    # text32
    output$text32 <- renderPrint({
        c <- if (input$XPlot1_displ == "displ") { kruskal.test(displ ~ as.factor(tr), data = data()) }
        else { kruskal.test(log(displ) ~ as.factor(tr), data = data()) }
        return(c)
    })
}