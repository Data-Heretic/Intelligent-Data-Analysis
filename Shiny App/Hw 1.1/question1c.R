###################################################
##########         Question 1c       ##############
###################################################

# UI

hw1.1_q1c_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Section c)",
          column(10,
                h3("What about engine displacement in litres?"),
                div(HTML("<ol start='1'><li> Boxplot and density plot of Engine displacement vs type of transmision. <br>
                         The analysis is done with a new variable that differs between automatic and manual cars. <br>
                         The objective is to determine whether the mean between both populations is equal <br></li></ol>")),
                fluidPage(fluidRow(box(plotOutput(ns("plot.box.displ"))),
                                   box(plotOutput(ns("plot.density.displ")))
                ),
                div(HTML("<ol start='2'><li> In order to test if there is a difference between the means, we propose to use One-Way ANOVA. <br>
                         Before we test the assumptions of normality of the errors and Homoscedasticity with Jarque bera and Levene tests, respectively.<br>
                         Finally we use One-Way ANOVA if the test assumptions are met otherwsise we apply the non-parametric Kruskall-Wallis test. <br>
                         There is also an option of performing the whole process removing the outliers from the Engine Displacement Data. <br>
                         We can Conclude by using the log of the data that there is a difference <br></li></ol> ")),
                fluidRow(box(plotOutput(ns("plot.qq.displ_errors"))),
                         box(verbatimTextOutput(ns("test.jarque_bera.displ")), verbatimTextOutput(ns("test.levene.displ")))),
                fluidRow(
                  verbatimTextOutput(ns("summary.aov.displ")),
                  verbatimTextOutput(ns("test.kruskal.displ"))
                )
              )
          ),
          column(2, box(width = 12, class = 'well box-options',
            options, selectInput(ns("displ_variable_type"), "Logarithmic or normal variable?", c("Original Data" = "displ", "Log Data" = "log(displ)"))))
    )
}

# Server

hw1.1_q1c_server <- function(input, output, session, data, dataM) {

    # Plot: Boxplot displ
    output$plot.box.displ <- renderPlot({
        ggplot(data(), aes(x = tr, y = displ, fill = tr)) +
            geom_boxplot() +
            labs(fill = "Litres", x = "Type of transmission", y = "Litres") +
            ggtitle("Engine Displacement") +
            theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()
    })

    # Plot: Density displ
    output$plot.density.displ <- renderPlot({
        ggplot(data(),
             aes(x = eval(parse(text = input$displ_variable_type)), fill = tr, colour = tr)) +
        geom_density(alpha = .3, bw = 0.4) + theme_minimal() + xlab(if (input$displ_variable_type == "displ") { "Engine Displacement" } else { "log(Engine Displacement)" })
    })

    # Plot: QQplot displ errors
    output$plot.qq.displ_errors <- renderPlot({
        c <- if (input$displ_variable_type == "displ") {
            qqnorm(data()$displ - mean(data()$displ), main = "QQ-Plot of Errors Engine Displacement")
            qqline(data()$displ - mean(data()$displ))
        }
        else {
            qqnorm(log(data()$displ) - mean(log(data()$displ)), main = "QQ-Plot of Errors of log(Engine Displacement)")
            qqline(log(data()$displ) - mean(log(data()$displ)))
        }
        return(c)
    })

    # Test: Jarque bera displ
    output$test.jarque_bera.displ <- renderPrint({
        c <- if (input$displ_variable_type == "displ") { jarque.bera.test(data()$displ) }
        else { jarque.bera.test(log(data()$displ)) }
        return(c)

    })

    # Test: Levene displ
    output$test.levene.displ <- renderPrint({
        c <- if (input$displ_variable_type == "displ") { leveneTest(displ ~ tr, data = data()) }
        else { leveneTest(log(displ) ~ tr, data = data()) }
        return(c)
    })

    # Summary: AOV displ
    output$summary.aov.displ <- renderPrint({
        c <- if (input$displ_variable_type == "displ") { summary(aov(displ ~ tr, data())) }
        else { summary(aov(log(displ) ~ tr, data())) }
        return(c)
    })

    # Test: Kruskal displ
    output$test.kruskal.displ <- renderPrint({
        c <- if (input$displ_variable_type == "displ") { kruskal.test(displ ~ as.factor(tr), data = data()) }
        else { kruskal.test(log(displ) ~ as.factor(tr), data = data()) }
        return(c)
    })
}