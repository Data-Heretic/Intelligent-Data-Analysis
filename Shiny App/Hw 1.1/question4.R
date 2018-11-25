###################################################
##########         Question 4        ##############
###################################################

# UI

hw1.1_q4_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Question 4",
        column(10,
            h2(hw1.1_title),
            h3("Proposed Question: Is there any difference in city and highway data between manufacturers?"),
            p("For this question we have used a double box plot to show how the different manufacturers engines behave in terms of consumption for City driving and highway driving."),
            fluidRow(plotOutput(ns("plot.box.dataM"))),
            h4("In order to test if there is a difference between the means, we propose to use One-Way ANOVA."),
            p("Before we apply it we have to test the assumptions of normality of the errors and Homoscedasticity taking advantage of Jarque bera and Levene tests, respectively.
                If the test assumptions are met we apply One-Way ANOVA, otherwsise we apply the non-parametric Kruskall-Wallis test.
                In case One-Way ANOVA finds significant results we then run Tukey's HSD, a post-hoc test based on the studentized range distribution, to find out which specific groups' means (compared with each other) are different. There is also an option of performing the whole process removing the outliers from the respective Data."),
            fluidRow(
                box(plotOutput(ns("plot.qq.cty"))),
                box(verbatimTextOutput(ns("test.jarque_bera.selection_3")), verbatimTextOutput(ns("test.levene.selection_3")))),
            fluidRow(
                box(verbatimTextOutput(ns("summary.aov.selection_3")), verbatimTextOutput(ns("test.kruskal.selection_3"))),
                box(verbatimTextOutput(ns("test.tukeyhsd.selection_3")))
            )
        ),
        column(2, box(width = 12, class = 'well box-options', options,
            selectInput(ns("selection_3"), "Between which variables do you want to perform a statistical test?",
                    choices = list("City Mileage ~ Manufacturer" = 1, "Highway Mileage ~ Manufacturer" = 2), selected = 1)))
    )
}

# Server

hw1.1_q4_server <- function(input, output, session, data, dataM) {

    # Plot: Boxplot dataM
    output$plot.box.dataM <- renderPlot({
        ggplot(dataM(), aes(x = variable, y = value, fill = manufacturer)) +
            geom_boxplot() +
            labs(fill = "Miles", x = "Manufacturer", y = "Miles per gallon") +
            ggtitle("Manufacturer influence") +
            theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()
    })

    # Plot: QQplot cty
    output$plot.qq.cty <- renderPlot({
        c <- if (input$selection_3 == 1) {
            qqnorm(data()$cty - mean(data()$cty), main = "QQ-Plot of Errors City Mileage")
            qqline(data()$cty - mean(data()$cty))
        }
        else if (input$selection_3 == 2) {
            qqnorm(data()$hwy - mean(data()$hwy), main = "QQ-Plot of Errors Highway Mileage")
            qqline(data()$hwy - mean(data()$hwy))
        }
        return(c)
    })

    # Test: Jarque bera selection
    output$test.jarque_bera.selection_3 <- renderPrint({
        c <- if (input$selection_3 == 1) { jarque.bera.test(data()$cty) }
        else if (input$selection_3 == 2) { jarque.bera.test(data()$hwy) }
        return(c)
    })

    # Test: Levene selection
    output$test.levene.selection_3 <- renderPrint({
        c <- if (input$selection_3 == 1) { leveneTest(cty ~ manufacturer, data = data()) }
        else if (input$selection_3 == 2) { leveneTest(hwy ~ manufacturer, data = data()) }
        return(c)
    })

    # Summary: AOV selection_3
    output$summary.aov.selection_3 <- renderPrint({
        c <- if (input$selection_3 == 1) { summary(aov(cty ~ as.factor(manufacturer), data())) }
        else if (input$selection_3 == 2) { summary(aov(hwy ~ as.factor(manufacturer), data())) }
        return(c)
    })

    # Test: TukeyHSD selection_3
    output$test.tukeyhsd.selection_3 <- renderPrint({
        c <- if (input$selection_3 == 1) { TukeyHSD(aov(cty ~ as.factor(manufacturer), data())) }
        else if (input$selection_3 == 2) { TukeyHSD(aov(hwy ~ as.factor(manufacturer), data())) }
        return(c)
    })

    # Test: Kruskal selection_3
    output$test.kruskal.selection_3 <- renderPrint({
        c <- if (input$selection_3 == 1) { kruskal.test(cty ~ as.factor(manufacturer), data = data()) }
        else if (input$selection_3 == 2) { kruskal.test(hwy ~ as.factor(manufacturer), data = data()) }
        return(c)
    })
}