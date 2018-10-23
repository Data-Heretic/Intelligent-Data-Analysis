###################################################
##########         Question 4        ##############
###################################################

# UI

hw1_q4_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Q4"),
            h2(HTML("<b> Mileage Analysis </b>")),
            h3("Proposed Question - 
               Is there any difference in city and highway data between manufacturers?"),
            h5("For this question we have used a double box plot to show how the different manufacturers engines behave in terms of consumption for City driving and highway driving."),
            fluidPage(
                  fluidRow(plotOutput(ns("proposed1"))),
                  fluidRow(
                    box(selectInput(ns("selectQ4"), h3("Between which variables do you want to perform a statistical test?"),
                                    choices = list("City Mileage ~ Manufacturer" = 1, "Highway Mileage ~ Manufacturer" = 2), selected = 1))
                  ),
                  div(HTML("In order to test if there is a difference between the means, we propose to use One-Way ANOVA. <br>
                           Before we apply it we have to test the assumptions of normality of the errors and Homoscedasticity taking advantage of Jarque bera and Levene tests, respectively.<br>
                           If the test assumptions are met we apply One-Way ANOVA, otherwsise we apply the non-parametric Kruskall-Wallis test. <br>
                           In case One-Way ANOVA finds significant results we then run Tukey&apos;s HSD, a post-hoc test based on the studentized range distribution, to find out which specific groups&apos;s means (compared with each other) are different.<br>
                           There is also an option of performing the whole process removing the outliers from the respective Data. <br>")),
                  fluidRow(box(plotOutput(ns("ErrorsQ4"))),
                           box(verbatimTextOutput(ns("ErrorsQ4_jar")), verbatimTextOutput(ns("ErrorsQ4_Lev")))),
                  fluidRow(
                    verbatimTextOutput(ns("textQ41")),
                    verbatimTextOutput(ns("textQ42")),
                    verbatimTextOutput(ns("textQ43"))
                  )
            )
    )
}

# Server

hw1_q4_server <- function(input, output, session, data, dataM) {

    # proposed1
    output$proposed1 <- renderPlot({
        ggplot(dataM(), aes(x = variable, y = value, fill = manufacturer)) +
            geom_boxplot() +
            labs(fill = "Miles", x = "Manufacturer", y = "Miles per gallon") +
            ggtitle("Manufacturer influence") +
            theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()
    })

    # ErrorsQ4
    output$ErrorsQ4 <- renderPlot({
        c <- if (input$selectQ4 == 1) {
            qqnorm(data()$cty - mean(data()$cty), main = "QQ-Plot of Errors City Mileage")
            qqline(data()$cty - mean(data()$cty))
        }
        else if (input$selectQ4 == 2) {
            qqnorm(data()$hwy - mean(data()$hwy), main = "QQ-Plot of Errors Highway Mileage")
            qqline(data()$hwy - mean(data()$hwy))
        }
        return(c)
    })

    # ErrorsQ4_jar
    output$ErrorsQ4_jar <- renderPrint({
        c <- if (input$selectQ4 == 1) { jarque.bera.test(data()$cty) }
        else if (input$selectQ4 == 2) { jarque.bera.test(data()$hwy) }
        return(c)
    })

    # ErrorsQ4_Lev
    output$ErrorsQ4_Lev <- renderPrint({
        c <- if (input$selectQ4 == 1) { leveneTest(cty ~ manufacturer, data = data()) }
        else if (input$selectQ4 == 2) { leveneTest(hwy ~ manufacturer, data = data()) }
        return(c)
    })

    # textQ41
    output$textQ41 <- renderPrint({
        c <- if (input$selectQ4 == 1) { summary(aov(cty ~ as.factor(manufacturer), data())) }
        else if (input$selectQ4 == 2) { summary(aov(hwy ~ as.factor(manufacturer), data())) }
        return(c)
    })

    # textQ42
    output$textQ42 <- renderPrint({
        c <- if (input$selectQ4 == 1) { TukeyHSD(aov(cty ~ as.factor(manufacturer), data())) }
        else if (input$selectQ4 == 2) { TukeyHSD(aov(hwy ~ as.factor(manufacturer), data())) }
        return(c)
    })

    # textQ43
    output$textQ43 <- renderPrint({
        c <- if (input$selectQ4 == 1) { kruskal.test(cty ~ as.factor(manufacturer), data = data()) }
        else if (input$selectQ4 == 2) { kruskal.test(hwy ~ as.factor(manufacturer), data = data()) }
        return(c)
    })
}