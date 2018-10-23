###################################################
##########         Question 2        ##############
###################################################

# UI

hw1_q2_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Q2"),
            h3("Has fuel type (or cylinders) any influence on highway data or city data?"),
            h5("Boxplots comparing the City Mileage to the Highway Mileage with respect to : A)The fuel type of each automobile and B)The number of cylinders each automobile has. "),
            fluidPage(
                  fluidRow(box(plotOutput(ns("plot.box.data"))),
                           box(plotOutput(ns("plot.box.datam")))
                  ),
                  fluidRow(
                    box(selectInput(ns("selection"), h3("Between which variables do you want to perform a statistical test?"),
                                    choices = list("City Mileage ~ Fuel Type" = 1, "Highway Mileage ~ Fuel Type" = 2,
                                                   "City Mileage ~ Number of Cylinders" = 3, "Highway Mileage ~ Number of Cylinders" = 4), selected = 1))
                  ),
                  div(HTML("In order to test if there is a difference between the means, we propose to use One-Way ANOVA. <br>
                           Before we apply it we have to test the assumptions of normality of the errors and Homoscedasticity taking advantage of Jarque bera and Levene tests, respectively.<br>
                           If the test assumptions are met we apply One-Way ANOVA, otherwsise we apply the non-parametric Kruskall-Wallis test. <br>
                           In case One-Way ANOVA finds significant results we then run Tukey&apos;s HSD, a post-hoc test based on the studentized range distribution, to find out which specific groups&apos;s means (compared with each other) are different.<br>
                           There is also an option of performing the whole process removing the outliers from the respective Data. <br>")),
                  fluidRow(box(plotOutput(ns("plot.qq.selection_errors"))),
                           box(verbatimTextOutput(ns("test.jarque_bera.selection")), verbatimTextOutput(ns("test.levene.selection")))),
                  fluidRow(
                    verbatimTextOutput(ns("summary.aov.selection")),
                    verbatimTextOutput(ns("test.tukeyhsd.selection")),
                    verbatimTextOutput(ns("test.kruskal.selection"))
                  )
            )
        )
}

# Server

hw1_q2_server <- function(input, output, session, data, dataM) {

    # Plot: Boxplot data
    output$plot.box.data <- renderPlot({
        ggplot(data = dataM(), aes(x = variable, y = value, fill = fl)) + geom_boxplot() + labs(fill = "Fuel type", x = " ", y = "Miles per gallon")
    })

    # Plot: Boxplot dataM
    output$plot.box.datam <- renderPlot({
        ggplot(dataM(), aes(x = variable, y = value, fill = as.factor(cyl))) + geom_boxplot() + labs(fill = "Number of cylinders", x = " ", y = "Miles per gallon")
    })

    # Plot: QQplot cty errors
    output$plot.qq.selection_errors <- renderPlot({
        c <- if (input$selection == 1 || input$selection == 3) {
            qqnorm(data()$cty - mean(data()$cty), main = "QQ-Plot of Errors City Mileage")
            qqline(data()$cty - mean(data()$cty))
        }
        else if (input$selection == 2 || input$selection == 4) {
            qqnorm(data()$hwy - mean(data()$hwy), main = "QQ-Plot of Errors Highway Mileage")
            qqline(data()$hwy - mean(data()$hwy))
        }
        return(c)
    })

    # Test: Jarque bera selection
    output$test.jarque_bera.selection <- renderPrint({
        c <- if (input$selection == 1 || input$selection == 3) { jarque.bera.test(data()$cty) }
        else if (input$selection == 2 || input$selection == 4) { jarque.bera.test(data()$hwy) }
        return(c)
    })

    # Test: Levene selection
    output$test.levene.selection <- renderPrint({
        c <- if (input$selection == 1) { leveneTest(cty ~ fl, data = data()) }
        else if (input$selection == 2) { leveneTest(hwy ~ fl, data = data()) }
        else if (input$selection == 3) { leveneTest(cty ~ cyl, data = data()) }
        else if (input$selection == 4) { leveneTest(hwy ~ cyl, data = data()) }
        return(c)
    })

    # Summary: AOV selection
    output$summary.aov.selection <- renderPrint({
        c <- if (input$selection == 1) { summary(aov(cty ~ fl, data())) }
        else if (input$selection == 2) { summary(aov(hwy ~ fl, data())) }
        else if (input$selection == 3) { summary(aov(cty ~ cyl, data())) }
        else if (input$selection == 4) { summary(aov(hwy ~ cyl, data())) }
        return(c)
    })

    # Test: TukeyHSD selection 
    output$test.tukeyhsd.selection <- renderPrint({
        c <- if (input$selection == 1) { TukeyHSD(aov(cty ~ fl, data())) }
        else if (input$selection == 2) { TukeyHSD(aov(hwy ~ fl, data())) }
        else if (input$selection == 3) { TukeyHSD(aov(cty ~ cyl, data())) }
        else if (input$selection == 4) { TukeyHSD(aov(hwy ~ cyl, data())) }
        return(c)
    })

    # Test: Kruskal selection
    output$test.kruskal.selection <- renderPrint({
        c <- if (input$selection == 1) { kruskal.test(cty ~ as.factor(fl), data = data()) }
        else if (input$selection == 2) { kruskal.test(hwy ~ as.factor(fl), data = data()) }
        else if (input$selection == 3) { kruskal.test(cty ~ as.factor(cyl), data = data()) }
        else if (input$selection == 4) { kruskal.test(hwy ~ as.factor(cyl), data = data()) }
        return(c)
    })
}