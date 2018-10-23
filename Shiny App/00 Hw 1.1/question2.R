###################################################
##########         Question 2        ##############
###################################################

# UI

hw1_q2_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Q2"),
            h2(HTML("<b> Mileage Analysis </b>")),
            h3("Has fuel type (or cylinders) any influence on highway data or city data?"),
            h5("Boxplots comparing the City Mileage to the Highway Mileage with respect to : A)The fuel type of each automobile and B)The number of cylinders each automobile has. "),
            fluidPage(
                  fluidRow(box(plotOutput(ns("plot_engine"))),
                           box(plotOutput(ns("plot_engine2")))
                  ),
                  fluidRow(
                    box(selectInput(ns("selectQ2"), h3("Between which variables do you want to perform a statistical test?"),
                                    choices = list("City Mileage ~ Fuel Type" = 1, "Highway Mileage ~ Fuel Type" = 2,
                                                   "City Mileage ~ Number of Cylinders" = 3, "Highway Mileage ~ Number of Cylinders" = 4), selected = 1))
                  ),
                  div(HTML("In order to test if there is a difference between the means, we propose to use One-Way ANOVA. <br>
                           Before we apply it we have to test the assumptions of normality of the errors and Homoscedasticity taking advantage of Jarque bera and Levene tests, respectively.<br>
                           If the test assumptions are met we apply One-Way ANOVA, otherwsise we apply the non-parametric Kruskall-Wallis test. <br>
                           In case One-Way ANOVA finds significant results we then run Tukey&apos;s HSD, a post-hoc test based on the studentized range distribution, to find out which specific groups&apos;s means (compared with each other) are different.<br>
                           There is also an option of performing the whole process removing the outliers from the respective Data. <br>")),
                  fluidRow(box(plotOutput(ns("ErrorsQ2"))),
                           box(verbatimTextOutput(ns("ErrorsQ2_jar")), verbatimTextOutput(ns("ErrorsQ2_Lev")))),
                  fluidRow(
                    verbatimTextOutput(ns("textQ21")),
                    verbatimTextOutput(ns("textQ22")),
                    verbatimTextOutput(ns("textQ23"))
                  )
            )
        )
}

# Server

hw1_q2_server <- function(input, output, session, data, dataM) {

    # plot_engine
    output$plot_engine <- renderPlot({
        ggplot(data = dataM(), aes(x = variable, y = value, fill = fl)) + geom_boxplot() + labs(fill = "Fuel type", x = " ", y = "Miles per gallon")
    })

    # plot_engine2
    output$plot_engine2 <- renderPlot({
        ggplot(dataM(), aes(x = variable, y = value, fill = as.factor(cyl))) + geom_boxplot() + labs(fill = "Number of cylinders", x = " ", y = "Miles per gallon")
    })

    # ErrorsQ2
    output$ErrorsQ2 <- renderPlot({
        c <- if (input$selectQ2 == 1 || input$selectQ2 == 3) {
            qqnorm(data()$cty - mean(data()$cty), main = "QQ-Plot of Errors City Mileage")
            qqline(data()$cty - mean(data()$cty))
        }
        else if (input$selectQ2 == 2 || input$selectQ2 == 4) {
            qqnorm(data()$hwy - mean(data()$hwy), main = "QQ-Plot of Errors Highway Mileage")
            qqline(data()$hwy - mean(data()$hwy))
        }
        return(c)
    })

    # ErrorsQ2_jar
    output$ErrorsQ2_jar <- renderPrint({
        c <- if (input$selectQ2 == 1 || input$selectQ2 == 3) { jarque.bera.test(data()$cty) }
        else if (input$selectQ2 == 2 || input$selectQ2 == 4) { jarque.bera.test(data()$hwy) }
        return(c)
    })

    # ErrorsQ2_Lev
    output$ErrorsQ2_Lev <- renderPrint({
        c <- if (input$selectQ2 == 1) { leveneTest(cty ~ fl, data = data()) }
        else if (input$selectQ2 == 2) { leveneTest(hwy ~ fl, data = data()) }
        else if (input$selectQ2 == 3) { leveneTest(cty ~ cyl, data = data()) }
        else if (input$selectQ2 == 4) { leveneTest(hwy ~ cyl, data = data()) }
        return(c)
    })

    # textQ21
    output$textQ21 <- renderPrint({
        c <- if (input$selectQ2 == 1) { summary(aov(cty ~ fl, data())) }
        else if (input$selectQ2 == 2) { summary(aov(hwy ~ fl, data())) }
        else if (input$selectQ2 == 3) { summary(aov(cty ~ cyl, data())) }
        else if (input$selectQ2 == 4) { summary(aov(hwy ~ cyl, data())) }
        return(c)
    })

    # textQ22
    output$textQ22 <- renderPrint({
        c <- if (input$selectQ2 == 1) { TukeyHSD(aov(cty ~ fl, data())) }
        else if (input$selectQ2 == 2) { TukeyHSD(aov(hwy ~ fl, data())) }
        else if (input$selectQ2 == 3) { TukeyHSD(aov(cty ~ cyl, data())) }
        else if (input$selectQ2 == 4) { TukeyHSD(aov(hwy ~ cyl, data())) }
        return(c)
    })

    # textQ23
    output$textQ23 <- renderPrint({
        c <- if (input$selectQ2 == 1) { kruskal.test(cty ~ as.factor(fl), data = data()) }
        else if (input$selectQ2 == 2) { kruskal.test(hwy ~ as.factor(fl), data = data()) }
        else if (input$selectQ2 == 3) { kruskal.test(cty ~ as.factor(cyl), data = data()) }
        else if (input$selectQ2 == 4) { kruskal.test(hwy ~ as.factor(cyl), data = data()) }
        return(c)
    })
}