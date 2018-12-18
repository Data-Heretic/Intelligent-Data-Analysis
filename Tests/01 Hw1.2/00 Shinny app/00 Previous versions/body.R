

body4UI <- function(id){
  ns <- NS(id)
  tabItems(
    tabItem(tabName = "Q1a",
            
            h3("Is there a significant difference in city mileage between automatic and manual transmission cars?"),
            div(HTML("<ol start='1'><li> Boxplot and density plot of City Mileage vs type of transmision. <br>
                     The analysis is done with a new variable that differs between automatic and manual cars. <br>
                     The objective is to determine whether the mean between both populations is equal <br></li></ol>")),
            fluidPage(
              fluidRow(box(plotOutput(ns("plot_cty"))),
                       box(plotOutput(ns("plot2_cty")),
                           selectInput(ns("XPlot1_cty"),"Logarithmic or normal variable?",c("Original Data"="cty", "Log Data" = "log(cty)"))
                       )),
              div(HTML("<ol start='2'><li> In order to test if there is a difference between the means, we propose to use One-Way ANOVA. <br>
                       Before we test the assumptions of normality of the errors and Homoscedasticity with Jarque bera and Levene tests, respectively.<br>
                       Finally we use One-Way ANOVA if the test assumptions are met otherwsise we apply the non-parametric Kruskall-Wallis test. <br>
                       There is also an option of performing the whole process removing the outliers from the City Mileage Data. <br>
                       We can Conclude by using the log of the data that there is a difference <br></li></ol> ")),
              fluidRow(box(plotOutput(ns("Errors1"))),
                       box(verbatimTextOutput(ns("Errors1_jar")), verbatimTextOutput("Errors1_Lev"))),
              fluidRow(
                verbatimTextOutput("text1"),
                verbatimTextOutput("text2")
              )
              )
            ),
    
    tabItem(tabName = "Q1b",
            
            h3("What about for highway mileage?"),
            div(HTML("<ol start='1'><li> Boxplot and density plot of Highway Mileage vs type of transmision. <br>
                     The analysis is done with a new variable that differs between automatic and manual cars. <br>
                     The objective is to determine whether the mean between both populations is equal <br></li></ol>")),
            fluidPage(
              fluidRow(box(plotOutput("plot_hwy")),
                       box(plotOutput("plot2_hwy"),
                           selectInput("XPlot1_hwy","Logarithmic or normal variable?",c("Original Data"="hwy", "Log Data" = "log(hwy)")))
                       
              ),
              div(HTML("<ol start='2'><li> In order to test if there is a difference between the means, we propose to use One-Way ANOVA. <br>
                       Before we test the assumptions of normality of the errors and Homoscedasticity with Jarque bera and Levene tests, respectively.<br>
                       Finally we use One-Way ANOVA if the test assumptions are met otherwsise we apply the non-parametric Kruskall-Wallis test. <br>
                       There is also an option of performing the whole process removing the outliers from the Highway Mileage Data. <br>
                       We can Conclude by using the log of the data that there is a difference <br></li></ol> ")),
              fluidRow(box(plotOutput("Errors2")), box(verbatimTextOutput("Errors2_jar"), verbatimTextOutput("Errors2_Lev"))),
              fluidRow(
                verbatimTextOutput("text21"),
                verbatimTextOutput("text22")
              )
              )
            
              ),
    tabItem(tabName = "Q1c",
            
            h3("What about engine displacement in litres?"),
            div(HTML("<ol start='1'><li> Boxplot and density plot of Engine displacement vs type of transmision. <br>
                     The analysis is done with a new variable that differs between automatic and manual cars. <br>
                     The objective is to determine whether the mean between both populations is equal <br></li></ol>")),
            fluidPage(fluidRow(box(plotOutput("plot_displ")),
                               box(plotOutput("plot2_displ"),
                                   selectInput("XPlot1_displ","Logarithmic or normal variable?",c("Original Data"="displ", "Log Data" = "log(displ)")))
            ),
            div(HTML("<ol start='2'><li> In order to test if there is a difference between the means, we propose to use One-Way ANOVA. <br>
                     Before we test the assumptions of normality of the errors and Homoscedasticity with Jarque bera and Levene tests, respectively.<br>
                     Finally we use One-Way ANOVA if the test assumptions are met otherwsise we apply the non-parametric Kruskall-Wallis test. <br>
                     There is also an option of performing the whole process removing the outliers from the Engine Displacement Data. <br>
                     We can Conclude by using the log of the data that there is a difference <br></li></ol> ")),
            fluidRow(box(plotOutput("Errors3")),
                     box(verbatimTextOutput("Errors3_jar"), verbatimTextOutput("Errors3_Lev"))),
            fluidRow(
              verbatimTextOutput("text31"),
              verbatimTextOutput("text32")
            )
            )
            
            ),
    tabItem(tabName = "Q2",
            
            h3("Has fuel type (or cylinders) any influence on highway mpg or city mpg?"),
            h5("Boxplots comparing the City Mileage to the Highway Mileage with respect to : A)The fuel type of each automobile and B)The number of cylinders each automobile has. "),
            fluidPage(
              fluidRow(box(plotOutput("plot_engine")),
                       box(plotOutput("plot_engine2"))
              ),
              fluidRow(
                box(selectInput("selectQ2", h3("Between which variables do you want to perform a statistical test?"), 
                                choices = list("City Mileage ~ Fuel Type" = 1, "Highway Mileage ~ Fuel Type" = 2,
                                               "City Mileage ~ Number of Cylinders" = 3, "Highway Mileage ~ Number of Cylinders" = 4), selected = 1))
              ),
              div(HTML("In order to test if there is a difference between the means, we propose to use One-Way ANOVA. <br>
                       Before we apply it we have to test the assumptions of normality of the errors and Homoscedasticity taking advantage of Jarque bera and Levene tests, respectively.<br>
                       If the test assumptions are met we apply One-Way ANOVA, otherwsise we apply the non-parametric Kruskall-Wallis test. <br>
                       In case One-Way ANOVA finds significant results we then run Tukey&apos;s HSD, a post-hoc test based on the studentized range distribution, to find out which specific groups&apos;s means (compared with each other) are different.<br>
                       There is also an option of performing the whole process removing the outliers from the respective Data. <br>")),
              fluidRow(box(plotOutput("ErrorsQ2")),
                       box(verbatimTextOutput("ErrorsQ2_jar"), verbatimTextOutput("ErrorsQ2_Lev"))),
              fluidRow(
                verbatimTextOutput("textQ21"),
                verbatimTextOutput("textQ22"),
                verbatimTextOutput("textQ23")
              )
              )
            ),
    tabItem(tabName = "Q3",
            h3("Comparing the distribution of some of the variables in years 1999 and 2008, determine whether the requests on automobiles have been changed."),
            h5("On the histogram plot we compare the distribution of the engine displacement for each automotive throught the years "),
            h5("On the scatterplot it is illustrated how the milage is affected by the type of transmission and the year of the car manufacturing."),
            
            fluidPage(fluidRow(box(plotOutput("year")), 
                               box(plotOutput("year3"))),
                      fluidRow(
                        h5("We have used a reactive bar chart to visualize how the different levels of the categorical variables are distributed between 1999 and 2008."),
                        box(column(3,div(style="display: inline-block; margin-top: 25%", radioButtons("radio",
                                                                                                      
                                                                                                      label="Variable",
                                                                                                      choices = list(  "Manufacturer"= 1 ,
                                                                                                                       "Fuel Type" = 2,
                                                                                                                       "Vehicle Class" = 3, 
                                                                                                                       "Transmission" = 4, 
                                                                                                                       "Drive Type" = 5, 
                                                                                                                       "Cylinders" = 6
                                                                                                      ), 
                                                                                                      selected = 1))),
                            column(8, (plotOutput("year2"))), width = 60)),
                      fluidRow(
                        h5("A mosaic plot is a another useful graphical display that allows us to examine the relationship between one of the above mentioned categorical variables and the year of manufacturing.
                           The plot is accompanied by a Chi-squared test among the corresponding categorical variables."),
                        box(tableOutput("ChiTest"), width = 5), 
                        box(plotOutput("mosaicPlot", width = "100%"), width = 7))
                      )
    ),
    
    tabItem(tabName = "Q4",
            h3("Proposed Question - 
               Is there any difference in city and highway mpg between manufacturers?"),
            h5("For this question we have used a double box plot to show how the different manufacturers engines behave in terms of consumption for City driving and highway driving."),
            fluidPage(
              fluidRow(plotOutput("proposed1")),
              fluidRow(
                box(selectInput("selectQ4", h3("Between which variables do you want to perform a statistical test?"), 
                                choices = list("City Mileage ~ Manufacturer" = 1, "Highway Mileage ~ Manufacturer" = 2), selected = 1))
              ),
              div(HTML("In order to test if there is a difference between the means, we propose to use One-Way ANOVA. <br>
                       Before we apply it we have to test the assumptions of normality of the errors and Homoscedasticity taking advantage of Jarque bera and Levene tests, respectively.<br>
                       If the test assumptions are met we apply One-Way ANOVA, otherwsise we apply the non-parametric Kruskall-Wallis test. <br>
                       In case One-Way ANOVA finds significant results we then run Tukey&apos;s HSD, a post-hoc test based on the studentized range distribution, to find out which specific groups&apos;s means (compared with each other) are different.<br>
                       There is also an option of performing the whole process removing the outliers from the respective Data. <br>")),
              fluidRow(box(plotOutput("ErrorsQ4")),
                       box(verbatimTextOutput("ErrorsQ4_jar"), verbatimTextOutput("ErrorsQ4_Lev"))),
              fluidRow(
                verbatimTextOutput("textQ41"),
                verbatimTextOutput("textQ42"),
                verbatimTextOutput("textQ43")
              )
              )
            
            )
            )
}

body4 <- function(input, output, session, data){
  
    output$Errors1_jar <- renderPrint({
    c <- if(input$XPlot1_cty == "cty"){jarque.bera.test(data$cty)}
    else {jarque.bera.test(log(data$cty))}
    return(c)
    
    })
    
    output$Errors1  <- renderPlot({
      c <- if(input$XPlot1_cty == "cty"){
        qqnorm(data$cty - mean(data$cty), main = "QQ-Plot of Errors City Mileage")
        qqline(data$cty - mean(data$cty)) }
      else {
        qqnorm(log(data$cty) - mean(log(data$cty)), main = "QQ-Plot of Errors of log(City Mileage)")
        qqline(log(data$cty) - mean(log(data$cty))) }
      return(c)
    })
    
    output$Errors1_Lev <- renderPrint({
      c <- if(input$XPlot1_cty == "cty"){leveneTest(cty ~ tr, data = dataset())}
      else {leveneTest(log(cty) ~ tr, data = dataset())}
      return(c)
    })
    
}