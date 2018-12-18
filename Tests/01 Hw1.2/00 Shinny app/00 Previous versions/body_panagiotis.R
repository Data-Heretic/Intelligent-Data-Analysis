

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
                       box(verbatimTextOutput(ns("Errors1_jar")), verbatimTextOutput(ns("Errors1_Lev")))),
              fluidRow(
                verbatimTextOutput(ns("text1")),
                verbatimTextOutput(ns("text2"))
              )
              )
            ),
    
    tabItem(tabName = "Q1b",
            
            h3("What about for highway mileage?"),
            div(HTML("<ol start='1'><li> Boxplot and density plot of Highway Mileage vs type of transmision. <br>
                     The analysis is done with a new variable that differs between automatic and manual cars. <br>
                     The objective is to determine whether the mean between both populations is equal <br></li></ol>")),
            fluidPage(
              fluidRow(box(plotOutput(ns("plot_hwy"))),
                       box(plotOutput(ns("plot2_hwy")),
                           selectInput(ns("XPlot1_hwy"),"Logarithmic or normal variable?",c("Original Data"="hwy", "Log Data" = "log(hwy)")))
                       
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
            
              ),
    tabItem(tabName = "Q1c",
            
            h3("What about engine displacement in litres?"),
            div(HTML("<ol start='1'><li> Boxplot and density plot of Engine displacement vs type of transmision. <br>
                     The analysis is done with a new variable that differs between automatic and manual cars. <br>
                     The objective is to determine whether the mean between both populations is equal <br></li></ol>")),
            fluidPage(fluidRow(box(plotOutput(ns("plot_displ"))),
                               box(plotOutput(ns("plot2_displ")),
                                   selectInput(ns("XPlot1_displ"),"Logarithmic or normal variable?",c("Original Data"="displ", "Log Data" = "log(displ)")))
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
            
            ),
    tabItem(tabName = "Q2",
            
            h3("Has fuel type (or cylinders) any influence on highway mpg or city mpg?"),
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
            ),
    tabItem(tabName = "Q3",
            h3("Comparing the distribution of some of the variables in years 1999 and 2008, determine whether the requests on automobiles have been changed."),
            h5("On the histogram plot we compare the distribution of the engine displacement for each automotive throught the years "),
            h5("On the scatterplot it is illustrated how the milage is affected by the type of transmission and the year of the car manufacturing."),
            
            fluidPage(fluidRow(box(plotOutput(ns("year"))), 
                               box(plotOutput(ns("year3")))),
                      fluidRow(
                        h5("We have used a reactive bar chart to visualize how the different levels of the categorical variables are distributed between 1999 and 2008."),
                        box(column(3,div(style="display: inline-block; margin-top: 25%", radioButtons(ns("radio"),
                                                                                                      
                                                                                                      label="Variable",
                                                                                                      choices = list(  "Manufacturer"= 1 ,
                                                                                                                       "Fuel Type" = 2,
                                                                                                                       "Vehicle Class" = 3, 
                                                                                                                       "Transmission" = 4, 
                                                                                                                       "Drive Type" = 5, 
                                                                                                                       "Cylinders" = 6
                                                                                                      ), 
                                                                                                      selected = 1))),
                            column(8, (plotOutput(ns("year2")))), width = 60)),
                      fluidRow(
                        h5("A mosaic plot is a another useful graphical display that allows us to examine the relationship between one of the above mentioned categorical variables and the year of manufacturing.
                           The plot is accompanied by a Chi-squared test among the corresponding categorical variables."),
                        box(tableOutput(ns("ChiTest")), width = 5), 
                        box(plotOutput(ns("mosaicPlot"), width = "100%"), width = 7))
                      )
    ),
    
    tabItem(tabName = "Q4",
            h3("Proposed Question - 
               Is there any difference in city and highway mpg between manufacturers?"),
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
            )
}

body4 <- function(input, output, session, data, dataM){
  
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
      c <- if(input$XPlot1_cty == "cty"){leveneTest(cty ~ tr, data = data)}
      else {leveneTest(log(cty) ~ tr, data = data)}
      return(c)
    })
    
    output$ChiTest <- renderTable({
      t1<- data %>%
        group_by(year,manufacturer) %>%
        summarize(Count1 = n()) 
      
      tfl<- data %>%
        group_by(year,fl) %>%
        summarize(Count1 = n()) 
      
      tclass<- data %>%
        group_by(year,class) %>%
        summarize(Count1 = n()) 
      
      ttr<- data %>%
        group_by(year,tr) %>%
        summarize(Count1 = n()) 
      
      tdt <- data %>%
        group_by(year,drv) %>%
        summarize(Count1 = n()) 
      
      tcyl <- data %>%
        group_by(year,cyl) %>%
        summarize(Count1 = n()) 
      
      datos <-if(input$radio == '1') {t1}
      else if(input$radio == '2') {tfl} 
      else if(input$radio == '3') {tclass}
      else if(input$radio == '4') {ttr}
      else if(input$radio == '5') {tdt}
      else if(input$radio == '6') {tcyl}
      
      a <- data.frame(Variable=1:length(distinct(datos[2])%>% pull()), Chi_Test_p_value=1)
      for (i in 1:length(distinct(datos[2])%>% pull())){
        a[i,1] = toString(((distinct(datos[2]) %>% pull())[i]))
        if (length(datos[which(datos[2]==toString(((distinct(datos[2]) %>% pull())[i]))),]$Count1)==1) {
          a[i,2] = "NA"
        }
        else {
          a[i,2] = chisq.test(datos[which(datos[2]==toString(((distinct(datos[2]) %>% pull())[i]))),]$Count1, p = c(1/2, 1/2))$p.value
        }
      }
      return(a)
    })
    
    output$mosaicPlot <- renderPlot({
      contigency_table <-if(input$radio == '1') {xtabs(~year+manufacturer, data=data)}
      else if(input$radio == '2') {xtabs(~year+fl, data=data)} 
      else if(input$radio == '3') {xtabs(~year+class, data=data)}
      else if(input$radio == '4') {xtabs(~year+tr, data=data)}
      else if(input$radio == '5') {xtabs(~year+drv, data=data)}
      else if(input$radio == '6') {xtabs(~year+cyl, data=data)}
      mosaic(contigency_table, gp=shading_max, split_vertical=TRUE, rot_labels=c(0,90,0,0), labeling_args = list(offset_labels = c(left = 1.5, top=0), offset_varnames = c(left = 4, top=1)))
    })
    
    output$proposed1 <- renderPlot({
      ggplot(dataM,aes(x=variable,y=value, fill = manufacturer)) +
        geom_boxplot() +
        labs(fill = "Miles",x="Manufacturer",y="Miles per gallon") + 
        ggtitle("Manufacturer influence") + 
        theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()
    })

    
}
