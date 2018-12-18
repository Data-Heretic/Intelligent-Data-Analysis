## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(tseries)
library(lawstat)
library(testit)
library(vcd)
library(car)

# Source helper functions -----
source("helpers.R")

mpg <- get(load("mpg.Rdata"))

mpg$tr <-substr(mpg$trans, 0, 1)
mpg$tr<-str_replace(mpg$tr,"^m","Manual") 
mpg$tr<-str_replace(mpg$tr,"^a","Automatic")

mpg<- mpg[mpg$fl!="c",]

mpg$year = as.factor(mpg$year)
mpg$drv = as.factor(mpg$drv)
mpg$cyl = as.factor(mpg$cyl)
mpg$fl = as.factor(mpg$fl)
mpg$class = as.factor(mpg$class)


ui <- dashboardPage(
  dashboardHeader( title = "H1.1 - Mileage",
                   tags$li(a(href = 'https://www.fi.upm.es/',
                             img(src = 'Captura.png',
                                 title = "a", height = "31px", width = "96px"),
                             style = "padding-top:10px; padding-bottom:10px;"),
                           class = "dropdown")),
  dashboardSidebar(
      sidebarMenu(
        menuItem("Question 1", tabName = "Q1", icon = icon("comment", lib = "font-awesome"), 
                 menuItem("a",
                          tabName = "Q1a",
                          icon = icon("line-chart")),
                 menuItem("b",
                          tabName = "Q1b",
                          icon = icon("line-chart")),
                 menuItem("c",
                          tabName = "Q1c",
                          icon = icon("line-chart"))
                 
        
                 ),
        menuItem("Question 2", tabName = "Q2", icon = icon("comment", lib = "font-awesome")),
        menuItem("Question 3", tabName = "Q3", icon = icon("comment", lib = "font-awesome")),
        menuItem("Question 4", tabName = "Q4", icon = icon("comment", lib = "font-awesome"))
      )
  ),
  dashboardBody( 
    tags$head(tags$style(HTML(".plot_engine2 {height: 1500px, width:500px}"))),
    tags$footer(checkboxInput("outliers", "Remove Outliers", value = FALSE)),
    h2(HTML("<b> Mileage Analysis </b>")),
    tabItems(
      tabItem(tabName = "Q1a",
              
              h3("Is there a significant difference in city mileage between automatic and manual transmission cars?"),
              div(HTML("<ol start='1'><li> Boxplot and density plot of City Mileage vs type of transmision. <br>
                       The analysis is done with a new variable that differs between automatic and manual cars. <br>
                       The objective is to determine whether the mean between both populations is equal <br></li></ol>")),
              fluidPage(
                fluidRow(box(plotOutput("plot_cty")),
                        box(plotOutput("plot2_cty"),
                            selectInput("XPlot1_cty","Logarithmic or normal variable?",c("Original Data"="cty", "Log Data" = "log(cty)"))
                        )),
                div(HTML("<ol start='2'><li> In order to test if there is a difference between the means, we propose to use One-Way ANOVA. <br>
                         Before we test the assumptions of normality of the errors and Homoscedasticity with Jarque bera and Levene tests, respectively.<br>
                         Finally we use One-Way ANOVA if the test assumptions are met otherwsise we apply the non-parametric Kruskall-Wallis test. <br>
                         There is also an option of performing the whole process removing the outliers from the City Mileage Data. <br>
                         We can Conclude by using the log of the data that there is a difference <br></li></ol> ")),
                fluidRow(box(plotOutput("Errors1")),
                         box(verbatimTextOutput("Errors1_jar"), verbatimTextOutput("Errors1_Lev"))),
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
  )
)

server <- function(input, output) { 
  
  dataset <- reactive({
    if(input$outliers){
      temp <- mpg
      temp$cty <- remove_outliers(temp$cty)
      temp$hwy <- remove_outliers(temp$hwy)
      temp$displ <- remove_outliers(temp$displ)
      temp <- temp[complete.cases(temp),] 
      return(temp)
    } 
    else{
      return(mpg)
    }
  })
  
  datasetM <- reactive({
    mmpg<-melt(dataset(),id=c("manufacturer","model","displ","year","cyl","trans","drv","fl","class","tr"))
    mmpg$variable<- factor(mmpg$variable, levels=c("cty", "hwy"), labels = c("City Mileage", "Highway Mileage") )
    mmpg$fl<-factor(mmpg$fl,levels=c("p","d","e","r"),labels=c("Petrol","Diesel","Ethanol","Regular"))
    return(mmpg)
  })
  
  
  
#### PLOTS #####  
  
  output$Errors1 <- renderPlot({
    c <- if(input$XPlot1_cty == "cty"){qqnorm(dataset()$cty - mean(dataset()$cty), main = "QQ-Plot of Errors City Mileage")
                                        qqline(dataset()$cty - mean(dataset()$cty)) }
    else {qqnorm(log(dataset()$cty) - mean(log(dataset()$cty)), main = "QQ-Plot of Errors of log(City Mileage)")
      qqline(log(dataset()$cty) - mean(log(dataset()$cty))) }
    return(c)
  })
  
  output$Errors1_jar <- renderPrint({
    c <- if(input$XPlot1_cty == "cty"){jarque.bera.test(dataset()$cty)}
    else {jarque.bera.test(log(dataset()$cty))}
    return(c)
    
  })
  output$Errors1_Lev <- renderPrint({
    c <- if(input$XPlot1_cty == "cty"){leveneTest(cty ~ tr, data = dataset())}
    else {leveneTest(log(cty) ~ tr, data = dataset())}
    return(c)
  })
  
  output$Errors2_jar <- renderPrint({
    c <- if(input$XPlot1_hwy == "hwy"){jarque.bera.test(dataset()$hwy)}
    else {jarque.bera.test(log(dataset()$hwy))}
    return(c)
  })
  
  output$Errors2 <- renderPlot({
    c <- if(input$XPlot1_hwy == "hwy"){qqnorm(dataset()$hwy - mean(dataset()$hwy), main = "QQ-Plot of Errors Highway Mileage")
                                       qqline(dataset()$hwy - mean(dataset()$hwy))}
    else {qqnorm(log(dataset()$hwy) - mean(log(dataset()$hwy)), main = "QQ-Plot of Errors of log(City Mileage)")
        qqline(log(dataset()$hwy) - mean(log(dataset()$hwy)))}
    return(c)
  }) 
  
  output$Errors2_Lev <- renderPrint({
    c <- if(input$XPlot1_hwy == "hwy"){leveneTest(hwy ~ tr, data = dataset())}
    else {leveneTest(log(hwy) ~ tr, data = dataset())}
    return(c)
  })

  
  output$Errors3 <- renderPlot({
    c <- if(input$XPlot1_displ == "displ"){qqnorm(dataset()$displ - mean(dataset()$displ), main = "QQ-Plot of Errors Engine Displacement")
      qqline(dataset()$displ - mean(dataset()$displ)) }
    else {qqnorm(log(dataset()$displ) - mean(log(dataset()$displ)), main = "QQ-Plot of Errors of log(Engine Displacement)")
      qqline(log(dataset()$displ) - mean(log(dataset()$displ))) }
    return(c)
  })
  
  output$Errors3_jar <- renderPrint({
    c <- if(input$XPlot1_displ == "displ"){jarque.bera.test(dataset()$displ)}
    else {jarque.bera.test(log(dataset()$displ))}
    return(c)
    
  })
  output$Errors3_Lev <- renderPrint({
    c <- if(input$XPlot1_displ == "displ"){leveneTest(displ ~ tr, data = dataset())}
    else {leveneTest(log(displ) ~ tr, data = dataset())}
    return(c)
  })
  
  output$ErrorsQ2 <- renderPlot({
    c <- if(input$selectQ2 == 1 || input$selectQ2 == 3){qqnorm(dataset()$cty - mean(dataset()$cty), main = "QQ-Plot of Errors City Mileage")
      qqline(dataset()$cty - mean(dataset()$cty)) }
    else if(input$selectQ2 == 2 || input$selectQ2 == 4){qqnorm(dataset()$hwy - mean(dataset()$hwy), main = "QQ-Plot of Errors Highway Mileage")
      qqline(dataset()$hwy - mean(dataset()$hwy)) }
    return(c)
  })
  
  output$ErrorsQ2_jar <- renderPrint({
    c <- if(input$selectQ2 == 1 || input$selectQ2 == 3){jarque.bera.test(dataset()$cty)}
    else if(input$selectQ2 == 2 || input$selectQ2 == 4){jarque.bera.test(dataset()$hwy)}
    return(c)
    
  })
  output$ErrorsQ2_Lev <- renderPrint({
    c <- if(input$selectQ2 == 1){leveneTest(cty ~ fl, data = dataset())}
    else if (input$selectQ2 == 2){leveneTest(hwy ~ fl, data = dataset())}
    else if (input$selectQ2 == 3){leveneTest(cty ~ cyl, data = dataset())}
    else if (input$selectQ2 == 4){leveneTest(hwy ~ cyl, data = dataset())}
    return(c)
  })
  
  output$ErrorsQ4 <- renderPlot({
    c <- if(input$selectQ4 == 1){qqnorm(dataset()$cty - mean(dataset()$cty), main = "QQ-Plot of Errors City Mileage")
      qqline(dataset()$cty - mean(dataset()$cty)) }
    else if(input$selectQ4 == 2){qqnorm(dataset()$hwy - mean(dataset()$hwy), main = "QQ-Plot of Errors Highway Mileage")
      qqline(dataset()$hwy - mean(dataset()$hwy)) }
    return(c)
  })
  
  output$ErrorsQ4_jar <- renderPrint({
    c <- if(input$selectQ4 == 1){jarque.bera.test(dataset()$cty)}
    else if(input$selectQ4 == 2){jarque.bera.test(dataset()$hwy)}
    return(c)
    
  })
  output$ErrorsQ4_Lev <- renderPrint({
    c <- if(input$selectQ4 == 1){leveneTest(cty ~ manufacturer, data = dataset())}
    else if (input$selectQ4 == 2){leveneTest(hwy ~ manufacturer, data = dataset())}
    return(c)
  })
  
  
  
  
  output$plot_cty <- renderPlot({
    
    ggplot(data = dataset(),aes(x=tr,y=cty, fill = tr)) +
      geom_boxplot() +
      labs(fill = "Miles",x="Type of transmission",y="Miles per gallon") + 
      ggtitle("City Mileage") + 
      theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()
    })
  
  output$plot_hwy <- renderPlot({
    
    ggplot(data = dataset(),aes(x=tr,y=hwy, fill = tr)) +
      geom_boxplot() +
      labs(fill = "Miles",x="Type of transmission",y="Miles per gallon") + 
      ggtitle("Highway Mileage") + 
      theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()
  })
  
  output$plot_displ <- renderPlot({
    
    ggplot(mpg,aes(x=tr, y = displ, fill =tr)) +
      geom_boxplot() +
      labs(fill = "Litres",x="Type of transmission",y="Litres") + 
      ggtitle("Engine Displacement") + 
      theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()
  })
  
  output$plot2_cty <- renderPlot({
    
    ggplot(data = dataset(),
           aes(x=eval(parse(text = input$XPlot1_cty)), fill = tr, colour = tr)) + 
           geom_density(alpha = 0.2) + theme_minimal() + xlab(if(input$XPlot1_cty == "cty") {"City Mileage"} else {"log(City Mileage)"})
    
  })
  
  output$plot2_hwy <- renderPlot({
    
    ggplot(data = dataset(),
           aes(x=eval(parse(text = input$XPlot1_hwy)), fill = tr, colour = tr)) + 
          geom_density(alpha = 0.2) + theme_minimal() + xlab(if(input$XPlot1_hwy == "hwy") {"Highway Mileage"} else {"log(Highway Mileage)"})
    
  })
  
  output$plot2_displ<- renderPlot({
    
    ggplot(mpg,
           aes(x=eval(parse(text = input$XPlot1_displ)), fill = tr, colour = tr)) + 
      geom_density( alpha=.3, bw=0.4) + theme_minimal() + xlab(if(input$XPlot1_displ == "displ") {"Engine Displacement"} else {"log(Engine Displacement)"})
    
  })
  
  output$text1 <- renderPrint({
    c <- if(input$XPlot1_cty == "cty"){summary(aov(cty ~ tr,dataset()))}
    else {summary(aov(log(cty) ~ tr,dataset()))}
    return(c)
  })
  
  output$text2 <- renderPrint({
    c <- if(input$XPlot1_cty == "cty"){kruskal.test(cty ~ as.factor(tr),data = dataset())}
    else {kruskal.test(log(cty) ~ as.factor(tr),data = dataset())}
    return(c)
  })
  
  output$text21 <- renderPrint({
    c <- if(input$XPlot1_hwy == "hwy"){summary(aov(hwy ~ tr,dataset()))}
    else {summary(aov(log(hwy) ~ tr,dataset()))}
    return(c)
  })
  
  output$text22 <- renderPrint({
    c <- if(input$XPlot1_hwy == "hwy"){kruskal.test(hwy ~ as.factor(tr),data = dataset())}
    else {kruskal.test(log(hwy) ~ as.factor(tr),data = dataset())}
    return(c)
  })
  
  
  output$text31 <- renderPrint({
    c <- if(input$XPlot1_displ == "displ"){summary(aov(displ ~ tr,dataset()))}
    else {summary(aov(log(displ) ~ tr,dataset()))}
    return(c)
  })
  
  output$text32 <- renderPrint({
    c <- if(input$XPlot1_displ == "displ"){kruskal.test(displ ~ as.factor(tr),data = dataset())}
    else {kruskal.test(log(displ) ~ as.factor(tr),data = dataset())}
    return(c)
  })
  
  output$textQ21 <- renderPrint({
    c <- if(input$selectQ2 == 1){summary(aov(cty ~ fl,dataset()))}
    else if(input$selectQ2 == 2) {summary(aov(hwy ~ fl,dataset()))}
    else if(input$selectQ2 == 3) {summary(aov(cty ~ cyl,dataset()))}
    else if(input$selectQ2 == 4) {summary(aov(hwy ~ cyl,dataset()))}
    return(c)
  })
  
  output$textQ22 <- renderPrint({
    c <- if(input$selectQ2 == 1){TukeyHSD(aov(cty ~ fl,dataset()))}
    else if(input$selectQ2 == 2) {TukeyHSD(aov(hwy ~ fl,dataset()))}
    else if(input$selectQ2 == 3) {TukeyHSD(aov(cty ~ cyl,dataset()))}
    else if(input$selectQ2 == 4) {TukeyHSD(aov(hwy ~ cyl,dataset()))}
    return(c)
  })
  
  output$textQ23 <- renderPrint({
    c <- if(input$selectQ2 == 1){kruskal.test(cty ~ as.factor(fl),data = dataset())}
    else if(input$selectQ2 == 2) {kruskal.test(hwy ~ as.factor(fl),data = dataset())}
    else if(input$selectQ2 == 3) {kruskal.test(cty ~ as.factor(cyl),data = dataset())}
    else if(input$selectQ2 == 4) {kruskal.test(hwy ~ as.factor(cyl),data = dataset())}
    return(c)
  })
  
  output$textQ41 <- renderPrint({
    c <- if(input$selectQ4 == 1){summary(aov(cty ~ as.factor(manufacturer),dataset()))}
    else if(input$selectQ4 == 2) {summary(aov(hwy ~ as.factor(manufacturer),dataset()))}
    return(c)
  })
  
  output$textQ42 <- renderPrint({
    c <- if(input$selectQ4 == 1){TukeyHSD(aov(cty ~ as.factor(manufacturer),dataset()))}
    else if(input$selectQ4 == 2) {TukeyHSD(aov(hwy ~ as.factor(manufacturer),dataset()))}
    return(c)
  })
  
  output$textQ43 <- renderPrint({
    c <- if(input$selectQ4 == 1){kruskal.test(cty ~ as.factor(manufacturer),data = dataset())}
    else if(input$selectQ4 == 2) {kruskal.test(hwy ~ as.factor(manufacturer),data = dataset())}
    return(c)
  })
  
  output$plot_engine <- renderPlot({
    
    ggplot(data = datasetM(),aes(x=variable,y=value,fill=fl))+geom_boxplot()+labs(fill = "Fuel type",x=" ",y="Miles per gallon")
  
  })

  output$plot_engine2 <- renderPlot({
    
    ggplot(datasetM(),aes(x=variable,y=value,fill=as.factor(cyl)))+geom_boxplot()+labs(fill = "Number of cylinders",x=" ",y="Miles per gallon")
    
  })
  output$year <- renderPlot({
    
    d1<-density(mpg$displ[mpg$year==1999])
    d2<-density(mpg$displ[mpg$year==2008])
    
    par(mfrow=c(1, 2))
    hist(mpg$displ[mpg$year==1999],breaks=10 ,probability = T,xlim=c(0, 8),ylim=c(0,0.5),col="gray", border="white",main="1999",xlab="Engine displacement (L)")
    lines(d1,col="red",lwd=2)
    hist(mpg$displ[mpg$year==2008],breaks=10 ,probability = T,xlim=c(0, 8),ylim=c(0,0.5),col="gray", border="white",main="2008",xlab="Engine displacement (L)",ylab=" ")
    lines(d2,col="red",lwd=2)
    
    
  })
  
  output$year3 <- renderPlot({
    
    ggplot(data = dataset(),aes(x=hwy,y=cty, fill = year, color=year)) +
             geom_point() +
             labs(x="Highway Mileage(mpg)",y="City Mileage(mpg)") + 
             ggtitle("Mileage") + 
             theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()
    
  })
  
  output$year2 <- renderPlot({
    t1<- dataset() %>%
      group_by(year,manufacturer) %>%
      summarize(Count1 = n()) 
    
    tfl<- dataset() %>%
      group_by(year,fl) %>%
      summarize(Count1 = n()) 
    
    tclass<- dataset() %>%
      group_by(year,class) %>%
      summarize(Count1 = n()) 
    
    ttr<- dataset() %>%
      group_by(year,tr) %>%
      summarize(Count1 = n()) 
    
    tdt <- dataset() %>%
      group_by(year,drv) %>%
      summarize(Count1 = n()) 
    
    tcyl <- dataset() %>%
      group_by(year,cyl) %>%
      summarize(Count1 = n()) 
    
    datos <-if(input$radio == '1') {t1}
            else if(input$radio == '2') {tfl} 
            else if(input$radio == '3') {tclass}
            else if(input$radio == '4') {ttr}
            else if(input$radio == '5') {tdt}
            else if(input$radio == '6') {tcyl}
    
    ggplot(data=datos,
               aes(x=eval(parse(text = colnames(datos)[2])), y=Count1, fill=factor(year))) + 
      geom_bar(position = "dodge", stat="identity") + scale_color_discrete("year") + theme_minimal() +
               xlab(colnames(datos)[2]) + ylab("Car nº")
    
    
  })
  output$ChiTest <- renderTable({
    t1<- dataset() %>%
      group_by(year,manufacturer) %>%
      summarize(Count1 = n()) 
    
    tfl<- dataset() %>%
      group_by(year,fl) %>%
      summarize(Count1 = n()) 
    
    tclass<- dataset() %>%
      group_by(year,class) %>%
      summarize(Count1 = n()) 
    
    ttr<- dataset() %>%
      group_by(year,tr) %>%
      summarize(Count1 = n()) 
    
    tdt <- dataset() %>%
      group_by(year,drv) %>%
      summarize(Count1 = n()) 
    
    tcyl <- dataset() %>%
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
    contigency_table <-if(input$radio == '1') {xtabs(~year+manufacturer, data=dataset())}
    else if(input$radio == '2') {xtabs(~year+fl, data=dataset())} 
    else if(input$radio == '3') {xtabs(~year+class, data=dataset())}
    else if(input$radio == '4') {xtabs(~year+tr, data=dataset())}
    else if(input$radio == '5') {xtabs(~year+drv, data=dataset())}
    else if(input$radio == '6') {xtabs(~year+cyl, data=dataset())}
    mosaic(contigency_table, gp=shading_max, split_vertical=TRUE, rot_labels=c(0,90,0,0), labeling_args = list(offset_labels = c(left = 1.5, top=0), offset_varnames = c(left = 4, top=1)))
  })
  
  output$proposed1 <- renderPlot({
    ggplot(datasetM(),aes(x=variable,y=value, fill = manufacturer)) +
      geom_boxplot() +
      labs(fill = "Miles",x="Manufacturer",y="Miles per gallon") + 
      ggtitle("Manufacturer influence") + 
      theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()
  })
}



shinyApp(ui, server)