## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)
library(ggplot2)
library(gridExtra)
library(reshape2)

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
  dashboardHeader( title = "H1.1 - Mileage"),
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
    tabItems(
      tabItem(tabName = "Q1a",
              h2("Mileage Analysis"),
              h3("Is there a significant difference in city mileage between automatic and manual transmission cars?"),
              fluidPage(
                # fluidRow(checkboxInput("outliers", "Remove Outliers", value = FALSE)),
                fluidRow(box(plotOutput("plot_cty")),
                        box(plotOutput("plot2_cty"),
                            selectInput("XPlot1_cty","Logarithmic or normal variable?",c("Original Data"="cty", "Log Data" = "log(cty)"))
                        )),
                        fluidRow(box(plotOutput("Errors1"))),
                        fluidRow(
                                 verbatimTextOutput("text1"),
                                 textOutput("text2"),
                                 verbatimTextOutput("text3")
                        )
      )),

      tabItem(tabName = "Q1b",
              h2("Mileage Analysis"),
              h3("What about for highway mileage?"),
              fluidPage(
                        fluidRow(box(plotOutput("plot_hwy")),
                        box(plotOutput("plot2_hwy"),
                            selectInput("XPlot1_hwy","Logarithmic or normal variable?",c("Original Data"="hwy", "Log Data" = "log(hwy)")))
              ))

      ),
      tabItem(tabName = "Q1c",
              h2("Mileage Analysis"),
              h3("What about engine displacement in litres?"),
              fluidPage(fluidRow(box(plotOutput("plot_displ")),
                                 box(plotOutput("plot2_displ"),
                                     selectInput("XPlot1_displ","Logarithmic or normal variable?",c("Original Data"="displ", "Log Data" = "log(displ)")))
              ))
              
      ),
      tabItem(tabName = "Q2",
              h2("Mileage Analysis"),
              h3("Has fuel type (or cylinders) any influence on highway mpg or city mpg?"),
              fluidPage(
                        fluidRow(box(plotOutput("plot_engine")),
                                 box(plotOutput("plot_engine2"))
              ))
      ),
      tabItem(tabName = "Q3",
              h2("Mileage Analysis"),
              h3("Comparing the distribution of some of the variables in years 1999 and 2008, determine wether the requests on automobiles have been changed."),
              fluidPage(fluidRow(box(plotOutput("year"))),
                        fluidRow(box(column(3,div(style="display: inline-block; margin-top: 25%", radioButtons("radio",
                                                           
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
                        fluidRow(box(tableOutput("ChiTest")))
              )
      ),
      
      tabItem(tabName = "Q4",
              h2("Mileage Analysis"),
              h3("Proposed Question - 
                 Is there any difference in city and highway mpg between manufacturers?"),
              fluidPage(
                        fluidRow(plotOutput("proposed1")))
              
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
    # mmpg$logv <- log(mmpg$value)
    mmpg$variable<- factor(mmpg$variable, levels=c("cty", "hwy"), labels = c("City Mileage", "Highway Mileage") )
    mmpg$fl<-factor(mmpg$fl,levels=c("p","d","e","r"),labels=c("Petrol","Diesel","Ethanol","Regular"))
    return(mmpg)
  })
  
  output$Errors1 <- renderPlot({
    qqnorm(dataset()$cty - mean(dataset()$cty))
    qqline(dataset()$cty - mean(dataset()$cty)) # qqline adds a line to a normal quantile-quantile plot which passes through the first and third quartiles
  })
  
  output$plot_cty <- renderPlot({
    
    ggplot(data = dataset(),aes(x=tr,y=cty, fill = tr)) +
      geom_boxplot() +
      labs(fill = "Miles",x="Type of transmission",y="NUmber of Miles") + 
      ggtitle("Mileage") + 
      theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()
    })
  
  output$plot_hwy <- renderPlot({
    
    ggplot(data = dataset(),aes(x=tr,y=hwy, fill = tr)) +
      geom_boxplot() +
      labs(fill = "Miles",x="Type of transmission",y="NUmber of Miles") + 
      ggtitle("Mileage") + 
      theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()
  })
  
  output$plot_displ <- renderPlot({
    
    ggplot(mpg,aes(x=tr, y = displ, fill =tr)) +
      geom_boxplot() +
      labs(fill = "Litres",x="Type of transmission",y="NUmber of Litres") + 
      ggtitle("Mileage") + 
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
    "test of assumptions"
  })
  
  output$text2 <- renderText({
    "Should we apply ANOVA or a non-parametric test?"
  })
  output$text3 <- renderPrint({
    "test results" 
  }) 
  
  output$plot_engine <- renderPlot({
    
    ggplot(data = datasetM(),aes(x=variable,y=value,fill=fl))+geom_boxplot()+labs(fill = "Type of engine",x=" ",y="NUmber of Miles")
  
  })

  output$plot_engine2 <- renderPlot({
    
    ggplot(datasetM(),aes(x=variable,y=value,fill=as.factor(cyl)))+geom_boxplot()+labs(fill = "Number of cylinders",x=" ",y="NUmber of Miles")
    
  })
  output$year <- renderPlot({
    
    d1<-density(mpg$displ[mpg$year==1999])
    d2<-density(mpg$displ[mpg$year==2008])
    
    par(mfrow=c(1, 2))
    hist(mpg$displ[mpg$year==1999],breaks=10 ,probability = T,xlim=c(0, 8),ylim=c(0,0.5),col="gray", border="white",main="1999",xlab="cc")
    lines(d1,col="red",lwd=2)
    hist(mpg$displ[mpg$year==2008],breaks=10 ,probability = T,xlim=c(0, 8),ylim=c(0,0.5),col="gray", border="white",main="2008",xlab="cc",ylab=" ")
    lines(d2,col="red",lwd=2)
    
    
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
               xlab(colnames(datos)[2]) + ylab("Nº de Coches")
    
    
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
    a <- matrix(ncol=length(distinct(datos[2])%>% pull()), nrow=1)
    for (i in 1:length(distinct(datos[2])%>% pull())){
      a[1,i] = (if (has_error(chisq.test(datos[which(datos[2]==((distinct(datos[2]) %>% pull())[i])),]$Count1 )$p.value)) {"NA"}
                else {chisq.test(datos[which(datos[2]==((distinct(datos[2]) %>% pull())[i])),]$Count1)$p.value})
    }
    return(a)
  })  
  output$proposed1 <- renderPlot({
    ggplot(datasetM(),aes(x=variable,y=value, fill = manufacturer)) +
      geom_boxplot() +
      labs(fill = "Miles",x="Manufacturer",y="NUmber of Miles") + 
      ggtitle("Manufacturer influence") + 
      theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()
  })
}



shinyApp(ui, server)