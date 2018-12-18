## app.R ##
library(shiny)
library(shinydashboard)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(reshape)

mpg <- get(load("mpg.Rdata"))
mpg$tr <-substr(mpg$trans, 0, 1)
mpg$tr<-str_replace(mpg$tr,"^m","Manual") 
mpg$tr<-str_replace(mpg$tr,"^a","Automatic")





mpg<- mpg[mpg$fl!="c",]
mmpg<-melt(mpg,id=c("manufacturer","model","displ","year","cyl","trans","drv","fl","class","tr"))
mmpg$logv <- log(mmpg$value)
mmpg$variable<- factor(mmpg$variable, levels=c("cty", "hwy"), labels = c("City Mileage", "Highway Mileage") )

mmpg$fl<-factor(mmpg$fl,levels=c("p","d","e","r","c"),labels=c("Petrol","Diesel","Ethanol","Regular","Mistake"))

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
                        icon = icon("line-chart"))),
      menuItem("Question 2", tabName = "Q2", icon = icon("comment", lib = "font-awesome")),
      menuItem("Question 3", tabName = "Q3", icon = icon("comment", lib = "font-awesome")),
      menuItem("Question 4", tabName = "Q4", icon = icon("comment", lib = "font-awesome"))
    )
  ),
  dashboardBody( 
    tags$head(tags$style(HTML(".plot_engine2 {height: 1500px, width:500px}"))),
    tabItems(
      tabItem(tabName = "Q1a",
              h2("Mileage Analysis"),
              h3("Is there a significant difference in city mileage between automatic and manual transmission cars?"),
              fluidPage(fluidRow(box(plotOutput("plot_cty")),
                        box(plotOutput("plot2_cty"),
                            selectInput("XPlot1_cty","Variable",c("Original Data"="value", "Log Data" = "logv"))
                        )),
                        fluidRow(
                                 verbatimTextOutput("text1"),
                                 textOutput("text2"),
                                 verbatimTextOutput("text3")
                        )
      )),

      tabItem(tabName = "Q1b",
              h2("Mileage Analysis"),
              h3("What about for highway mileage?"),
              fluidPage(fluidRow(box(plotOutput("plot_hwy")),
                        box(plotOutput("plot2_hwy"),
                            selectInput("XPlot1_hwy","Variable",c("Original Data"="value", "Log Data" = "logv")))
              ))

      ),
      tabItem(tabName = "Q2",
              h2("Mileage Analysis"),
              h3("Has fuel type (or cylinders) any influence on highway mpg or city mpg?"),
              fluidPage(fluidRow(box(plotOutput("plot_engine")),
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
                                     column(8, (plotOutput("year2"))), width = 60))
              )
      ),
      
      tabItem(tabName = "Q4",
              h2("Mileage Analysis"),
              h3("Proposed Question - 
                 Is there any difference in city and highway mpg between manufacturers?"),
              fluidPage(fluidRow(box(plotOutput("proposed1"))))
              
      )
    )
  )
)

server <- function(input, output) { 
  
  output$plot_cty <- renderPlot({
    
    ggplot(mmpg[which(mmpg$variable == "City Mileage"),],aes(x=tr,y=value, fill = variable)) +
      geom_boxplot() +
      labs(fill = "Miles",x="Type of transmission",y="NUmber of Miles") + 
      ggtitle("Mileage") + 
      theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()
    })
  
  output$plot_hwy <- renderPlot({
    
    ggplot(mmpg[which(mmpg$variable == "Highway Mileage"),],aes(x=tr,y=value, fill = variable)) +
      geom_boxplot() +
      labs(fill = "Miles",x="Type of transmission",y="NUmber of Miles") + 
      ggtitle("Mileage") + 
      theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()
  })
  

  
  output$plot2_cty <- renderPlot({
    
    ggplot(mmpg[which(mmpg$variable == "City Mileage"),],
           aes(x=eval(parse(text = input$XPlot1_cty)), fill = tr, colour = tr)) + 
           geom_density(alpha = 0.2) + theme_minimal() + xlab(if(input$XPlot1_cty == "value") {"City Mileage"} else {"log(City Mileage)"})
    
  })
  
  output$plot2_hwy <- renderPlot({
    
    ggplot(mmpg[which(mmpg$variable == "Highway Mileage"),],
           aes(x=eval(parse(text = input$XPlot1_hwy)), fill = tr, colour = tr)) + 
          geom_density(alpha = 0.2) + theme_minimal() + xlab(if(input$XPlot1_hwy == "value") {"Highway Mileage"} else {"log(Highway Mileage)"})
    
  })
  
  output$text1 <- renderPrint({
    var.test(mmpg[which(mmpg$variable == "City Mileage"),][[input$XPlot1_cty]] ~mpg[which(mmpg$variable == "City Mileage"),]$tr)
  })
  
  output$text2 <- renderText({
    "We can accept that variables are the same"
  })
  output$text3 <- renderPrint({
    t.test(mmpg[[input$XPlot1_cty]] ~ mmpg$variable) 
  }) 
  
  output$plot_engine <- renderPlot({
    
    ggplot(mmpg,aes(x=variable,y=value,fill=fl))+geom_boxplot()+labs(fill = "Type of engine",x=" ",y="NUmber of Miles")
  
  })

  output$plot_engine2 <- renderPlot({
    
    ggplot(mmpg,aes(x=variable,y=value,fill=as.factor(cyl)))+geom_boxplot()+labs(fill = "Number of cylinders",x=" ",y="NUmber of Miles")
    
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
    
    t1<- mpg %>%
      group_by(year,manufacturer) %>%
      summarize(Count1 = n()) 
    
    tfl<- mpg %>%
      group_by(year,fl) %>%
      summarize(Count1 = n()) 
    
    tclass<- mpg %>%
      group_by(year,class) %>%
      summarize(Count1 = n()) 
    
    ttr<- mpg %>%
      group_by(year,tr) %>%
      summarize(Count1 = n()) 
    
    tdt <- mpg %>%
      group_by(year,drv) %>%
      summarize(Count1 = n()) 
    
    tcyl <- mpg %>%
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
  
  output$proposed1 <- renderPlot({
    ggplot(mmpg,aes(x=variable,y=value, fill = manufacturer)) +
      geom_boxplot() +
      labs(fill = "Miles",x="Manufacturer",y="NUmber of Miles") + 
      ggtitle("Manufacturer influence") + 
      theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()
  })
}



shinyApp(ui, server)