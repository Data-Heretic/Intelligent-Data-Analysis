## app.R ##
library(shiny)
library(shinydashboard)

mpg <- get(load("mpg.Rdata"))
mpg$tr <-substr(mpg$trans, 0, 1)
mpg$tr<-str_replace(mpg$tr,"^m","Manual") 
mpg$tr<-str_replace(mpg$tr,"^a","Automatic")



mpg<- mpg[mpg$fl!="c",]
mmpg<-melt(mpg,id=c("manufacturer","model","displ","year","cyl","trans","drv","fl","class","tr"))
mmpg$logv = log(mmpg$value)

mmpg$fl<-factor(mpg$fl,levels=c("p","d","e","r","c"),labels=c("petrol","diesel","ethanol","regular","mistake"))

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
    tabItems(
      tabItem(tabName = "Q1a",
              h2("Mileage Analysis"),
              h3("Is there a significant difference in city mileage between automatic and manual transmission cars?"),
              fluidPage(box(plotOutput("plot"),
                            selectInput("XPlot","Variable",c("Automatic/Manual"="tr", "City/Highway" = "variable"))),
                        box(plotOutput("plot2"),
                            selectInput("XPlot1","Variable",c("Original Data"="value", "Log Data" = "logv")), 
                            verbatimTextOutput("text1"),
                            verbatimTextOutput("text2"),
                            textOutput("text3"),
                            verbatimTextOutput("text4")
                            )
                        )
      ),
      tabItem(tabName = "Q1b",
              h2("Mileage Analysis"),
              h3("What about for highway mileage?")
      ),
      tabItem(tabName = "Q2",
              h2("Mileage Analysis"),
              h3("Has fuel type (or cylinders) any influence on highway mpg or city mpg?")
      ),
      tabItem(tabName = "Q3",
              h2("Mileage Analysis"),
              h3("Comparing the distribution of some of the variables in years 1999 and 2008, determine wether the requests on automobiles have been changed.")
      ),
      tabItem(tabName = "Q4",
              h2("Mileage Analysis"),
              h3("Proposed Question - 
                 Is there any difference in city and highway mpg between manufacturers?")
      )
    )
  )
)

server <- function(input, output) { 
  
  output$plot <- renderPlot({
    
    ggplot(mmpg,aes(x=eval(parse(text = input$XPlot)),y=value,fill=(if (input$XPlot == "tr") {variable} else {tr}))) +
      geom_boxplot() +
      labs(fill = "Miles",x="Type of transmission",y="NUmber of Miles") + 
      ggtitle("Mileage") + 
      theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()
    })
  
  output$plot2 <- renderPlot({
    
    ggplot(mmpg,aes(x=eval(parse(text = input$XPlot1)), fill = variable, colour = variable)) + geom_density(alpha = 0.2) + theme_minimal()
    
  })
  
  output$text1 <- renderPrint({
    var.test(mmpg[which(mmpg$variable == "cty"),][[input$XPlot1]] ~mpg[which(mmpg$variable == "cty"),]$tr)
  })
  
  output$text2 <- renderPrint({
    var.test(mmpg[which(mmpg$variable == "hwy"),][[input$XPlot1]] ~mpg[which(mmpg$variable == "cty"),]$tr)
  }) 
  
  output$text3 <- renderText({
    "We can accept that variables are the same"
  })
  output$text4 <- renderPrint({
    t.test(mmpg[[input$XPlot1]] ~ mmpg$variable) 
  }) 
  
}


shinyApp(ui, server)