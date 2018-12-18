# Load packages ----
library(shiny)
library(stringr)
library(ggplot2)
library(gridExtra)
library(plyr)
library(reshape)

mpg <- get(load("mpg.Rdata"))
mpg$tr <-substr(mpg$trans, 0, 1)
mpg$tr<-str_replace(mpg$tr,"^m","Manual") 
mpg$tr<-str_replace(mpg$tr,"^a","Automatic")

mpg<- mpg[mpg$fl!="c",]

mmpg<-melt(mpg,id=c("manufacturer","model","displ","year","cyl","trans","drv","fl","class","tr"))


# User interface ----
ui <- fluidPage(
  titlePanel("Simple Mileage Analysis"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select what do you want to place on the x axis to examine."),
      
      radioButtons("radio",label="X axis",
                   choices = list(  "Automatic~Manual"= 1 ,"Cty~Hwy" = 2), 
                   selected = 1),
      br()
      
    ),
    
    mainPanel(h3("Barplot of results"),
              
              conditionalPanel(
                condition = "input.radio == '1'",plotOutput("plot")),
              conditionalPanel(
                condition = "input.radio == '2'",plotOutput("plot2"))
                
                
              )
    )
  )


# Server logic
server <- function(input, output) {
  dataInput<- reactive({ input$radio })
  
  
  output$plot <- renderPlot({
    
      ggplot(mmpg,aes(x=tr,y=value,fill=variable))+geom_boxplot()+labs(fill = "Miles",x="Type of transmission",y="NUmber of Miles")
      
  })
  
  output$plot2 <- renderPlot({
    
    ggplot(mmpg,aes(x=variable,y=value,fill=tr))+geom_boxplot()+labs(fill = "Miles",x="Type of transmission",y="NUmber of Miles")
    
  })
  
}

# Run the app
shinyApp(ui, server)