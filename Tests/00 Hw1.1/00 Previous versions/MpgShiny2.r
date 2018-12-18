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

mmpg$fl<-factor(mpg$fl,levels=c("p","d","e","r","c"),labels=c("petrol","diesel","ethanol","regular","mistake"))



# User interface ----
ui <- fluidPage(
  titlePanel("Simple Mileage Analysis"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select options to examine."),
      
      radioButtons("radio",label="Question 1",
                   choices = list(  "Automatic~Manual"= 1 ,"Cty~Hwy" = 2), 
                   selected = 1),
      br(),
      radioButtons("radio2",label="Question 2",
                   choices = list(  "Fuel Type"= 1 ,"Number of Cylinders" = 2), 
                   selected = 1),
      br(),
      h4("select number for question 3"),
      sliderInput("slider", label = h3("Slider"), min = 0, 
                  max = 50, value = 20)
    
    ),
    
    mainPanel(h3("Barplot of results"),
              tabsetPanel(id = "Panels",
                          tabPanel("Question 1",
                                   conditionalPanel(
                                      condition = "input.radio == '1'",plotOutput("plot")),
                                   conditionalPanel(
                                      condition = "input.radio == '2'",plotOutput("plot2"))
                                  ),
                          tabPanel("Question2",
                                   conditionalPanel(
                                     condition = "input.radio2 == '1'",plotOutput("plot3")),
                                   conditionalPanel(
                                     condition = "input.radio2 == '2'",plotOutput("plot4"))
                                   
                                   ),
                          tabPanel("Question3",
                                   h3("Engine displacement in litres per year"),
                                   plotOutput( "plot5")
                          )
                          )
              
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
  output$plot3 <- renderPlot({
    
    ggplot(mmpg,aes(x=variable,y=value,fill=fl))+geom_boxplot()+labs(fill = "Type of engine",x=" ",y="NUmber of Miles")
    
  })
  
  output$plot4 <- renderPlot({
    
    ggplot(mmpg,aes(x=variable,y=value,fill=as.factor(cyl)))+geom_boxplot()+labs(fill = "Number of cylinders",x=" ",y="NUmber of Miles")
  })
  dataInput<-reactive(input$slider)
  
  output$plot5 <- renderPlot({
    sl<-dataInput()
    d1<-density(mpg$displ[mpg$year==1999])
    d2<-density(mpg$displ[mpg$year==2008])
    
    par(mfrow=c(1, 2))
    hist(mpg$displ[mpg$year==1999],breaks=sl ,probability = T,xlim=c(0, 8),ylim=c(0,0.5),col="gray", border="white",main="1999",xlab="cc")
    lines(d1,col="red",lwd=2)
    hist(mpg$displ[mpg$year==2008],breaks=sl ,probability = T,xlim=c(0, 8),ylim=c(0,0.5),col="gray", border="white",main="2008",xlab="cc",ylab=" ")
    lines(d2,col="red",lwd=2)
    
    
  })
}

# Run the app
shinyApp(ui, server)