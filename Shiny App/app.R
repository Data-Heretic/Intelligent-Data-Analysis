###################################################
##########           Libraries     ################
###################################################

library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(tseries) # Jarque Bera
library(lawstat)
library(testit)
library(vcd)
library(car) # Pooled groups
library(moments) # Skewness and kurtosis
library(MVN) # Comparing fitting to a normal distribution before and after the transformation with Mardia test 
library(mvoutlier)
library(Hmisc)
library(MXM)
library(corrplot)
library(corrgram)
library(ppcor)
library(acepack)

###################################################
##########           Sources       ################
###################################################

source("helpers.R")
source("styles.R")
source("layout.R")

###################################################
##########           UI            ################
###################################################

ui <- dashboardPage(
  header("Header"),
  dashboardSidebar(
    sidebar("Sidebar")
  ),
  dashboardBody(
    styles("Styles"),
    outliersInput("OutliersInput"),
    title("Title"),
    content("Content")
  )
)

###################################################
##########           Server        ################
###################################################

server <- function(input, output, session) {
    callModule(hw1_server, "HW1", reactive({ input$outliers }))
    callModule(hw2_server, "HW2", reactive({ input$outliers }))
}

shinyApp(ui, server)