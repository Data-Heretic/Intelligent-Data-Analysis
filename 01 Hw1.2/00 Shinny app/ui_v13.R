###################################################
###################################################
##########           Libraries     ################
###################################################
###################################################
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

###################################################
###################################################
##########           Sources       ################
###################################################
###################################################

source("helpers.R")
source("Header.R")
source("Sidebar.R")
source("body.R")
source("server_fun.R")

###################################################
###################################################
##########           Main Data     ################
###################################################
###################################################

# Hw1 Data
mpg <- get(load("mpg.Rdata"))

mpg$tr <- substr(mpg$trans, 0, 1)
mpg$tr <- str_replace(mpg$tr, "^m", "Manual")
mpg$tr <- str_replace(mpg$tr, "^a", "Automatic")

mpg <- mpg[mpg$fl != "c",]

mpg$year = as.factor(mpg$year)
mpg$drv = as.factor(mpg$drv)
mpg$cyl = as.factor(mpg$cyl)
mpg$fl = as.factor(mpg$fl)
mpg$class = as.factor(mpg$class)

###################################################
###################################################
##########           UI            ################
###################################################
###################################################

ui <- dashboardPage(
  SHeader("H1"),
  dashboardSidebar(
    SMenu("S1")
  ),
  dashboardBody(
    tags$footer(checkboxInput("outliers", "Remove Outliers", value = FALSE)),
    body1("B1"),
    body3("B3"),
    body4UI("B4")
  )
)

###################################################
###################################################
##########           Server        ################
###################################################
###################################################

server <- function(input, output, session) {

    dataset <- reactive({
        if (input$outliers) {
            temp <- mpg
            temp$cty <- remove_outliers(temp$cty)
            temp$hwy <- remove_outliers(temp$hwy)
            temp$displ <- remove_outliers(temp$displ)
            temp <- temp[complete.cases(temp),]
            return(temp)
        }
        else {
            return(mpg)
        }
    })

    datasetM <- reactive({
        mmpg <- melt(dataset(), id = c("manufacturer", "model", "displ", "year", "cyl", "trans", "drv", "fl", "class", "tr"))
        mmpg$variable <- factor(mmpg$variable, levels = c("cty", "hwy"), labels = c("City Mileage", "Highway Mileage"))
        mmpg$fl <- factor(mmpg$fl, levels = c("p", "d", "e", "r"), labels = c("Petrol", "Diesel", "Ethanol", "Regular"))
        return(mmpg)
    })



    #### PLOTS #####  



    callModule(body4, "B4", dataset, datasetM)




}



shinyApp(ui, server)