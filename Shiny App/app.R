###################################################
##########           Libraries     ################
###################################################

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders) #for loading icon
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
library(corrplot)
library(corrgram)
library(ppcor)
library(WVPlots)
library(FactoMineR) #load FactoMineR and perform a PCA analysis on matrix R.
library(factoextra)
library(GGally)
library(forcats) # for merging

###################################################
##########           Sources       ################
###################################################

# Resources

source("helpers.R")

# Homework modules

source("Hw 1.1/module.R")
source("Hw 1.2/module.R")
source("Hw 1.3/module.R")
source("Hw 2.1/module.R")
source("Hw 2.2/module.R")

###################################################
##########           UI            ################
###################################################

ui <- navbarPage(
        title = "Data Analysis",
        position = "fixed-top",
        theme = shinytheme("journal"),
        hw1.1_ui("HW1_1"),
        hw1.2_ui("HW1_2"),
        hw1.3_ui("HW1_3"),
        hw2.1_ui("HW2_1"),
        hw2.2_ui("HW2_2"),
        footer = div(HTML("<span>&copy; Data Heretic 2018</span>"), class = 'footer'),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")))

###################################################
##########           Server        ################
###################################################

server <- function(input, output, session) {
    callModule(hw1.1_server, "HW1_1", reactive({ input$outliers }))
    callModule(hw1.2_server, "HW1_2", reactive({ input$outliers }))
    callModule(hw1.3_server, "HW1_3", reactive({ input$outliers }))
    callModule(hw2.1_server, "HW2_1")
    callModule(hw2.2_server, "HW2_2")
}

shinyApp(ui, server)