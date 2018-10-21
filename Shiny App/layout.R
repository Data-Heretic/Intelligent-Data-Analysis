###################################################
##########           Layout        ################
###################################################

source("00 Hw 1.1/module.R")
source("00 Hw 1.2/module.R")

# Header

header <- function(id) {
    ns <- NS(id)
    dashboardHeader(title = "Data Analysis",
        tags$li(a(href = 'https://www.fi.upm.es/',
                  img(src = 'Captura.png', title = "a", height = "31px", width = "96px"),
                  style = "padding-top:10px; padding-bottom:10px;"
                ), class = "dropdown"
        )
    )
}

# Menu

sidebar <- function(id) {
    ns <- NS(id)
    sidebarMenu(
        hw1_menuItem("HW1"),
        hw2_menuItem("HW2")
    )
}

# Outliers input

outliersInput <- function(id) {
    ns <- NS(id)
    tags$footer(checkboxInput("outliers", "Remove Outliers", value = FALSE))
}

# Title

title <- function(id) {
    ns <- NS(id)
    h2(HTML("<b> Mileage Analysis </b>"))
}

# Content

content <- function(id) {
    ns <- NS(id)
    do.call(tabItems, c(hw1_ui("HW1"), hw2_ui("HW2")))
}