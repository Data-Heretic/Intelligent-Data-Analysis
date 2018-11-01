###################################################
##########           Layout        ################
###################################################

source("Hw 1.1/module.R")
source("Hw 1.2/module.R")
source("Hw 1.3/module.R")

# Header

header <- function(id) {
    ns <- NS(id)
    dashboardHeader(title = "Data Analysis",
                    dropdownMenu(type = "messages", icon = icon("user"),
                                 messageItem(from = "Data Heretics", "Welcome to our shiny dashboard")),
                    dropdownMenu(type="notifications",icon = icon("warning"),badgeStatus = "warning",
                                 notificationItem("Hw 1.3 takes time to load,be patient.")),
                    dropdownMenu(type = "tasks", badgeStatus = "primary", icon = icon("tasks"),
                                 taskItem(value = 57, color = "aqua", "HW 1.3 coming soon")
                                 ),
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
        hw2_menuItem("HW2"),
        hw3_menuItem("HW3")
    )
}

# Content

content <- function(id) {
    ns <- NS(id)
    do.call(tabItems, c(hw1_ui("HW1"), hw2_ui("HW2"), hw3_ui("HW3")))
}