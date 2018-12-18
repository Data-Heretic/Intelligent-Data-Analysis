SMenu <- function(id) {
  ns <- NS(id)
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
}