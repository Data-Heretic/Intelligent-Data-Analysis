###################################################
##########          HW1 Module         ############
###################################################

hw1_path <- "Hw 1.1/"

# Load questions

source(str_c(hw1_path, "question1a.R"))
source(str_c(hw1_path, "question1b.R"))
source(str_c(hw1_path, "question1c.R"))
source(str_c(hw1_path, "question2.R"))
source(str_c(hw1_path, "question3.R"))
source(str_c(hw1_path, "question4.R"))

# Data preprocessing

source(str_c(hw1_path, "data-preprocessing.R"), local = TRUE)

# Menu item

hw1_menuItem <- function(id) {
    ns <- NS(id)
    menuItem("Homework 1.1",
        checkboxInput("outliers", "Remove Outliers", value = FALSE),
        menuItem("Question 1", tabName = str_c(id, "Q1"), icon = icon("comment", lib = "font-awesome"),
            menuItem("a",
                    tabName = str_c(id, "Q1a"),
                    icon = icon("line-chart")),
            menuItem("b",
                    tabName = str_c(id, "Q1b"),
                    icon = icon("line-chart")),
            menuItem("c",
                    tabName = str_c(id, "Q1c"),
                    icon = icon("line-chart"))
        ),
        menuItem("Question 2", tabName = str_c(id, "Q2"), icon = icon("comment", lib = "font-awesome")),
        menuItem("Question 3", tabName = str_c(id, "Q3"), icon = icon("comment", lib = "font-awesome")),
        menuItem("Question 4", tabName = str_c(id, "Q4"), icon = icon("comment", lib = "font-awesome"))
    )
}

# UI (tabs)

hw1_ui <- function(id) {
    ns <- NS(id)
    list(hw1_q1a_ui(id), hw1_q1b_ui(id), hw1_q1c_ui(id), hw1_q2_ui(id), hw1_q3_ui(id), hw1_q4_ui(id))
}

# Server

hw1_server <- function(input, output, session, outliers) {

    # Load reactives

    source(str_c(hw1_path, "reactives.R"), local = TRUE)
    
    # Call questions' servers

    hw1_q1a_server(input, output, session, dataset, datasetM)
    hw1_q1b_server(input, output, session, dataset, datasetM)
    hw1_q1c_server(input, output, session, dataset, datasetM)
    hw1_q2_server(input, output, session, dataset, datasetM)
    hw1_q3_server(input, output, session, dataset, datasetM)
    hw1_q4_server(input, output, session, dataset, datasetM)
}
