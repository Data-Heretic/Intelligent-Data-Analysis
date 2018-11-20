###################################################
##########          Module         ################
###################################################

hw2.1_path <- "Hw 2.1/"

# Load questions

source(str_c(hw2.1_path, "question1.R"))
source(str_c(hw2.1_path, "question2.R"))
source(str_c(hw2.1_path, "question3a.R"))
source(str_c(hw2.1_path, "question3b.R"))
source(str_c(hw2.1_path, "question4.R"))

# Data preprocessing

source(str_c(hw2.1_path, "data-preprocessing.R"))

# Menu item

hw2.1_menuItem <- function(id) {
    ns <- NS(id)
    menuItem("Homework 2.1",
        menuItem("Question 1", tabName = str_c(id, "Q1"), icon = icon("comment", lib = "font-awesome")),
        menuItem("Question 2", tabName = str_c(id, "Q2"), icon = icon("comment", lib = "font-awesome")),
        menuItem("Question 3", tabName = str_c(id, "Q3"), icon = icon("comment", lib = "font-awesome"),
            menuItem("Section a)",
                    tabName = str_c(id, "Q3a"),
                    icon = icon("line-chart")),
            menuItem("Section b)",
                    tabName = str_c(id, "Q3b"),
                    icon = icon("line-chart")),
            menuItem("Section c)",
                    tabName = str_c(id, "Q3c"),
                    icon = icon("line-chart"))
        ),
        menuItem("Question 4", tabName = str_c(id, "Q4"), icon = icon("comment", lib = "font-awesome"))
    )
}

# UI (tabs)

hw2.1_ui <- function(id) {
    ns <- NS(id)
    list(hw2.1_q1_ui(id), hw2.1_q2_ui(id), hw2.1_q3a_ui(id), hw2.1_q3b_ui(id), hw2.1_q4_ui(id))
}

# Server

hw2.1_server <- function(input, output, session) {

    # Load reactives

    source(str_c(hw2.1_path, "reactives.R"), local = TRUE)

    # Call questions' servers

    hw2.1_q1_server(input, output, session)
    hw2.1_q2_server(input, output, session)
    hw2.1_q3a_server(input, output, session)
    hw2.1_q3b_server(input, output, session)
    hw2.1_q4_server(input, output, session)
}
