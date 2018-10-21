###################################################
##########          Module         ################
###################################################

hw2_path <- "00 Hw 1.2/"

# Load questions

source(str_c(hw2_path, "question1a.R"))
source(str_c(hw2_path, "question1b.R"))
source(str_c(hw2_path, "question1c.R"))
source(str_c(hw2_path, "question2.R"))

# Data preprocessing

source(str_c(hw2_path, "data-preprocessing.R"))

# Menu item

hw2_menuItem <- function(id) {
    ns <- NS(id)
    menuItem("Homework 1.2",
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
        menuItem("Question 2", tabName = str_c(id, "Q2"), icon = icon("comment", lib = "font-awesome"))
    )
}

# UI (tabs)

hw2_ui <- function(id) {
    ns <- NS(id)
    list(hw2_q1a_ui(id), hw2_q1b_ui(id), hw2_q1c_ui(id), hw2_q2_ui(id))
}

# Server

hw2_server <- function(input, output, session, outliers) {

    # Load reactives

    source(str_c(hw2_path, "reactives.R"), local = TRUE)

    # Call questions' servers

    hw2_q1a_server(input, output, session, wines_TS02)
    hw2_q1b_server(input, output, session, mahalanobis24)
    hw2_q1c_server(input, output, session)
    hw2_q2_server(input, output, session)
}
