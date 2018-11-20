###################################################
##########          Module         ################
###################################################

hw2.2_path <- "Hw 2.2/"

# Load questions

source(str_c(hw2.2_path, "question1.R"))

# Data preprocessing

source(str_c(hw2.2_path, "data-preprocessing.R"))

# Menu item

hw2.2_menuItem <- function(id) {
    ns <- NS(id)
    menuItem("Homework 2.2",
        menuItem("Question 1", tabName = str_c(id, "Q1"), icon = icon("comment", lib = "font-awesome"))
    )
}

# UI (tabs)

hw2.2_ui <- function(id) {
    ns <- NS(id)
    list(hw2.2_q1_ui(id))
}

# Server

hw2.2_server <- function(input, output, session) {

    # Load reactives

    source(str_c(hw2.2_path, "reactives.R"), local = TRUE)

    # Call questions' servers

    hw2.2_q1_server(input, output, session)
}
