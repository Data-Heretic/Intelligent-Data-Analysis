###################################################
##########          Module         ################
###################################################

hw3_path <- "Hw 1.3/"

# Load questions

source(str_c(hw3_path, "question1.R"))
source(str_c(hw3_path, "question2.R"))

# Data preprocessing

source(str_c(hw3_path, "data-preprocessing.R"))

# Menu item

hw3_menuItem <- function(id) {
    ns <- NS(id)
    menuItem("Homework 1.3",
        menuItem("Question 1", tabName = str_c(id, "Q1"), icon = icon("comment", lib = "font-awesome")),
        menuItem("Question 2", tabName = str_c(id, "Q2"), icon = icon("comment", lib = "font-awesome"))
    )
}

# UI (tabs)

hw3_ui <- function(id) {
    ns <- NS(id)
    list(hw3_q1_ui(id), hw3_q2_ui(id))
}

# Server

hw3_server <- function(input, output, session, outliers) {

    # Load reactives

    source(str_c(hw3_path, "reactives.R"), local = TRUE)

    # Call questions' servers

    hw3_q1_server(input, output, session, wines)
    hw3_q2_server(input, output, session, cars, cars.pca, cars.scores, cars.k3)
}
