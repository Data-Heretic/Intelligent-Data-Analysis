###################################################
##########          Module         ################
###################################################

hw2.2_path <- "Hw 2.2/"
hw2.2_title <- "Temporary title"

# Load questions

source(str_c(hw2.2_path, "question1.R"))

# Data preprocessing

source(str_c(hw2.2_path, "data-preprocessing.R"))

# UI (tabs)

hw2.2_ui <- function(id) {
    ns <- NS(id)
    tabPanel("Homework 2.2", class = 'content-wrapper',
        navlistPanel(widths = c(2, 10), "Questions", hw2.2_q1_ui(id, hw2.2_options()))
    )
}

hw2.2_options <- function() {
    return(div(
        h4("Options"),
        "Nothing yet"
    ))
}

# Server

hw2.2_server <- function(input, output, session) {

    # Load reactives

    source(str_c(hw2.2_path, "reactives.R"), local = TRUE)

    # Call questions' servers

    hw2.2_q1_server(input, output, session)
}
