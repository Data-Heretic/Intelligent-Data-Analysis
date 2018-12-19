###################################################
##########          Module         ################
###################################################

hw2.2_path <- "Hw 2.2/"
hw2.2_title <- "Elk crossing"

# Load questions

source(str_c(hw2.2_path, "question1.R"))
source(str_c(hw2.2_path, "question2.R"))
source(str_c(hw2.2_path, "question3.R"))
source(str_c(hw2.2_path, "question4.R"))

# Data preprocessing

source(str_c(hw2.2_path, "data-preprocessing.R"))

# UI (tabs)

hw2.2_ui <- function(id) {
    ns <- NS(id)
    tabPanel("Homework 2.2", class = 'content-wrapper',
        navlistPanel(widths = c(2, 10), "Questions", hw2.2_q1_ui(id), hw2.2_q2_ui(id), hw2.2_q3_ui(id), hw2.2_q4_ui(id)))
}

# Server

hw2.2_server <- function(input, output, session) {

    # Load reactives

    source(str_c(hw2.2_path, "reactives.R"), local = TRUE)

    # Call questions' servers

    hw2.2_q1_server(input, output, session)
    hw2.2_q2_server(input, output, session)
    hw2.2_q3_server(input, output, session)
    hw2.2_q4_server(input, output, session)
}
