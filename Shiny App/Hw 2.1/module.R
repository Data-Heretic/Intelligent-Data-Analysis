###################################################
##########          Module         ################
###################################################

hw2.1_path <- "Hw 2.1/"
hw2.1_title <- "Diamonds"

# Load questions

source(str_c(hw2.1_path, "question1.R"))
source(str_c(hw2.1_path, "question2.R"))
source(str_c(hw2.1_path, "question3a.R"))
source(str_c(hw2.1_path, "question3b.R"))
source(str_c(hw2.1_path, "question4.R"))

# Data preprocessing

source(str_c(hw2.1_path, "data-preprocessing.R"))

# UI (tabs)

hw2.1_ui <- function(id) {
    ns <- NS(id)
    tabPanel("Homework 2.1", class = 'content-wrapper',
        navlistPanel(widths = c(1, 11), "Questions",
            hw2.1_q1_ui(id),
            hw2.1_q2_ui(id),
            tabPanel(title = "Question 3", column(12, h2(hw2.1_title), tabsetPanel(hw2.1_q3a_ui(id), hw2.1_q3b_ui(id), type = "pills"))),
            hw2.1_q4_ui(id)))
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
