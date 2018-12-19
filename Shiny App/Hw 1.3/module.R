###################################################
##########          Module         ################
###################################################

hw1.3_path <- "Hw 1.3/"
hw1.3_title <- "Cars Analysis"

# Load questions

source(str_c(hw1.3_path, "question1.R"))
source(str_c(hw1.3_path, "question2.R"))

# Data preprocessing

source(str_c(hw1.3_path, "data-preprocessing.R"))

# UI (tabs)

hw1.3_ui <- function(id) {
    ns <- NS(id)
    tabPanel("Homework 1.3", class = 'content-wrapper',
        navlistPanel(widths = c(2, 10), "Questions",
            hw1.3_q1_ui(id),
            hw1.3_q2_ui(id)
        )
    )
}

# Server

hw1.3_server <- function(input, output, session, outliers) {

    # Load reactives

    source(str_c(hw1.3_path, "reactives.R"), local = TRUE)

    # Call questions' servers

    hw1.3_q1_server(input, output, session, wines)
    hw1.3_q2_server(input, output, session, cars, cars.pca, cars.scores, cars.k3)
}
