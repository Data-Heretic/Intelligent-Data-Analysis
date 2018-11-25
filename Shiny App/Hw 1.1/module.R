###################################################
##########          HW1 Module         ############
###################################################

hw1.1_path <- "Hw 1.1/"
hw1.1_title <- "Mileage Analysis"

# Load questions

source(str_c(hw1.1_path, "question1a.R"))
source(str_c(hw1.1_path, "question1b.R"))
source(str_c(hw1.1_path, "question1c.R"))
source(str_c(hw1.1_path, "question2.R"))
source(str_c(hw1.1_path, "question3.R"))
source(str_c(hw1.1_path, "question4.R"))

# Data preprocessing

source(str_c(hw1.1_path, "data-preprocessing.R"), local = TRUE)

# UI (tabs)

hw1.1_ui <- function(id) {
    ns <- NS(id)
    tabPanel("Homework 1.1", class = 'content-wrapper',
        navlistPanel(widths = c(1, 11), "Questions",
            tabPanel(title = "Question 1", column(12, h2(hw1.1_title),
                     tabsetPanel(hw1.1_q1a_ui(id, hw1.1_options()), hw1.1_q1b_ui(id, hw1.1_options()), hw1.1_q1c_ui(id, hw1.1_options()), type = "pills"))),
            hw1.1_q2_ui(id, hw1.1_options()),
            hw1.1_q3_ui(id, hw1.1_options()),
            hw1.1_q4_ui(id, hw1.1_options())
        )
    )
}

hw1.1_options <- function() {
    return(div(
        h4("Options"),
        checkboxInput("outliers", "Remove Outliers", value = FALSE)
    ))
}

# Server

hw1.1_server <- function(input, output, session, outliers) {

    # Load reactives

    source(str_c(hw1.1_path, "reactives.R"), local = TRUE)

    # Call questions' servers

    hw1.1_q1a_server(input, output, session, dataset, datasetM)
    hw1.1_q1b_server(input, output, session, dataset, datasetM)
    hw1.1_q1c_server(input, output, session, dataset, datasetM)
    hw1.1_q2_server(input, output, session, dataset, datasetM)
    hw1.1_q3_server(input, output, session, dataset, datasetM)
    hw1.1_q4_server(input, output, session, dataset, datasetM)
}
