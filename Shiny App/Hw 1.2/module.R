###################################################
##########          Module         ################
###################################################

hw1.2_path <- "Hw 1.2/"
hw1.2_title <- "Wines Analysis"

# Load questions

source(str_c(hw1.2_path, "question1a.R"))
source(str_c(hw1.2_path, "question1b.R"))
source(str_c(hw1.2_path, "question1c.R"))
source(str_c(hw1.2_path, "question2.R"))

# Data preprocessing

source(str_c(hw1.2_path, "data-preprocessing.R"))

# UI (tabs)

hw1.2_ui <- function(id) {
    ns <- NS(id)
    tabPanel("Homework 1.2", class = 'content-wrapper',
        navlistPanel(widths = c(2, 10), "Questions",
            tabPanel(title = "Question 1", column(12,
                     h2(hw1.2_title),
                     tabsetPanel(hw1.2_q1a_ui(id, hw1.2_options()), hw1.2_q1b_ui(id, hw1.2_options()), hw1.2_q1c_ui(id, hw1.2_options()), type = "pills"))),
            hw1.2_q2_ui(id, hw1.2_options())
        )
    )
}

hw1.2_options <- function() {
    return(div(
        h4("Options"),
        checkboxInput("outliers", "Remove Outliers", value = FALSE)
    ))
}

# Server

hw1.2_server <- function(input, output, session, outliers) {

    # Load reactives

    source(str_c(hw1.2_path, "reactives.R"), local = TRUE)

    # Call questions' servers

    hw1.2_q1a_server(input, output, session, wines_TS02)
    hw1.2_q1b_server(input, output, session, mahalanobis24)
    hw1.2_q1c_server(input, output, session, wines_for_correlations)
    hw1.2_q2_server(input, output, session)
}
