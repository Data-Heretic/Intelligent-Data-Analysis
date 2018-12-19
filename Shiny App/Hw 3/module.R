###################################################
##########          Module         ################
###################################################

hw3_path <- "Hw 3/"
hw3_title <- "Coming soon..."

# Load questions

source(str_c(hw3_path, "question1.R"))
source(str_c(hw3_path, "question2.R"))
source(str_c(hw3_path, "question3a.R"))
source(str_c(hw3_path, "question3b.R"))
source(str_c(hw3_path, "question3c.R"))
source(str_c(hw3_path, "question3d.R"))
source(str_c(hw3_path, "question3e.R"))
source(str_c(hw3_path, "question3f.R"))
source(str_c(hw3_path, "question3g.R"))
source(str_c(hw3_path, "question3h.R"))

# Data preprocessing

source(str_c(hw3_path, "data-preprocessing.R"))

# UI (tabs)

hw3_ui <- function(id) {
    ns <- NS(id)
    tabPanel("Homework 3", class = 'content-wrapper',
        navlistPanel(widths = c(2, 10), "Questions",
            hw3_q1_ui(id, hw3_options()),
            hw3_q2_ui(id, hw3_options()),
            tabPanel(title = "Question 3", column(12, h2(hw3_title), tabsetPanel(
                hw3_q3a_ui(id, hw3_options()), hw3_q3b_ui(id, hw3_options()), hw3_q3c_ui(id, hw3_options()), hw3_q3d_ui(id, hw3_options()),
                hw3_q3e_ui(id, hw3_options()), hw3_q3f_ui(id, hw3_options()), hw3_q3g_ui(id, hw3_options()), hw3_q3h_ui(id, hw3_options()), type = "pills"))))
    )
}

hw3_options <- function() {
    return(div(
        h4("Options"),
        "Nothing yet"
    ))
}

# Server

hw3_server <- function(input, output, session) {

    # Load reactives

    source(str_c(hw3_path, "reactives.R"), local = TRUE)

    # Call questions' servers

    hw3_q1_server(input, output, session)
    hw3_q2_server(input, output, session)
    hw3_q3a_server(input, output, session)
    hw3_q3b_server(input, output, session)
    hw3_q3c_server(input, output, session)
    hw3_q3d_server(input, output, session)
    hw3_q3e_server(input, output, session)
    hw3_q3f_server(input, output, session)
    hw3_q3g_server(input, output, session)
    hw3_q3h_server(input, output, session)
}
