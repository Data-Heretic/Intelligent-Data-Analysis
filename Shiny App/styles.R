###################################################
##########           Styles        ################
###################################################

styles <- function(id) {
    ns <- NS(id)
    tags$head(tags$style(HTML(".plot_engine2 {height: 1500px, width:500px}")))
}