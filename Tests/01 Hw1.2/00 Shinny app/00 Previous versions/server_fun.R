
body1 <- function (id){
  ns <- NS(id)
  tags$head(tags$style(HTML(".plot_engine2 {height: 1500px, width:500px}")))
}
body2 <- function(id){
  ns <- NS(id)
  tags$footer(checkboxInput("outliers", "Remove Outliers", value = FALSE))
}
body3 <- function(id){
  ns <- NS(id)
  h2(HTML("<b> Mileage Analysis </b>"))
}