SHeader <- function(id) {

    ns <- NS(id)

    dashboardHeader(title = "Data Analysis",
                 tags$li(a(href = 'https://www.fi.upm.es/',
                           img(src = 'Captura.png',
                               title = "a", height = "31px", width = "96px"),
                           style = "padding-top:10px; padding-bottom:10px;"),
                         class = "dropdown"))
}