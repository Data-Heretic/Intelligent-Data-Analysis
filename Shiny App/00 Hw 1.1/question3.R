###################################################
##########         Question 3        ##############
###################################################

# UI

hw1_q3_ui <- function(id) {
    ns <- NS(id)
    tabItem(tabName = str_c(id, "Q3"),
            h3("Comparing the distribution of some of the variables in years 1999 and 2008, determine whether the requests on automobiles have been changed."),
            h5("On the histogram plot we compare the distribution of the engine displacement for each automotive throught the years "),
            h5("On the scatterplot it is illustrated how the milage is affected by the type of transmission and the year of the car manufacturing."),

            fluidPage(fluidRow(box(plotOutput(ns("year"))),
                               box(plotOutput(ns("year3")))),
                      fluidRow(
                        h5("We have used a reactive bar chart to visualize how the different levels of the categorical variables are distributed between 1999 and 2008."),
                        box(column(3, div(style = "display: inline-block; margin-top: 25%", radioButtons(ns("radio"),

                                                                                                      label = "Variable",
                                                                                                      choices = list("Manufacturer" = 1,
                                                                                                                       "Fuel Type" = 2,
                                                                                                                       "Vehicle Class" = 3,
                                                                                                                       "Transmission" = 4,
                                                                                                                       "Drive Type" = 5,
                                                                                                                       "Cylinders" = 6
                                                                                                      ),
                                                                                                      selected = 1))),
                            column(8, (plotOutput(ns("year2")))), width = 60)),
                      fluidRow(
                        h5("A mosaic plot is a another useful graphical display that allows us to examine the relationship between one of the above mentioned categorical variables and the year of manufacturing.
                           The plot is accompanied by a Chi-squared test among the corresponding categorical variables."),
                        box(tableOutput(ns("ChiTest")), width = 5),
                        box(plotOutput(ns("mosaicPlot"), width = "100%"), width = 7))
                      )
    )
}

# Server

hw1_q3_server <- function(input, output, session, data, dataM) {

    # year
    output$year <- renderPlot({
        d1 <- density(data()$displ[data()$year == 1999])
        d2 <- density(data()$displ[data()$year == 2008])

        par(mfrow = c(1, 2))
        hist(data()$displ[data()$year == 1999], breaks = 10, probability = T, xlim = c(0, 8), ylim = c(0, 0.5), col = "gray", border = "white", main = "1999", xlab = "Engine displacement (L)")
        lines(d1, col = "red", lwd = 2)
        hist(data()$displ[data()$year == 2008], breaks = 10, probability = T, xlim = c(0, 8), ylim = c(0, 0.5), col = "gray", border = "white", main = "2008", xlab = "Engine displacement (L)", ylab = " ")
        lines(d2, col = "red", lwd = 2)
    })

    # year2
    output$year2 <- renderPlot({
        t1 <- data() %>%
            group_by(year, manufacturer) %>%
            summarize(Count1 = n())

        tfl <- data() %>%
        group_by(year, fl) %>%
        summarize(Count1 = n())

        tclass <- data() %>%
        group_by(year, class) %>%
        summarize(Count1 = n())

        ttr <- data() %>%
        group_by(year, tr) %>%
        summarize(Count1 = n())

        tdt <- data() %>%
        group_by(year, drv) %>%
        summarize(Count1 = n())

        tcyl <- data() %>%
        group_by(year, cyl) %>%
        summarize(Count1 = n())

        datos <- if (input$radio == '1') { t1 }
            else if (input$radio == '2') { tfl }
            else if (input$radio == '3') { tclass }
            else if (input$radio == '4') { ttr }
            else if (input$radio == '5') { tdt }
            else if (input$radio == '6') { tcyl }

        ggplot(data = datos,
             aes(x = eval(parse(text = colnames(datos)[2])), y = Count1, fill = factor(year))) +
        geom_bar(position = "dodge", stat = "identity") + scale_color_discrete("year") + theme_minimal() +
        xlab(colnames(datos)[2]) + ylab("Car nº")
    })

    # year3
    output$year3 <- renderPlot({
        ggplot(data = data(), aes(x = hwy, y = cty, fill = year, color = year)) +
        geom_point() +
        labs(x = "Highway Mileage(data)", y = "City Mileage(data)") +
        ggtitle("Mileage") +
        theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) + theme_minimal()   
    })

    # ChiTest
    output$ChiTest <- renderTable({
        t1 <- data() %>%
            group_by(year, manufacturer) %>%
            summarize(Count1 = n())

        tfl <- data() %>%
        group_by(year, fl) %>%
        summarize(Count1 = n())

        tclass <- data() %>%
        group_by(year, class) %>%
        summarize(Count1 = n())

        ttr <- data() %>%
        group_by(year, tr) %>%
        summarize(Count1 = n())

        tdt <- data() %>%
        group_by(year, drv) %>%
        summarize(Count1 = n())

        tcyl <- data() %>%
        group_by(year, cyl) %>%
        summarize(Count1 = n())

        datos <- if (input$radio == '1') { t1 }
            else if (input$radio == '2') { tfl }
            else if (input$radio == '3') { tclass }
            else if (input$radio == '4') { ttr }
            else if (input$radio == '5') { tdt }
            else if (input$radio == '6') { tcyl }

        a <- data.frame(Variable = 1:length(distinct(datos[2]) %>% pull()), Chi_Test_p_value = 1)
        for (i in 1:length(distinct(datos[2]) %>% pull())) {
            a[i, 1] = toString(((distinct(datos[2]) %>% pull())[i]))
            if (length(datos[which(datos[2] == toString(((distinct(datos[2]) %>% pull())[i]))), ]$Count1) == 1) {
                a[i, 2] = "NA"
            }
            else {
                a[i, 2] = chisq.test(datos[which(datos[2] == toString(((distinct(datos[2]) %>% pull())[i]))),]$Count1, p = c(1 / 2, 1 / 2))$p.value
            }
        }
        return(a)
    })

    # mosaicPlot
    output$mosaicPlot <- renderPlot({
        contigency_table <- if (input$radio == '1') { xtabs(~year + manufacturer, data = data()) }
        else if (input$radio == '2') { xtabs(~year + fl, data = data()) }
        else if (input$radio == '3') { xtabs(~year + class, data = data()) }
        else if (input$radio == '4') { xtabs(~year + tr, data = data()) }
        else if (input$radio == '5') { xtabs(~year + drv, data = data()) }
        else if (input$radio == '6') { xtabs(~year + cyl, data = data()) }
        mosaic(contigency_table, gp = shading_max, split_vertical = TRUE, rot_labels = c(0, 90, 0, 0), labeling_args = list(offset_labels = c(left = 1.5, top = 0), offset_varnames = c(left = 4, top = 1)))
    })
}