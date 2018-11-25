###################################################
##########         Question 2        ##############
###################################################

# UI

hw1.2_q2_ui <- function(id) {
    ns <- NS(id)
    tabPanel(title = "Question 2",
        column(10,
            h2(hw1.2_title),
            h3("Permutation test"),
            p(HTML("With Permutation testing the reference distribution is generated from the data themselves, instead of comparing the
                actual value of a test statistic to a standard statistical distribution. Permutation provides an efficient
               approach to test when the data do not conform to the distributional assumptions of the
               statistical method one wants to use (e.g. normality).<br>
               The null hypothesis is that correlation is equal to 0. This means that there is no linear
               relationship between the two variables. If that is true, then any of the Y observations is just
               as likely to appear with any of the X's. In other words, Y<sub>i</sub> is just as likely to appear with X<sub>i</sub>
               as it is to appear with X<sub>j</sub>, i&ne;j.")),
            fluidRow(
                box(plotOutput(ns("plot_tips_pctTip"))), box(plotOutput(ns("histogram_prob_random")))),
            fluidRow(
                box(width = 6, title = h5("Correlation"), verbatimTextOutput(ns("correlation"))),
                box(width = 6, title = h5("Probability randomized"),
                    verbatimTextOutput(ns("probability_randomized")),
                    verbatimTextOutput(ns("p_value_explanation")), align = "center"))
        ),
        column(2, box(width = 12, class = 'well box-options', h4("Options"),
            checkboxInput(ns("remove_30_percentage"), "Repeat the analysis deleting the values for three customers that left a tip greater than 30% of the bill? Are these generous customers outliers?", value = FALSE)))
    )
}

# Server

hw1.2_q2_server <- function(input, output, session) {
  
  # Reactive object for RestaurantTips dataset
  datasetR <- reactive({
    if (input$remove_30_percentage) {
      temp <- restaurantTips[-which(restaurantTips$PctTip > 30),]
      return(temp)
    }
    else {
      return(restaurantTips)
    }
  })
  

    # plot tips ~ pctTip
    output$plot_tips_pctTip <- renderPlot({
        return(plot(datasetR()$Bill ~ datasetR()$PctTip, col = "blue", ylab = "Amount of Bill", xlab = "Percentage Tip"))
    })

    # Correlation
    output$correlation <- renderPrint({
        r.obt <- rcorr(as.matrix(datasetR()[, c(1, 7)]), type = "pearson")$r[1, 2]
        return(cat("The obtained correlation is", r.obt, '\n'))
    })

    # Probability randomized
    output$probability_randomized <- renderPrint({
        nreps <- 10000
        r.obt <- rcorr(as.matrix(datasetR()[, c(1, 7)]), type = "pearson")$r[1, 2]
        r.random <- replicate(nreps, { Y <- datasetR()$PctTip; X <- sample(datasetR()$Bill, length(datasetR()$Bill), replace = FALSE); rcorr(cbind(X,Y), type="pearson")$r[1,2] })
        prob <- length(r.random[r.random >= r.obt]) / nreps

        # Histogram
        output$histogram_prob_random <- renderPlot({
          hist(r.random, breaks = 50, main =  expression(paste("Distribution around p = 0")), xlab = "r from randomized samples", col = "blue", border = "red")
          # legend(.05, 500, paste("obtained correlation =",as.character(round(r.obt, digits = 2))), bty = "n")
          legend("topright", legend=paste("obtained correlation =",as.character(round(r.obt, digits = 2))),
                 col="green", lty=1:2, cex=0.7, box.lty = 0)
          abline(v=r.obt,col="green",lwd=2)
        })

        return(cat("Probability randomized r >= r.obt:", prob))
    })
    
    output$p_value_explanation <- renderPrint({
      if(input$remove_30_percentage){
        return(cat("The p-value is less than the significance level of 0.05 that means there is strong evidence against the null hypothesis. 
                   So we can conclude that generous customers were indeed outliers in our distribution!"))
      }
      else{
        return(cat("The p-value fluctuates around the significance level of 0.05 that means there is evidence against the null hypothesis but not clear."))
      }
    })

    
}