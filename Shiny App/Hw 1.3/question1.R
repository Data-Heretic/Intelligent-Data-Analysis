###################################################
##########         Question 1        ##############
###################################################

# UI

hw1.3_q1_ui <- function(id, options) {
    ns <- NS(id)
    tabPanel(title = "Question 1",
        column(10,
            h2(hw1.3_title),
            h4("Perform a Principal Component Analysis on the wine data set (11 quantitative variables, don't include quality, but you might want to include type as a supplementary categorical variable)."),
            h4("We will use the R matrix (correlation) because variables are on different scales
            You tend to use the covariance matrix (S) when the variable scales are similar and 
            the correlation matrix (R) when variables are on different scales
            scale.unit=TRUE bases the PCA on the correlation matrix."),
            fluidPage(
                fluidRow(
                    plotOutput(ns("plot.pairs")),
                    br(),
                    br()
                ),
                fluidRow(
                  column(6,
                  box(status = "primary",
                    h5("First 4 components"),
                    verbatimTextOutput(ns("f4comp")),
                    h5("First 4 cumulative components"),
                    verbatimTextOutput(ns("fc4comp")),
                    align = "center",width = 12)),
                  column(6,
                         box(status = "primary",
                          h4("We can see that the first 4 components reflect 71.7% of the variation of data;
we can see that components 1 and 2 are the components that 
                             mostly reflect the variation of data with a value of 48%,
                             so we can use those values to represent the data."),
                          align="center",width=12
                         ))
                ),
                fluidRow(
                  column(6,
                  box(status = "primary",
                    h5("Contributions"),
                    verbatimTextOutput(ns("fc4contrib")),
                    align = "center", width = 12)),
                  column(6,
                         box(status = "primary",
                          h4("We can see which variables contribute the most to the first 4 components"),
                          align = "center", width = 12
                         ))
                ),
                fluidRow(
                  column(6,
                         box(status = "warning",
                             h5("Most important components"),
                             verbatimTextOutput(ns("m2_comp")),
                             align = "center",width = 12)
                         ),
                  column(6,
                         box(status = "warning",
                           h4("Interpretation of the Principal components is based on finding which variables are strongly correlated with the components,
so values with high values either in positive or negative directions will be considered highly correlated,
                              ( > | 0.5 | ).The first component is highly correlated with 'total sulfur dioxide', 'free sulfur dioxide',
            'citic acid' and 'residual sugar' in a positive direction. This suggests that these 4 criteria vary together.
                              If one increases, then the remaining ones tend to as well.The second component can be viewed as a measure of fixed acidity, chlorides and sulphates , while having a low ph and alcohol ,
                              so this component can be viewed as a term of how un-alcoholic this wine could be."),
                           align="center",width=12)
                         )
                  ),
                fluidRow(
                  column(6,box(status="warning",plotOutput(ns("variables")),align = "center",width = 12)),
                  column(6,box(status="warning",plotOutput(ns("top.var")),align = "center",width = 12))
                ),
                fluidRow(
                  column(6,box(status="success",plotOutput(ns("pca_type")),align = "center",width = 12)),
                  column(6,box(status="success",h4("In the dataset we have 2 wine types and one of the main functions of the PCA is to do classification,
                                  as we can see in the below plot we can cluster the wine variables based on their types.
                                  We can asssume that Dim1 separates wines according to type more that Dim2."),align = "center",width = 12))
                ),
                fluidRow(
                  #column(6,box(status="danger",plotOutput(ns("without.qual")),align="center",width=12)),
                  column(6,box(status="danger",plotOutput(ns("with.qual")),align="center",width=12))
                ,
                  #column(6,box(status="danger")),
                column(6, box(status = "danger", h4("We can appreciate how the second principal component seems to divide the zones with high quality versus the zone with lower quality, being the mayority in the bottom right quadrant.
                                                Also the top left quadrant seems to albergate mostly low quality, although low quality elements are more or less equally ditributed among all the quadrants.
                                                Finally the medium level group are present equally distributed in all quadrants except the top left one.
                                                We can say that top - medium quality tend to be in the bottom right quadrant "),align=" center ",width=12))
                ),
                fluidRow(
                  column(6,box(status="info",plotOutput(ns("circle_corr")),align="center",width=12)),
                  column(6,box(status="info",h4("In the  circle correlation plot we can see the reflection of what we described previously
                                              in the interpretation of the first 2 PCA components , the closer to the PC1 components the wines 
                                                are the more the more sulfur dioxide and cidic acid they will have, aslo the lower they close to DIM2 ,
                                                the less alcoholic those wines would be."),align="center",width=12))
                )
            )
        ),
        column(2, box(width = 12, class = 'well box-options', options))
    )
}

# Server

hw1.3_q1_server <- function(input, output, session, wines) {

    output$plot.pairs <- renderPlot({
      ggpairs(wns, lower = list(continuous = "points", combo = "facetdensity", mapping = aes(color = type)))
    })

    output$f4comp <- renderPrint({
      #First 4 components 
        w_pca$eig[1:4,2]
    })
    output$fc4comp <- renderPrint({
      #This can answer the first two questions 
      w_pca$eig[1:4,3]
    })
    output$fc4contrib <- renderPrint({
    #This can answer the first two questions 
      w_pca$var$contrib
    })
    output$m2_comp <- renderPrint({
      #This can answer the first two questions 
      w_pca$var$coord[,1:2]
    })
    output$variables <- renderPlot({
      fviz_pca_var(w_pca, col.var="contrib")+
        scale_color_gradient2(low="green",mid = "green" ,high="red")+theme_light()
      
    })
    output$top.var <- renderPlot({
      plot(w_pca, shadow=TRUE,choix="var", select="contrib 3")
    })
    output$pca_type <- renderPlot({
      fviz_pca_ind(w_pca,  label="none", habillage=12)
    })
    
    output$with.qual <- renderPlot({
      fviz_pca_ind(wines_pca,  label="none", habillage=quality,cex=0.8,addEllipses=TRUE)
    })
    #output$without.qual <- renderPlot({
    #  fviz_pca_ind(w_pca,  label="none", habillage=quality,cex=0.8,addEllipses=TRUE)
    #})
    output$circle_corr <- renderPlot({
      fviz_pca_var(w_pca)
    })
}