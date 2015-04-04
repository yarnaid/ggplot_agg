
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
require(ggplot2)


library(shiny)
library(ggplot2)

# data <- WDI(extra = TRUE, country = c("US","CA","MX"))
data <- diamonds
points_fill <- I("darkorchid1")
alpha = 0.1

plotType <- function(shape=1, type, binwidth=5, pch=21, alpha=0.1) {
  switch(type,
         sp = geom_point(shape=shape, pch = pch),
         hp = geom_histogram(binwidth=binwidth),
#          p = pie()
         )
}

shinyServer(function(input, output) {
  input_data <- reactive({
    data[, c(input$column_x, input$column_y, input$column_facet, input$column_color, 'color')]
    })
  
  output$table_view <- renderDataTable({
    data
  })
  
  
  output$main_plot <- renderPlot({
    res <- ggplot(data, aes_string(x=input$column_x), environment = environment())
    if (input$plot_type == "hp") {
      x <- input_data()[input$column_x]
      binwidth <- diff(range(x)) / input$bins
      res <- res + geom_histogram(binwidth=binwidth)
    } else {
      res <- res + plotType(type=input$plot_type) + aes_string(y=input$column_y)
    }
    
    if (input$facet_grid == TRUE) {
      f <- as.formula(paste(input$column_facet, '~ .'))
      res <- res + facet_grid(f)
    }
    
    if (input$coloring == TRUE) {
      res <- res + aes(color=factor(get(input$column_color)))
    }
    
    if (FALSE) {
      res <- res + geom_jitter(alpha = alpha)
    }
    
    print(res)
    })
  })

# shinyServer(function(input, output) {
# 
#   output$distPlot <- renderPlot({
# 
#     # generate bins based on input$bins from ui.R
#     x    <- faithful[, 2]
#     bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#     # draw the histogram with the specified number of bins
#     hist(x, breaks = bins, col = 'darkgray', border = 'white')
# 
#   })
# 
# })
