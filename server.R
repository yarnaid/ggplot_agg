
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)

# data <- WDI(extra = TRUE, country = c("US","CA","MX"))
data <- diamonds

plotType <- function(shape=1, type, binwidth=5) {
  switch(type,
         sp = geom_point(shape=shape),
         hp = geom_histogram(shape=shape, binwidth=binwidth),
         p = pie())
}

shinyServer(function(input, output) {
  output$main_plot <- renderPlot({
    if (input$plot_type == "hp") {
      x <- data[input$column_x]
      binwidth <- diff(range(x)) / input$bins
      res <- ggplot(data, aes_string(x=input$column_x)) + plotType(binwidth=binwidth, type=input$plot_type)
    } else {
      res <- ggplot(data, aes_string(x=input$column_x, y=input$column_y)) + plotType(type=input$plot_type)
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
