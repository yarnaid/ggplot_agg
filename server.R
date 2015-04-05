
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

image_ext <- '.png'
data_ext <- '.csv'

plotType <- function(shape=1, type, binwidth=5, pch=21, alpha=0.1) {
  switch(type,
         sp = geom_point(shape=shape, pch = pch),
         hp = geom_histogram(binwidth=binwidth),
#          p = pie()
         )
}

shinyServer(function(input, output) {
  input_data <- reactive({
    data[, c(input$x, input$y, input$facet, input$color)]
    })
  
  output$table_view <- renderDataTable({
    data
  })

  bin_width <- reactive({
    x <- input_data()[input$x]
    binwidth <- diff(range(x)) / input$bins
  })
  
  plot_input <- reactive({
    res <- ggplot(data, aes_string(x=input$x), environment = environment())
    if (input$plot_type == "hp") {
      res <- res + geom_histogram(binwidth=bin_width())
    } else if(input$plot_type == "sp") {
      res <- res + aes_string(y=input$y) + geom_point()
    } else if(input$plot_type == "tp") {
      res <- res + aes_string(y=input$y) + geom_line() # + xlab("") + ylab("Daily Views")
    }


    
    if (input$facet_grid == TRUE) {

      f <- as.formula(paste(input$facet, input$facet2, sep = ' ~ '))
      res <- res + facet_grid(f)
    }
    
    if (input$coloring == TRUE) {
      res <- res + aes(color=factor(get(input$color)))
    }

    if (input$shaping == TRUE) {
      res <- res + aes(shape=factor(get(input$shape)))
    }
    
    
    if (input$jitter) {
      res <- res + geom_jitter(alpha = alpha)
    }
    res
  })
  
  output$main_plot <- renderPlot({
    print(plot_input())
  })
  
  output$save_plot <- downloadHandler(
    filename = function() {
      if (!grepl(image_ext, input$file_name)) {
        res <- paste(input$file_name, image_ext, sep='')
      } else {
        res <- input$file_name
      }
      res
    },
    content = function(file) {
      png(file)
      print(plot_input())
      dev.off()
  })

  output$save_data <- downloadHandler(
    filename = function() {
      if (!grepl(data_ext, input$file_name)) {
        res <- paste(input$file_name, data_ext, sep='')
      } else {
        res <- input$file_name
      }
      res
    },
  content = function(file) {
    write.csv(data ,file)
  }
  )
})
