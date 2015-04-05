
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
require(ggplot2)
require(ggvis)

library(shiny)
library(ggplot2)
library(ggvis)

no_aggr <- c('color', 'cut', 'clarity')
pageLength <- 10


plotType <- function(shape=1, type, binwidth=5, pch=21, alpha=0.1) {
  switch(type,
   sp = geom_point(shape=shape, pch = pch),
   hp = geom_histogram(binwidth=binwidth),
#          p = pie()
   )
}

shinyServer(function(input, output, session) {

  observe({
    data <- update_data()
    x_range <- range(data[input$x])
    step_range <- diff(x_range) / 100
    updateSliderInput(session, 'x_range', min = x_range[1], max = x_range[2], step = step_range)
  })

  update_data <- reactive({
    in_file <- input$file1
    if (is.null(in_file))
      return(data)
    data <- data.frame(read.csv(in_file$datapath))

    cols <- c(colnames(data))
    updateRadioButtons(session, 'x', choices = cols, selected = input$x, inline = TRUE)

    data
  })

  grouping_choised <- function(id, label, selected)(
  checkboxGroupInput(id, label = h3(label),
    choices = cols,
    selected = selected
    # inline = TRUE
    )
  )

  output$aggr_group <- renderUI({
    grouping_choised('group_by', 'Group by', 'color')
  })

  output$aggr_over <- renderUI({
    grouping_choised('group_over', 'Calculate over', 'x')
  })


  input_data <- reactive({
    x_min <- data[input$x] >= input$x_range[1]
    x_max <- data[input$x] <= input$x_range[2]
    slice <- x_min&x_max
    res <- data[slice, c(input$x, input$y, input$facet, input$color, input$shape)]
  })

  aggr_data <- reactive({
    x_min <- data[input$x] >= input$x_range[1]
    x_max <- data[input$x] <= input$x_range[2]
    slice <- x_min&x_max
    res <- data[slice,]
  })

  output$table_view <- renderDataTable({
    input_data()
  })

  aggr_function <- function(FUN) {
    if (!input$over_all) {
    over <- paste(input$group_over, collapse = ' + ')
    } else {
      over <- '.'
    }
    if (!input$group_all) {
    by <- paste(input$group_by, collapse = ' + ')
    } else {
      by <- '.'
    }
    f <- as.formula(paste(over, by, sep=' ~ '))
    summaryBy(f, data=aggr_data(), FUN=FUN)
  }

  output$sum_table <- renderDataTable(
    aggr_function(sum),
    options = list (
      pageLength = pageLength
      )
    )

  output$length_table <- renderDataTable(
    aggr_function(length),
    options = list (
      pageLength = pageLength
      )
    )

  output$mean_table <- renderDataTable(
    aggr_function(mean),
    options = list (
      pageLength = pageLength
      )
    )



  bin_width <- reactive({
    x <- input_data()[input$x]
    binwidth <- diff(range(x)) / input$bins
  })

  facet_formula <- reactive({
    f <- as.formula(paste(input$facet, input$facet2, sep = ' ~ '))
  })

  plot_input <- reactive({
    res <- ggplot(input_data(), aes_string(x=input$x), environment = environment())
    if (input$plot_type == "hp") {
      res <- res + geom_histogram(binwidth=bin_width())
    } else if(input$plot_type == "sp") {
      res <- res + aes_string(y=input$y) + geom_point()
    } else if(input$plot_type == "tp") {
      res <- res + aes_string(y=input$y) + geom_line()
    }


    if (input$facet_grid == TRUE) {
      res <- res + facet_grid(facet_formula())
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
