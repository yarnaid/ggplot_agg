
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
require(ggplot2)
require(ggvis)
require(shinythemes)
require(sqldf)

library(shiny)
library(ggplot2)
library(ggvis)
library(sqldf)

pageLength <- 10


shinyServer(function(input, output, session) {
  update_ui <- reactive({
    #     browser()
    input$input_data
    input$file1
    ui_ready <<- FALSE
    cols <<- c(colnames(data))
    factor_names <<- c()
    number_names <<- c()
    for (col in cols) {
      if (is.factor(data[[col]]))
      {
        factor_names <<- c(factor_names, col)
      } else if (is.character(data[[col]])) {
        factor_names <<- c(factor_names, col)
        data[[col]] <<- ordered(data[[col]])
      } else {
        number_names <<- c(number_names, col)
      }
    }
    selected_x <<- number_names[1]
    selected_y <<- number_names[2]
    updateRadioButtons(session, 'x', choices = number_names, selected = number_names[1], inline = TRUE)
    updateRadioButtons(session, 'y', choices = number_names, selected = number_names[2], inline = TRUE)
    updateSelectInput(session, 'facet', choices = c(factor_names, '.'))
    updateSelectInput(session, 'facet2', choices = c(factor_names, '.'))
    updateSelectInput(session, 'color', choices = factor_names)
    updateSelectInput(session, 'shape', choices = factor_names)
    ui_ready <<- TRUE
  })
  
  get_data <- reactive({
    #     browser()
    input$file1
    ui_ready <<- FALSE
    if (input$input_data == 'diamonds')
    {
      res <- head(diamonds, 10000)
#       res <- diamonds
      data_title <<- 'Diamonds'
    } else if (input$input_data == 'flights') {
      data_title <<- 'Flights'
      res <- head(flights, 10000)
#       res <- flights
    } else {
      data_title <<- 'User Data'
      res <- data.frame(c())
    }
    data <<- res
    res
  })
  
  observe({
    #     browser()
    input$input_data
    input$file1
    ready_to_plot <<- FALSE
    update_data()
    update_ui()
    if (ui_ready) {
      ui_ready <<- FALSE
      if (!invalid(input$x)) {
        if (input$x %in% number_names) {
          x <- input$x
          x_range <- range(data[input$x], na.rm=TRUE)      
        } else {
          x_range <- range(data[selected_x], na.rm=TRUE)
        }
      } else {
        x_range <- c(0, 0, 0)
      }
      if (is.na(x_range[1]) || is.na(x_range[2])) {
        x_range[1] <- 0
        x_range[2] <- 0
        step_range <- 1
      } else {
        step_range <- diff(x_range) / 100
      }
      updateSliderInput(session, 'x_range', min = x_range[1], max = x_range[2], step = step_range, value = c(x_range[1], x_range[2]))
      ui_ready <<- TRUE
      ready_to_plot <<- TRUE
    }
  })
  
  update_data <- reactive({
    #     browser()
    ui_ready <<- FALSE
    in_file <<- input$file1
    if (is.null(in_file))
      return(get_data())
    data <<- data.frame(read.csv(in_file$datapath))
    
    data
  })
  
  grouping_choised <- function(id, label, selected, choises)(
    checkboxGroupInput(id, label = h3(label),
                       choices = choises,
                       selected = selected
                       # inline = TRUE
    )
  )
  
  output$aggr_group <- renderUI({
    input$input_data
    input$file1
    grouping_choised('group_by', 'Group by', factor_names[1], factor_names)
  })
  
  output$aggr_over <- renderUI({
    input$input_data
    input$file1
    grouping_choised('group_over', 'Calculate over', number_names[1], number_names)
  })
  
  
  input_data <- reactive({
    #     browser()
    input$file1
    lims <- input$x_range
    res <- data.frame(c())
    isolate({
      x <- input$x
      if (!invalid(x)) {
        x_min <- data[x] >= lims[1]
        x_max <- data[x] <= lims[2]
        slice <- x_min&x_max
        res <- data[slice,]
      }
    })
    return(res)
  })
  
  aggr_data <- reactive({
    input$file1
    input_data()
  })
  
  output$table_view <- renderDataTable({
    input_data()
  })
  
  aggr_function <- function(FUNs, nms) {
#     browser()
#     update_data()
#     update_ui()
    if (!is.element(input$group_over[1], number_names))
      return()
    input$input_data
    d <- aggr_data()
    over_fs <- ''
    for (i in 1:length(FUNs)) {
      FUN <- FUNs[i]
      name <- nms[i]
      if (!input$over_all) {
        over <- paste(sapply(input$group_over, function(x) {paste(FUN, '(', x, ') ', name, '_', x, sep = '')}), collapse = ', ')
      } else {
        over <- paste(FUN, '(*) ', name)
        over <- paste(sapply(number_names, function(x) {paste(FUN, '("', x, '") ', name, '_', x, sep = '')}), collapse = ', ')
      }
      over_fs <- paste(over_fs, over, sep = ', ')
    }
    if (!input$group_all) {
      by <- paste(input$group_by, collapse=', ')
    } else {
      by <- paste(factor_names, collapse=', ')
    }
    query <- paste('select', by, over_fs, 'from d group by', by)
    sqldf(query)
  }
  
  output$sum_table <- renderDataTable(
    aggr_function(c('sum', 'avg', 'count'), c('sum', 'mean', 'count')),
    options = list (
      pageLength = pageLength
    )
  )
  
#   output$length_table <- renderDataTable(
#     aggr_function(length),
#     options = list (
#       pageLength = pageLength
#     )
#   )
#   
#   output$mean_table <- renderDataTable(
#     aggr_function(mean),
#     options = list (
#       pageLength = pageLength
#     )
#   )
  
  bin_width <- reactive({
    x <- input_data()[input$x]
    binwidth <- diff(range(x)) / input$bins
  })
  
  facet_formula <- reactive({
    f <- as.formula(paste(input$facet, input$facet2, sep = ' ~ '))
  })
  
  plot_input <- reactive({
    #     browser()
    d <- input_data()
    input$file1
    x <- input$x
    y <- input$y
    if (ui_ready&&ready_to_plot&&length(d)>0&&!invalid(d)) {
      res <- ggplot(input_data(), aes_string(x=input$x), environment = environment())
      if (input$plot_type == "hp") {
        #         browser()
        res <- res + geom_histogram(binwidth=bin_width())
      } else if(input$plot_type == "sp") {
        res <- res + aes_string(y=input$y) + geom_point(na.rm=TRUE)
        if (input$jitter) {
          res <- res + geom_jitter(alpha = alpha)
        }
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
            
      res
    }
  })
  
  output$main_plot <- renderPlot({
    if(ui_ready) {
      print(plot_input())
    }
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
