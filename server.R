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
    input$input_data
    input$file1
    input$plot_aggr
    ui_ready <<- FALSE
    if (!input$plot_aggr) {
      updated_data <<- data
    } else {
      tmp <- aggr_function(c('sum', 'avg', 'count'), c('sum', 'mean', 'count'))
      if (!invalid(tmp)) {
        updated_data <<- tmp
      }
    }
    cols <<- c(colnames(updated_data))
    factor_names <<- c()
    number_names <<- c()
    for (col in cols) {
      if (is.factor(updated_data[[col]]))
      {
        factor_names <<- c(factor_names, col)
      } else if (is.character(updated_data[[col]])) {
        factor_names <<- c(factor_names, col)
        updated_data[[col]] <<- ordered(updated_data[[col]])
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
          x_range <- range(updated_data[input$x], na.rm=TRUE)      
        } else {
          x_range <- range(updated_data[selected_x], na.rm=TRUE)
        }
      } else {
        x_range <- c(0, 0, 0)
      }
      if (invalid(x_range[1]) || invalid(x_range[2]) || is.na(x_range[1]) || is.na(x_range[2]) || is.infinite(x_range[1]) || is.infinite(x_range[2])) {
        x_range[1] <- 0
        x_range[2] <- 2
        step_range <- 1
      } else {
        step_range <- diff(x_range) / 100
      }
#       browser()
      updateSliderInput(session, 'x_range', min = x_range[1], max = x_range[2], step = step_range, value = c(x_range[1], x_range[2]))
      ui_ready <<- TRUE
      ready_to_plot <<- TRUE
    }
  })
  
  update_data <- reactive({
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
    if (!is.element(input$group_over[1], number_names))
      return()
    input$input_data
    d <- na.omit(aggr_data())
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
  
  bin_width <- reactive({
    x <- input_data()[input$x]
    binwidth <- diff(range(x)) / input$bins
  })
  
  facet_formula <- reactive({
    f <- as.formula(paste(input$facet, input$facet2, sep = ' ~ '))
  })
  
  plot_input <- reactive({
    input$file1
    x <- input$x
    y <- input$y
    d <- updated_data
    if (ui_ready&&ready_to_plot&&length(d)>0&&!invalid(d)) {
      res <- ggplot(d, aes_string(x=input$x), environment = environment())
      if (input$plot_type == "hp") {
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

  redraw <- eventReactive(input$redraw, {
    print(plot_input())
  })
  
  output$main_plot <- renderPlot({
    if(ui_ready) {
      if (input$auto_update) {
        print(plot_input())
      } else {
        redraw()
      }
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

output$save_plot_jpeg <- downloadHandler(
  filename = function() {
    if (!grepl(jpeg_ext, input$file_name)) {
      res <- paste(input$file_name, jpeg_ext, sep='')
    } else {
      res <- input$file_name
    }
    res
  },
  content = function(file) {
    jpeg(file)
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

output$save_aggr_data <- downloadHandler(
  filename = function() {
    if (!grepl(data_ext, input$file_name)) {
      res <- paste(input$file_name, '_aggr', data_ext, sep='')
    } else {
      res <- input$file_name
    }
    res
  },
  content = function(file) {
    write.csv(aggr_function(c('sum', 'avg', 'count'), c('sum', 'mean', 'count')) ,file)
  }
)  
})
