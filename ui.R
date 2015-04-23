
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(ggplot2)

#  FIX: update only on value of color or shape changed like in facet grid...

side_bar <- TRUE
plot_width <- 10
agr_control_width <- 3
full_width <- 12
plot_height <- 600
# selected_x <- number_names[1]
# selected_y <- number_names[2]
default_bins <- 30
selected_grid <- factor_names[1]
selected_color <- factor_names[1]
selected_shape <- factor_names[1]
control_style <- 'padding:10px;margin-bottom:10px'
skin <- 'blue'


dashboard_sidebar <- dashboardSidebar(
  disable = !side_bar,
  tags$h2(icon("cog"), 'Menu'),
  selectInput('input_data', 'Select input data from list or upload some below', c('diamonds','flights')),
  fileInput('file1', 'Choose CSV File',
            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
  sidebarMenu(
    menuItem("Plot", icon = icon("line-chart"), tabName="main"),
    menuItem("Aggregation", icon = icon("table"), tabName="aggregation"),
    menuItem("Data Table", icon = icon("table"), tabName="data_table")
  ),
  fluidRow(
    box(
      class = 'text-center',
      radioButtons(
        "x",
        "X value",
        cols,
        selected = selected_x,
        inline = TRUE
      ),
      width = 12
    )
  ),
  fluidRow(
    box(
      class = 'text-center',
      sliderInput('x_range', 'X range', min = 0, max = 100, value = c(0, 100)),
      width = 12
    )
  ),
  fluidRow(
    box(
      class = 'text-center',
      textInput('file_name', 'Download file name', value='plot'),
      downloadButton('save_plot', 'png'),
      downloadButton('save_data', 'csv'),
      width = 12
    )
  )
)

main_controls_box <- box(
  title = "Controls",
  width = full_width - plot_width,
  selectInput(
    "plot_type",
    "Plot Type",
    list("sp", "hp", "tp")
  ),
  wellPanel(
    style = control_style,
    radioButtons(
      "y",
      "Y values",
      cols,
      selected = selected_y,
      inline = FALSE
    )
  ),
  conditionalPanel(
    condition = 'input.plot_type != "hp"',
    checkboxInput('jitter', 'Use jitter')
  ),
  conditionalPanel(
    condition = 'input.plot_type == "hp"',
    numericInput('bins', 'Histogram bins', default_bins, min = 1)
  ),
  wellPanel(
    style = control_style,
    checkboxInput('facet_grid', 'Facet grid'),
    selectInput(
      "facet",
      "Facet grid I",
      c(factor_names, '.'),
      selected = selected_grid
      #         inline = TRUE
    ),
    selectInput(
      "facet2",
      "Facet grid II",
      c(factor_names, '.'),
      selected = '.'
      #         inline = TRUE
    )
  ),
  wellPanel(
    style = control_style,
    checkboxInput('coloring', 'Color'),
    selectInput(
      "color",
      "Colors for",
      factor_names,
      selected = selected_color
      #         inline = TRUE
    )
  ),
  conditionalPanel(
    condition = 'input.plot_type == "sp"',
    wellPanel(
      style = control_style,
      checkboxInput('shaping', 'Shaping'),
      selectInput(
        "shape",
        "Shapes for",
        factor_names,
        selected = selected_shape
        #         inline = TRUE
      )
    )
  )
)

main_tab_item <- tabItem(
  tabName = 'main',
  fluidRow(
    box(plotOutput("main_plot", height = plot_height), width = plot_width),
    main_controls_box
  )
)

data_table_tab_item <- tabItem(
  tabName = 'data_table',
  h2(data_title),
  fluidRow(
    box(dataTableOutput('table_view'), width=12)
  )
)

aggr_controls_box <-     box(
  title = 'Controls',
  width = agr_control_width,
  wellPanel(style = 'padding:10px',
            checkboxInput('group_all', 'Group over all', value = FALSE),
            conditionalPanel(condition = '!input.group_all', uiOutput('aggr_group'))
  ),
  wellPanel(style = 'padding:10px',
            checkboxInput('over_all', 'Calculate over all', value = FALSE),
            conditionalPanel(condition = '!input.over_all', uiOutput('aggr_over'))
  )
)

aggregation_tab_item <- tabItem(
  tabName = 'aggregation',
  h2('Aggregation'),
  fluidRow( aggr_controls_box,
            box(
              #                 title = 'Aggeration',
              width = full_width - agr_control_width,
              wellPanel(dataTableOutput('sum_table'))
            )
            #     tabBox(
            #       title = 'Values',
            #       tabPanel('Sum Panel', dataTableOutput('sum_table')),
            #       tabPanel('Count Panel', dataTableOutput('length_table')),
            #       tabPanel('Mean Panel', dataTableOutput('mean_table')),
            #       width = full_width - agr_control_width
            #       )
  )
)

dashboard_body <- dashboardBody(
  tabItems(
    main_tab_item,
    data_table_tab_item,
    aggregation_tab_item
  )
)


ui <- dashboardPage(
  skin = skin,
  dashboardHeader(title = "ggplot"),
  dashboard_sidebar,
  dashboard_body
)
