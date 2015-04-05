
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(ggplot2)

#  FIX: update only on value of color or shape changed like in facet grid...

# library(WDI)

# data <- WDI(extra = TRUE, country = c("US","CA","MX"))
side_bar <- TRUE
plot_width <- 10
full_width <- 12
plot_height <- 600
selected_x <- 'x'
selected_y <- 'y'
default_bins <- 30
selected_grid <- 'cut'
selected_color <- 'color'
selected_shape <- 'cut'


dashboard_sidebar <- dashboardSidebar(
  disable = !side_bar,
  tags$h3('Menu'),
  fileInput('file1', 'Choose CSV File',
            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
  tags$hr(),
  sidebarMenu(
    menuItem("Main", tabName="main"),
    menuItem("Data Table", tabName="data_table"),
    menuItem("Aggregation", tabName="aggregation")
    ),
  tags$hr(),
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
      textInput('file_name', 'Download file name', value='plot'),
      downloadButton('save_plot', 'png'),
      downloadButton('save_data', 'csv'),
      width = 12
      )
    ),
  fluidRow(
    box(
      class = 'text-center',
      sliderInput('x_range', 'X range', min = 0, max = 100, value = c(0, 100)),
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
  radioButtons(
    "y",
    "Y values",
    cols,
    selected = selected_y,
            inline = TRUE
    ),
  checkboxInput('jitter', 'Use jitter'),
  numericInput('bins', 'Histogram bins', default_bins, min = 1),
  checkboxInput('facet_grid', 'Facet grid'),
  selectInput(
    "facet",
    "Facet grid",
    c(cols, '.'),
    selected = selected_grid
    #         inline = TRUE
    ),
  selectInput(
    "facet2",
    "Facet grid(second variable)",
    c(cols, '.'),
    selected = '.'
    #         inline = TRUE
    ),
  checkboxInput('coloring', 'Color'),
  selectInput(
    "color",
    "Colors for",
    cols,
    selected = selected_color
    #         inline = TRUE
    ),
  checkboxInput('shaping', 'Shaping'),
  selectInput(
    "shape",
    "Shapes for",
    cols,
    selected = selected_shape
    #         inline = TRUE
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
  h2('Diamonds Data'),
  fluidRow(
    box(dataTableOutput('table_view'), width=12)
    )
  )

aggregation_tab_item <- tabItem(
  tabName = 'aggregation',
  h2('Aggregation'),
  fluidRow(
    box(
      title = 'Controls'
      ),
    box(
      title = 'Values'
      )
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
  dashboardHeader(title = "ggplot"),
  dashboard_sidebar,
  dashboard_body
  )
