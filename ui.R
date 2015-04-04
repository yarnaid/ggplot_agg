
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

require(ggplot2)
require(shinydashboard)

library(shiny)
library(shinydashboard)
library(ggplot2)

# library(WDI)

# data <- WDI(extra = TRUE, country = c("US","CA","MX"))
data <- diamonds
cols <- c(colnames(data))
plot_width <- 9
full_width <- 12
plot_height <- 600
side_bar <- TRUE

dashboard_sidebar <- dashboardSidebar(
  disable = !side_bar,
  sidebarMenu(
    menuItem("Main", tabName="main"),
    menuItem("Data Table", tabName="data_table")
  )
)

main_controls_box <- box(
  title = "Controls",
  width = full_width - plot_width,
  selectInput(
    "plot_type",
    "Plot Type",
    list("sp", "hp")
  ),
  selectInput(
    "column_x",
    "X values",
    cols,
    #         inline = TRUE
  ),
  selectInput(
    "column_y",
    "Y values",
    cols,
    #         inline = TRUE
  ),
  numericInput('bins', 'Histogram bins', 30, min = 1),
  checkboxInput('facet_grid', 'Facet grid'),
  selectInput(
    "column_facet",
    "Facet grid",
    cols,
    #         inline = TRUE
  ),
  checkboxInput('coloring', 'Color'),
  selectInput(
    "column_color",
    "Colors for",
    cols,
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
  h2('Data'),
  fluidRow(
    box(dataTableOutput('table_view'), width=12)
  )
)

dashboard_body <- dashboardBody(
  tabItems(
    main_tab_item,
    data_table_tab_item
  )
)


ui <- dashboardPage(
  dashboardHeader(title = "ggplot"),
  dashboard_sidebar,
  dashboard_body
)

# shinyUI(fluidPage(
# 
#   # Application title
#   titlePanel("Old Faithful Geyser Data"),
# 
#   # Sidebar with a slider input for number of bins
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("bins",
#                   "Number of bins:",
#                   min = 1,
#                   max = 50,
#                   value = 30)
#     ),
# 
#     # Show a plot of the generated distribution
#     mainPanel(
#       plotOutput("distPlot")
#     )
#   )
# ))
