
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(WDI)

data <- WDI(extra = TRUE, country = c("US","CA","MX"))
cols <- c(colnames(data))

ui <- dashboardPage(
  dashboardHeader(title = "ggplot"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
    box(plotOutput("main_plot", height = 400)),
    box(
      title = "Controls",
      selectInput(
        "plot_type",
        "Plot Type",
        list("sp", "hp", "p")
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
      numericInput('bins', 'Histogram bins', 30, min = 1)
    )
    ))
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
