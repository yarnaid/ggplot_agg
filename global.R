require(ggplot2)
require(shinydashboard)
# require(doBy)
require(shinythemes)
require(nycflights13)
require(gtools)
require(ggvis)
require(sqldf)

library(ggplot2)
# library(doBy)
library(shinythemes)
library(nycflights13)
library(gtools)

options(shiny.maxRequestSize = 10000*1024^2)

data <- data.frame(c(1))
data_title <- ''
cols <- c(1)

points_fill <- I("darkorchid1")
alpha = 0.1
ui_ready <- FALSE
ready_to_plot <- FALSE

image_ext <- '.png'
data_ext <- '.csv'
jpeg_ext <- '.jpeg'

factor_names <- c()
number_names <- c()
selected_x <- 'x' # number_names[1]
selected_y <- 'y' #number_names[2]