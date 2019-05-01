#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(rlang)
library(data.table)
library(readxl)
library(readr)
library(magrittr)
library(ggplot2)
library(scales)
library(ggthemes)
library(ggiraph)
library(grid)
library(gridExtra)
library(GGally)
library(lubridate)
library(anytime)

source('ui/date_formats.R', local = TRUE)

options(spinner.type = 6, spinner.color = '#3c8dbc')

theme_pattern <-
  '^theme\\_(?!get|update|void|set|linedraw|replace|wsj|map|solid|fivethirtyeight)'

ui <- dashboardPage(
  header = source('ui/header.R', local = TRUE)$value,
  sidebar = source('ui/sidebar.R', local = TRUE)$value,
  body = source('ui/body.R', local = TRUE)$value,
  skin = 'blue'
)


server <- source('server/server.R', local=TRUE)$value

shinyApp(ui, server)
