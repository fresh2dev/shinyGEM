wd <- commandArgs(trailingOnly=TRUE)[1]

shiny::runApp(wd, port=8080, launch.browser=FALSE)
