
library(shiny)
library(devtools)

port <- Sys.getenv('PORT')

# Install GRousselet/rogme package
devtools::install_github("GRousselet/rogme")

shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)