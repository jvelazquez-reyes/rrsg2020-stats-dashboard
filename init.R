# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("reticulate", "Metrics","gridExtra","ggplot2","HDInterval","remotes","tibble",
                "stringi","DescTools","rjson","viridis","irr","sjPlot","plotly","shiny",
                "shinyjs","shinythemes","shinydashboard")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))

## Install rogme from Github
if (!"rogme" %in% rownames(installed.packages())) {
  remotes::install_github("GRousselet/rogme")
}