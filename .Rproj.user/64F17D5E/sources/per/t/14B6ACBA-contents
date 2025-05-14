# Needed libraries
packages <- c(
  "shiny", "shinydashboard", "shinyWidgets", "shinyBS", "shinycssloaders",
  "DBI", "glue", "DatabaseConnector",
  "dplyr","purrr", "ggplot2", "plotly", "DT"
)

#' Function that installs all libraries
#'
#' @param pkg
#'
#' @return
#' @export
#'
#' @examples
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
})

source("ui/sidebar.R")
source("ui/body.R")
source("server/server_main.R")


# UI with sidebar and body
ui <- dashboardPage(
  dashboardHeader(title = "Cohort Explorer"),
  sidebar,
  body
)

# Starting the app
shinyApp(ui = ui, server = server)
