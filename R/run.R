#' Run Application
#'
#' @import dplyr
#' @import shiny
#' @import tidyr
#' @import stringr
#' @import readr
#' @import magrittr
#' @import arrow
#' @import sf
#' @import leaflet
#' @import scales
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import shinyWidgets
#' @import htmltools
#' @import DT
#' @import sever
#' @import waiter
#' @import dashboardthemes
#' @import shinyjs
#' @import cicerone
#' 
#' @export
run_app <- function() {

  # Boundaries
  lad_shp <- read_sf(pkg_data("lad.shp"))
  msoa_shp <- read_sf(pkg_data("msoa.shp"))

  ri <- read_feather(pkg_data("resilience-index.feather"))
  vi <- read_feather(pkg_data("vulnerability-index-msoa-england.feather"))

  # ---- Data prep ----
  ri_shp <- lad_shp %>%
    left_join(ri, by = c("lad19cd" = "LAD19CD"))

  vi_shp <- msoa_shp %>%
    left_join(vi, by = c("MSOA11CD" = "Code"))

  .globals$lad_shp <- lad_shp
  .globals$ri <- ri
  .globals$vi <- vi
  .globals$ri_shp <- ri_shp
  .globals$vi_shp <- vi_shp

  # server static files
  shiny::addResourcePath(
    "assets",
    system.file("www", package = "resilience.index")
  )

  enableBookmarking(store = "url") # saving as url results in a very long url but necessary in this deployment

  # Run app ----
  shinyApp(ui, server)
}
