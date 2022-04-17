#' Wrapper to run greylitsearcher Shiny app locally
#'
#' @description Run the greylitsearcher Shiny app locally with this
#' function. Calls the latest version of the app from GitHub without
#' the need to install greylitsearcher
#' @importFrom shiny runGitHub
#' @return Runs the GitHub version of the Shiny App locally.
#' @export
#' @examples
#' \dontrun{
#' rungreylitsearcher()
#' }
rungreylitsearcher <- function(){
  shiny::runGitHub("greylitsearcher",
                   "nealhaddaway",
                   subdir = "inst/shiny-examples/greylitsearcher")
}
