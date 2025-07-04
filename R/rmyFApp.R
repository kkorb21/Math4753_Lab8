#' Launch Shiny app for simulating B
#'
#' This function launches a Shiny app that visualizes and summarizes simulations
#' of B = X + Y using the inverse CDF optimization method.
#'
#' @return Launches interactive Shiny app
#' @export
#' @examples
#' rmyFApp()
rmyFApp <- function() {
  appDir <- system.file("shiny", "rmyFApp", package = "Lab8KORBMath4753")
  if (appDir == "") {
    stop("Could not find Shiny app directory. Try reinstalling the package.")
  }
  shiny::runApp(appDir, display.mode = "normal")
}
