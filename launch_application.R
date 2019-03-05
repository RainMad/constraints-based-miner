#' @export
runExample <- function() {
  appDir <- system.file("constreaintsBasedApp", package = "constraintsbasedminer")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `constraintsbasedminer`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}