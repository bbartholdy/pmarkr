#' Function to launch the pmarkr shinyapp
#'
#' This function launches the shiny graphical user interface (GUI) for the pmarkr package.
#' @details The GUI is provided as a user-friendly alternative for less experienced R-users. It can be used to obtain the PMarks from the MB11 sample or an uploaded dataset.
#' @example inst/runPMarkex.R
#' @export runPMark
runPMark <- function(){
  shiny::runApp(system.file("pmarkrApp", package = "pmarkr"))
}
