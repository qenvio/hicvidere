#' Launch a shiny app to visualize HiC data.
#'
#' @export

runSimple <- function() {
  appDir <- system.file("shiny-examples", "simpleVis", package = "hicvidere")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `hicvidere`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
