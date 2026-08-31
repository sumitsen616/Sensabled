#' Run the Sensabled Shiny Application
#' @param ... Arguments passed to shinyAppDir
#' @return A Shiny app object
#' @export
run_app <- function(...) {
  app_dir <- system.file("app", package = "Sensabled")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `Sensabled`.", call. = FALSE)
  }
  # Set global upload limit before app launch
  options(shiny.maxRequestSize = 250 * 1024^2)
  
  # Serve static files from inst/app/www
  www_path <- system.file("app/www", package = "Sensabled")
  if (www_path == "") {
    www_path <- "inst/app/www" 
  }
  shiny::addResourcePath(
    prefix = "www",
    directoryPath = www_path
  )
  data_path <- system.file("app/data", package = "Sensabled")
  if (data_path == "" || !dir.exists(data_path)) {
    data_path <- file.path(getwd(), "inst/app/data")
  }
  shiny::addResourcePath(
    prefix = "data",
    directoryPath = normalizePath(data_path)
    )
  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    ...
  )
}

