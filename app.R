### Source Code for SEN'sabale Plotting App ###
### MIT License - see LICENSE file for details
### Copyright (c) 2026 Sumit Sen

### app.R file for source code for running SEN'sable Plotting on shiny server ###
options(shiny.autoload.r = FALSE)

suppressWarnings({
  pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
})
Sensabled::run_app()