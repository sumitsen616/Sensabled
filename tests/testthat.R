library(shinytest2)

test_that("{shinytest2} recording: app", {
  # app_dir <- system.file("app", package = "Sensabled")
  # if (app_dir == "") {
  #   app_dir <- test_path("../../inst/app")
  # }
  # 
  # local_app_support(app_dir)
  # local_app_support(test_path(system.file()))
  library(Sensabled)
  app <- AppDriver$new(app_dir = run_app(), name = "app",  
                       load_timeout = 60000)
  app$expect_values()
  app$set_inputs(sub_ui = "Demo Data")
  app$set_inputs(menuBtn_state = FALSE)
  app$click("exampleFile")
  app$set_inputs(`shinyjs-delay-1f6ed96c98c85c62830b90307de462c8` = 100, allow_no_input_binding_ = TRUE)
  app$set_inputs(`shinyjs-delay-bc6a400a3f46c4bf48cd1f7cde6d7ef7` = 150, allow_no_input_binding_ = TRUE)
  app$set_inputs(DTtip_dropmenu = FALSE)
  app$click("DTtip")
  app$set_inputs(contents_rows_current = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), allow_no_input_binding_ = TRUE)
  app$set_inputs(contents_rows_all = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 
                                       14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 
                                       33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 
                                       52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 
                                       71, 72, 73, 74, 75), allow_no_input_binding_ = TRUE)
  app$set_inputs(contents_state = c(1788020174610, 0, 10, "", TRUE, FALSE, TRUE, 
                                    c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
                                                                                                      TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 0, 1, 2, 3), allow_no_input_binding_ = TRUE)
  # app$set_window_size(width = 1235, height = 730)
  app$set_inputs(confirmDemo = FALSE, allow_no_input_binding_ = TRUE)
  app$set_inputs(demo_guide = TRUE)
  # app$set_window_size(width = 1235, height = 730)
  app$click("runAnalysis")
  app$set_inputs(compList = "groupC")
  app$set_inputs(statCols = c("Vehicle", "Drug1<br>(2mM)", "Drug2<br>(10mM)", "Drug1+2"))
  app$set_inputs(askCorrection = "bonferroni")
  app$set_inputs(askCorrectionP = "bonferroni")
  app$set_inputs(askCorrectionG = "bonferroni")
  app$set_inputs(askControl = "Vehicle")
  app$set_inputs(askCorrectionC = "bonferroni")
  app$set_inputs(askCorrectionCG = "bonferroni")
  app$set_inputs(askCorrectionPC = "bonferroni")
  app$set_inputs(askCorrectionPCG = "bonferroni")
  app$set_inputs(statTwoCol = c("Vehicle", "Drug1<br>(2mM)"))
  app$wait_for_js(
    "document.querySelector('#runAnalysisFinal.shiny-bound-input') !== null",
    timeout = 15000
  )
  app$click("runAnalysisFinal")
  # app$set_window_size(width = 1235, height = 730)
  app$set_inputs(genAcc = character(0))
  app$set_inputs(genAcc = "annotePanel")
  # app$set_window_size(width = 1235, height = 730)
  app$set_inputs(`shinyjs-delay-dd95f0b3e877fd18cdff7730ad60614e` = 5000, allow_no_input_binding_ = TRUE)
  app$set_inputs(grplist = c("Vehicle vs Drug1(2mM)", "Vehicle vs Drug1+2", "Drug1(2mM) vs Drug1+2"))
  # app$set_window_size(width = 1235, height = 730)
  app$set_inputs(main_ui = "File Upload")
  app$set_inputs(contents_rows_current = character(0), allow_no_input_binding_ = TRUE)
  app$set_inputs(contents_rows_all = character(0), allow_no_input_binding_ = TRUE)
  app$set_inputs(contents_state = character(0), allow_no_input_binding_ = TRUE)
  app$set_inputs(contents_rows_current = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), allow_no_input_binding_ = TRUE)
  app$set_inputs(contents_rows_all = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 
                                       14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 
                                       33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 
                                       52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 
                                       71, 72, 73, 74, 75), allow_no_input_binding_ = TRUE)
  app$set_inputs(contents_state = c(1788020188806, 0, 10, "", TRUE, FALSE, TRUE, 
                                    c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
                                                                                                      TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 0, 1, 2, 3), allow_no_input_binding_ = TRUE)
  # app$set_window_size(width = 1235, height = 730)
  app$set_inputs(demoSheetList = "2")
  app$set_inputs(contents_rows_current = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), allow_no_input_binding_ = TRUE)
  app$set_inputs(contents_rows_all = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 
                                       14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 
                                       33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 
                                       52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 
                                       71, 72, 73, 74, 75), allow_no_input_binding_ = TRUE)
  app$set_inputs(contents_state = c(1788020190660, 0, 10, "", TRUE, FALSE, TRUE, 
                                    c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
                                                                                                      TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 0, 1, 2, 3), allow_no_input_binding_ = TRUE)
  # app$set_window_size(width = 1235, height = 730)
  app$click("exampleFile")
  app$set_inputs(DataTables_Table_2_length = "10")
  app$set_inputs(`shinyjs-delay-d66febd8d0b29f7bf581f428361b407b` = 100, allow_no_input_binding_ = TRUE)
  app$set_inputs(`shinyjs-delay-724f16caa7e9797a022f74377b76583e` = 150, allow_no_input_binding_ = TRUE)
  app$set_inputs(contents_rows_current = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), allow_no_input_binding_ = TRUE)
  app$set_inputs(contents_rows_all = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 
                                       14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 
                                       33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 
                                       52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66), allow_no_input_binding_ = TRUE)
  app$set_inputs(contents_state = c(1788020193545, 0, 10, "", TRUE, FALSE, TRUE, 
                                    c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
                                                                                                      TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
                                                                                                                                                            TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 
                                    c(TRUE, "", TRUE, FALSE, TRUE), 0, 1, 2, 3, 4, 5, 6, 7), allow_no_input_binding_ = TRUE)
  app$set_inputs(DataTables_Table_3_length = "10")
  app$set_inputs(confirmDemo = character(0), allow_no_input_binding_ = TRUE)
  # app$set_window_size(width = 1235, height = 730)
  app$set_inputs(confirmDemo = FALSE, allow_no_input_binding_ = TRUE)
  app$set_inputs(demo_guide = TRUE)
  # app$set_window_size(width = 1235, height = 730)
  app$click("runAnalysis")
  app$set_inputs(statCols = c("2hrs:Vehicle", "2hrs:Drug_(IC<sub>50</sub>)", "4hrs:Vehicle", 
                              "4hrs:Drug_(IC<sub>50</sub>)", "6hrs:Vehicle", "6hrs:Drug_(IC<sub>50</sub>)", 
                              "8hrs:Vehicle", "8hrs:Drug_(IC<sub>50</sub>)"))
  app$set_inputs(askControl = "2hrs:Vehicle")
  app$set_inputs(statTwoCol = c("2hrs:Vehicle", "2hrs:Drug_(IC<sub>50</sub>)"))
  app$wait_for_js(
    "document.querySelector('#runAnalysisFinal.shiny-bound-input') !== null",
    timeout = 15000
  )
  app$click("runAnalysisFinal")
  # app$set_window_size(width = 1235, height = 730)
  app$set_inputs(`shinyjs-delay-cfa0eabee2345d3604c07076b6fea9c8` = 5000, allow_no_input_binding_ = TRUE)
  # app$set_window_size(width = 1235, height = 730)
})
