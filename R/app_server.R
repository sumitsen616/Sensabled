### Source Code for SEN'sabale Plotting App ###
### MIT License - see LICENSE file for details
### Copyright (c) 2026 Sumit Sen

### Server Code ###

app_server <- function(input, output, session) {
  # Sets maximum file size (250 MB) that can be uploaded 
  options(shiny.maxRequestSize = 250 * 1024^2) 
  
  #Declaring reactive values
  current_colnames <- reactiveVal(NULL)
  obsBtn <- reactiveValues(submitVal=NULL, pasteVal=NULL, demoVal=NULL)
  submitFileBtn <- reactiveVal(FALSE)
  exampleFileBtn <- reactiveVal(FALSE)
  demoFile <- reactiveVal(NULL)
  demoSettFile <- reactiveVal(NULL)
  sheetTempName <- reactiveVal(NULL)
  
  ## Load demo data file
  observeEvent(input$exampleFile,{
    req(input$demoSheetList)
    demoFile(NULL)
    exampleFileBtn(TRUE)
    file <- openxlsx::read.xlsx(get_package_file('Example_data.xlsx'), sheet = safe_as_numeric(input$demoSheetList))
    demoFile(file)
  })
  ## Load demo setting file
  observeEvent(input$exampleFile,{
    req(demoFile())
    demoSettFile(NULL)
    file <- openxlsx::read.xlsx(get_package_file('Demo_Settings.xlsx'), sheet = safe_as_numeric(input$demoSheetList))
    demoSettFile(file)
  })
  
  #Automatic tab change to guide users for demo data plotting and analysis
  observeEvent(input$exampleFile,{
    shinyWidgets::show_alert(title = NULL,
                             text = tags$div(id = "demo-text",HTML("Data Loading <span id='ellipse'>...</span>")), type = "info",
                             closeOnClickOutside = FALSE,
                             btn_labels = NA)
    nav_select(
      id = "main_ui",
      selected = "Graph"
    )
    shinyjs::delay(100,{nav_select(
      id = "main_ui",
      selected = "Statistics"
    )})
    shinyjs::delay(150,{confirmSweetAlert(
      session = session,
      inputId = "confirmDemo",
      title = "Demo Data Loaded!",
      text = "Run analysis to visualize the plot",
      type = "info",
      btn_labels = "Okay",
      btn_colors = "#3459e6")})
  })
  observeEvent(input$confirmDemo,{
    toggle_popover("demo_guide")
  })
  observeEvent(input$runAnalysisFinal,{
    if (input$sub_ui == 'Demo Data' && isTruthy(input$exampleFile)){
      shinyjs::delay(5000,{
        nav_select(
          id = 'main_ui',
          selected = 'Graph'
        )
      })
    }
  })
  
  #Disables paste data or upload demo data button if XLSX file is uploaded
  observe({
    req(input$file)
    updateActionButton(session = session, 'pasteBtn',
                       'Upload Pasted Data', disabled = T)
    updateActionButton(session = session, 'exampleFile',
                       'Upload Example File', disabled = T)
  })
  #Disables upload demo data button if data is pasted
  observe({
    req(input$pasteBtn)
    updateActionButton(session = session, 'exampleFile', 'Upload Example File',
                       disabled = T)
  })
  
  #Sheet names handler: dynamic update
  observeEvent(input$file,{
    req(input$file$datapath)
    
    new_sheets <- tryCatch(
      openxlsx::getSheetNames(input$file$datapath),
      error = function(e) character(0)
    )
    if (length(new_sheets) == 0) {
      show_alert(
        title = "Invalid File Uploaded",
        "Could not read sheet names from the new file",
        type = "error")
      return()
    }
    sheetTempName(new_sheets)
    
    updateSelectInput(session, "sheetlist",
                      choices  = new_sheets,
                      selected = new_sheets[1])
    submitFileBtn(FALSE)
    current_colnames(NULL)
  })
  
  #Reset Submit event upon sheet name/ data change
  observeEvent(input$sheetlist,{
    submitFileBtn(FALSE)
  })
  observeEvent(input$demoSheetList,{
    exampleFileBtn(FALSE)
  })
  
  #Rendering options for selecting plot type
  output$fileupload <- renderUI({
    req(data())
    div(style="display:inline-flex;width:100%; flex-direction:column;",
        tagList(
          conditionalPanel(
            condition = "input.dataGroup",
            #Choosing plot type for grouped data
            pickerInput('askPlotTypeG',
                        label = 'Plot Type',
                        choices = plotG,
                        choicesOpt = list(content = plotG_img),
                        selected = 'violin')
          ), conditionalPanel(
            condition = "input.dataGroup == false",
            #Choosing plot type for ungrouped data
            pickerInput('askPlotType',
                        label = 'Plot Type',
                        choices = plotUG,
                        choicesOpt = list(content = plotUG_img),
                        selected = 'violin')
          )))
    
  })
  outputOptions(output,"fileupload", suspendWhenHidden = FALSE)
  #Rendering option for Upload data button
  output$uploadBtnShow <- renderUI({
    req(input$file,sheetTempName())
    actionButton('submitFile', 'Upload Datasheet', icon = icon('file-arrow-up'))
  })
  observeEvent(input$submitFile,{
    #Dynamically make submit button value true upon button click
    submitFileBtn(TRUE)
    shinyWidgets::show_toast(
      title = "Data Loaded Sucessfully",
      type = "success", timer = 2000
    )
    #Updates user about excel file upload in the data paste box
    updateTextAreaInput(session, 'pasted_Data', label = 'Paste data in the box',
                        placeholder = "Data uploaded through excel sheet.")
  })
  
  #Clear pasted data upon button click
  observeEvent(input$clearBox,{
    updateTextAreaInput(session, 'pasted_Data', value = "")
  })
  
  #Rendering option for current sheet names from the uploaded excel file
  output$sheetnames <- renderUI({
    req(sheetTempName())
    tagList(
      selectInput("sheetlist", "Select Datasheet",
                  choices = sheetTempName(), selected = sheetTempName()[1])
    )
  })
  
  #Code for reading pasted data table
  pasteDf <- reactiveValues(df = NULL)
  
  observeEvent(input$pasteBtn,{ 
    req(input$pasted_Data)
    pastedData <- read.delim(text = input$pasted_Data, 
                             header = TRUE,
                             sep = "\t",
                             check.names = FALSE,
                             stringsAsFactors = FALSE)
    pastedData <- as.data.frame(pastedData)
    
    #checks whether pasted data is a data-frame and gives an alert
    if (nrow(pastedData)>0 && isTRUE(is.data.frame(pastedData))){
      pasteDf$df <- pastedData
      shinyWidgets::show_toast(
        title ='Data Pasted Successfully',
        type = 'success')
    } else {
      show_alert(
        title = "Invalid Data",
        text = 'Please paste data table in wide format.', 
        type = 'error')
    }
  })
  
  #Rendering option to select whether data can be grouped  
  output$grpBtn <- renderUI({
    req(!is.null(input$file) ||
          (input$pasteBtn > 0 && isTruthy(input$pasted_Data)) ||
          !is.null(demoFile()))
    #Initial value for toggle button
    grpvalue <- FALSE
    if (input$demoSheetList == 1){
      grpvalue <- FALSE
    } else if (input$demoSheetList == 2){
      grpvalue <- TRUE
    } else {
      grpvalue <- FALSE
    }
    #Toggle button to decide data grouping
    div(class= 'groupBtn',
        prettyToggle(
          inputId = 'dataGroup',
          label_on = "",
          label_off = "",
          value = grpvalue,
          fill = T, bigger = T
        ),
        style = "width:100px; text-align:center; display:inline-flex;"
    )
  })
  #Resets toggle button for data group decision when user loads or pastes new data
  observeEvent(c(input$file, input$pasteBtn),{
    updatePrettyToggle(
      session,
      inputId = 'dataGroup',
      value = FALSE
    )
  })
  
  #Code to read and collect data column names 
  colNames <- reactive({
    df <- NULL
    #From uploaded data sheet
    if (!is.null(input$file)){
      req(input$sheetlist)
      df <- openxlsx::read.xlsx(input$file$datapath,
                                sheet = input$sheetlist,
                                colNames = TRUE) 
    }
    # From pasted data
    if (!is.null(pasteDf$df) && isTruthy(input$pasteBtn) && is.null(input$file)){
      df <- NULL
      df <- pasteDf$df
    } 
    # From demo data file
    if (isTruthy(input$exampleFile) && is.null(input$file) && !is.null(obsBtn$demoVal) ){
      req(demoFile())
      df <- NULL
      df <- demoFile()
      obsBtn$demoVal <- NULL
    } 
    
    namecol <- colnames(df)
    #removes any default hyphen in the column header (required for statistical analysis)
    if(has_element(str_detect(namecol,"-"),TRUE)){
      req(c(obsBtn$submitVal, obsBtn$pasteVal))
      shinyWidgets::show_toast(title = "Hyphen is not allowed in the column header.
                               Replaced with '_'",
                               text = NULL, type = 'warning')
    }
    namecol <- gsub('-','_',namecol)
    return(namecol)
  })
  
  #Rendering option for column picker UI (after submit or sheet change)
  output$colnames <- renderUI({
    req(current_colnames(),isTRUE(submitFileBtn()))
    pickerInput(
      inputId = "selectedCols",
      label = "Select Columns", 
      choices = current_colnames(),
      selected = current_colnames(),
      multiple = TRUE,
      options = pickerOptions(container = "body", actionsBox = TRUE),
      width = "100%"
    )
  })
  
  #Registering corresponding file upload button data to their reactive values
  observeEvent(input$submitFile,{
    obsBtn$submitVal <- input$submitFile
  })
  observeEvent(input$pasteBtn,{
    obsBtn$pasteVal <- input$pasteBtn
  })
  observeEvent(input$exampleFile,{
    obsBtn$demoVal <- input$exampleFile
  })
  
  observeEvent(c(obsBtn$submitVal, obsBtn$pasteVal, obsBtn$demoVal), {
    #Checks whether data upload button is clicked and registers their column names
    #and updates list for selecting columns to plot
    current_colnames(colNames())
    updatePickerInput(session, "selectedCols", "Select Columns",
                      choices = current_colnames(), selected = current_colnames())
  }, ignoreNULL = TRUE)
  
  
  observeEvent(c(obsBtn$submitVal, obsBtn$pasteVal, obsBtn$demoVal),{
    validate(
      need(isTRUE(is.numeric(data()[,1])), "Please add valid data.")
    )
    output$colupdateBttn <- renderUI({
      tagList(
        #Renders list of column names to be chosen for plotting
        uiOutput("colnames"),
        #Button to open modal box to change current column names
        actionButton('chngColBtn', 'Update Column Header', icon = icon('heading'), 
                     class = "btn-primary"),
        HTML("<p style = 'color: grey; font-size:13px; width:100%; text-align:center;'>
             &#9888; Click to Customize Major X-Axis Texts with Markdown &#9888; </p>")
      )
    })
  })
  
  # If user changes sheet after submit: reset names and selection
  observeEvent(c(input$sheetlist), {
    current_colnames(colNames())
    updatePickerInput(session, "selectedCols", "Select Columns",
                      choices = current_colnames(), selected = current_colnames())
  })
  observeEvent(c(input$demoSheetlist), {
    req(demoFile())
    current_colnames(colNames())
    updatePickerInput(session, "selectedCols", "Select Columns",
                      choices = current_colnames(), selected = current_colnames())
  })
  
  #Updates column names and data if users reorder columns of displayed table by dragging 
  current_column_order <- reactiveVal(seq_along(data()))
  observeEvent(input$current_column_order,{
    #get the new order index of the rearranged columns by user
    order <- as.vector(unlist(input$current_column_order)[3:(length(data())+2)])+1
    #Readjust the column names and update current column name
    new_order <- current_colnames()[c(order)]
    current_colnames(c(new_order))
    updatePickerInput(session, "selectedCols", "Select Columns",
                      choices = current_colnames(), selected = current_colnames())
  })
  
  ### Main data reactive: read full sheet, apply current names, then subset ###
  
  data <- reactive({
    df_full <- NULL
    
    ## 1. Data process if excel file is uploaded ##
    if (!is.null(input$file) && !is.null(input$sheetlist)) {
      req(input$file,input$sheetlist,isTRUE(submitFileBtn()))
      
      current_sheets <- openxlsx::getSheetNames(input$file$datapath)
      #fail-safe if sheet name mismatch
      if (!input$sheetlist %in% current_sheets) {
        show_alert(
          title = "Invalid Sheetname",
          text = paste("Sheet", input$sheetlist, "not found in the new file.
                     Please click 'Upload Datasheet' again."),
          type = "error"
        )
        return(NULL)
      }
      #Reads excel file as data frame
      df_full <- openxlsx::read.xlsx(
        input$file$datapath,
        sheet = input$sheetlist,
        colNames = TRUE,
        skipEmptyRows = TRUE,
        fillMergedCells = TRUE
      )
    }
    
    ## 2. Data process if data table is pasted ##
    if (is.null(df_full) && !is.null(pasteDf$df) && is.data.frame(pasteDf$df)) {
      df_full <- pasteDf$df
    }
    
    ## 3. Demo data process ##
    if(!is.null(demoFile()) && is.null(pasteDf$df) &&
       is.null(input$file)){
      df_full <- demoFile()
    } 
    
    #Checks whether correct data table loaded
    # Contains data, numeric, a table, more than  one columns
    if (is.null(df_full) || !is.data.frame(df_full) || nrow(df_full) == 0 ||
        isTRUE(has_element(sapply(df_full,is.numeric),FALSE)) || ncol(df_full)<2) {
      req(input$submitFile>0)
      show_alert(
        title = "Invalid Data",
        text = "No valid data loaded.", type = "error")
      #When uploaded data is wrong, resets temporary reactive value to store sheet names
      #to read any change in sheet names when new data is uploaded
      sheetTempName(NULL)
      return(NULL)
    }
    
    #If data group is active then check if the columns names can be grouped
    #grouping data requires parameters to be separated by ':' in the column header
    if(isTRUE(input$dataGroup)){
      order_data <- df_full |> pivot_longer(names_to = c('variable','groups'),
                                            names_sep = ':',
                                            values_to = 'val',
                                            cols = everything()) |> arrange(variable)
      
      #rejects if there is an ungroupable parameter, 
      #for e.g. A:X, A:Y, A:Z, B:X, B:Y, B:Z is correct, 
      #but suddenly A:Q or S:X in the same table is not expected
      eqlN <- order_data |> group_by(variable) |> count()
      eqlG <- order_data |> group_by(groups) |> count()
      eqlN_len <- length(unique(eqlN$n))
      eqlG_len <- length(unique(eqlG$n))
      if(eqlN_len != 1 || eqlG_len != 1){
        show_alert(title = "Invalid Column Name",
                   text = "Please check there is no mismatch in column headers.",
                   type = "error")
        df_full <- NULL
        submitFileBtn(FALSE)
        return()
      }
    }
    
    ## When the columns were reordered, update the data table
    df_full <- df_full[,current_colnames()]

    # Use current_colnames() to rename the dataset if number of columns matches
    if (ncol(df_full) == length(current_colnames())) {
      colnames(df_full) <- current_colnames()
      df_full <- df_full[,current_colnames()]
    } else {
      req(input$submitFile>0)
      shinyWidgets::show_toast(
        title = "Warning",
        text ="Column count mismatch after loading &#8212; using original names.",
        type = "warning")
      return(NULL)
    }
    
    
    # Subset only if selectedCols exist and are valid
    selected <- input$selectedCols %||% current_colnames()
    selected <- intersect(selected, colnames(df_full))
    
    if (length(selected) == 0) {
      req(input$selectedCols)
      shinyWidgets::show_toast(title = "No valid columns selected.", type = "warning")
      return(NULL)
    }
    df_full[, selected, drop = FALSE]
  })  
  
  # Renders Main Data table
  output$contents <- renderDT({
    req(data())
    #columns can be reordered, data can be edited
    DT::datatable(as.data.frame(data()), 
                  editable = TRUE,
                  rownames = F,
                  extensions = 'ColReorder',
                  callback = callback_js,
                  options = list(
                    columnDefs = list(list(className = 'dt-left', targets = '_all')),
                    colReorder = TRUE
                  ))
    
  })
  
  #Data table info button and list of suggestions
  output$DTtipOut <- renderUI({
    req(data())
    dropMenu(
      actionBttn("DTtip", label = NULL, icon = icon("circle-question"),
                 style = "unite"),
      tags$div(
        tags$h5("Table Editing Tips"),
        tags$li('Drag column headers to reorder'),
        tags$li('Double click a cell to edit value'),
        tags$li("Select columns from the list under Data Settings Panel"),
        tags$li("Update column titles by clicking 'Update Column Header' button 
                in the Data Settings Panel"),
        style="padding:10px;"), label = NULL,
      placement = 'right', arrow = FALSE,
      padding = "5px", trigger = "click",
      hideOnClick = TRUE, icon = icon('circle-question'), theme='material')
  })
  
  #Code for renaming column headers
  observeEvent(input$chngColBtn, {
    #Markdown styles list
    choice_labels <- c(
      "<b>B</b>", 
      "<i>i</i>", 
      "x&#178", 
      "x&#8322",
      "&#8629;"
    )
    choice_values <- c(
      "**", 
      "*", 
      "<sup>", 
      "<sub>",
      "<br>"
    )
    combNames <- setNames(choice_values, choice_labels)
    #Modal to show column headers text inputs
    showModal(modalDialog(
      title = "Update Column Headers",
      #Markdown list
      radioGroupButtons(
        'mdHead',label=NULL,
        choices = combNames, selected = character(0),
        justified=  F, individual = T, size = 'sm', width = "500px"
      ),
      #Button to add the markdown
      actionButton('addmdHead', label = c('Add Markdown')),
      #Renders the text inputs for data columns for editing
      uiOutput('newColOut'),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        #Button to finalize the changes in column headers
        actionButton("saveColNames", "Save Changes", class = "btn-primary")
      )
    ))
  })
  
  # Modal showing current column names to be edited
  output$newColOut <- renderUI({
    req(data())
    #Dynamically creates text inputs for individual column names
    lapply(seq_along(data()), function(i) {
      textInput(
        paste0("newcolname_", i),
        label = paste("Column", i, "- current:", colnames(data())[i]),
        value = current_colnames()[i]
      )
    })
  })
  # Markdown update with selected-text wrapping feature
  # Generated using Gemini and Grok AI
  # Define what each button should do (open + close tags)
  markdown_wrappers <- reactive({
    list(
      "**"    = list(open = "**",   close = "**"),      # Bold
      "*"     = list(open = "*",    close = "*"),       # Italic
      "<sup>" = list(open = "<sup>",close = "</sup>"),  # Superscript
      "<sub>" = list(open = "<sub>",close = "</sub>"),  # Subscript
      "<br>"  = list(open = "<br>", close = "")         # Line break (just insert)
    )
  })
  observeEvent(input$addmdHead, {
    
    # Safety checks
    req(input$active_input, input$mdHead)
    
    # Get the wrapper safely
    wrapper <- markdown_wrappers()[[input$mdHead]]
    
    if (is.null(wrapper)) {
      shinyWidgets::show_toast(
        title = "Warning",
        text = "Unknown markdown tag selected", type = "warning")
      return()
    }
    
    # Send to JavaScript (wraps selected text!)
    session$sendCustomMessage("wrapText", list(
      id    = input$active_input,
      open  = wrapper$open,
      close = wrapper$close
    ))
  })
  
  # Saves renamed columns
  observeEvent(input$saveColNames, {
    req(current_colnames())
    old_names <- current_colnames()
    new_names <- sapply(seq_along(old_names), function(i) {
      val <- input[[paste0("newcolname_", i)]]
      if (is.null(val) || trimws(val) == "") old_names[i] else trimws(val)
    })
    
    #Prevent user to add hyphen in the column names (as it conflicts later with stat result)
    if(isTRUE(has_element(str_detect(new_names,'-'),TRUE))){
      show_alert(
        title = "Invalid Column Header",
        text = 'Hyphen is not allowed in the column names.',
        type = 'error')
      return()
    } 
    #Check if there is any mismatch in making groups in grouped data condition
    if (isTRUE(input$dataGroup) && any(grepl(":", new_names, fixed = TRUE))){
      df <- data()
      colnames(df) <- new_names
      order_data <- df |> pivot_longer(names_to = c('variable','groups'),
                                       names_sep = ':',
                                       values_to = 'val', cols = everything()) |> arrange(variable)
      eqlN <- order_data |> group_by(variable) |> count()
      eqlG <- order_data |> group_by(groups) |> count()
      eqlN_len <- length(unique(eqlN$n))
      eqlG_len <- length(unique(eqlG$n))
      
      if(eqlN_len != 1 || eqlG_len != 1){
        show_alert(title = "Invalid Column Name",
                   text = "Please check there is no mismatch in column headers.",
                   type = "error")
        return()
      }
    }
    
    #Update the new column headers
    current_colnames(new_names)
    removeModal()
    
    # Preserve selection by matching old names for selected columns
    selected_new <- new_names[old_names %in% (input$selectedCols %||% old_names)]
    updatePickerInput(session, "selectedCols",
                      choices = new_names, selected = selected_new)
    
  })
  
  #Live updates selected plot type between File Upload and Graph View page
  observeEvent(input$askPlotType, {
    updatePickerInput(session, "askPlotTypeII", selected = input$askPlotType)
  })
  observeEvent(input$askPlotTypeG, {
    updatePickerInput(session, "askPlotTypeIIG", selected = input$askPlotTypeG)
  }) 
  
  observeEvent(input$askPlotTypeII, {
    updatePickerInput(session, "askPlotType", selected = input$askPlotTypeII)
  })
  observeEvent(input$askPlotTypeIIG, {
    updatePickerInput(session, "askPlotTypeG", selected = input$askPlotTypeIIG)
  })
  
  
  ## Processing Input Raw Data for plotting ##
  
  orderdata <- reactive({
    req(data())
    
    if (input$dataGroup == T){
      # If data grouping is selected
      
      #Grouping parameter selected by user 
      #(Parameter name present before or after ':' in column header )
      if(isTRUE(input$grpSwitch)){
        colsOrder <- c("variable", "groups")
      } else { 
        colsOrder <- c("groups","variable")
      }
      #Converting wide to long data format required for ggplot()
      order_data <-
        data() |> pivot_longer(cols = everything(),
                               names_to = colsOrder,
                               names_sep = ':',
                               values_to = "value") |>
        arrange(variable)
    } else {
      # If data grouping is not selected
      #Converting wide to long data format required for ggplot()
      order_data <-
        data() |> pivot_longer(cols = everything(),
                               names_to = "variable",
                               values_to = "value") |>
        arrange(variable)}
    return(order_data)
  })
  
  ### Server side calculation or processing for different plot-related functions ###
  
  ## Graph Axis Processing
  axistitleY <- reactive({
    aytitle <- as.character(req(input$aytitle))
  })
  axistitleX <- reactive({
    axtitle <- (input$axtitle)
  })
  
  ## Rendering options for special characters
  #Plot title
  output$showsymbolTit <- renderUI({
    if (input$chksymbolTit == T) {
      tagList(
        radioGroupButtons(
          'symbolsTit',
          label = c('Select from the list'),
          choices = intToUtf8(c(945, 946, 947, 948, 949, 950, 951,
                                952, 953, 954, 955, 956, 957, 958,
                                959, 960, 961, 963, 964, 965, 966,
                                967, 968, 969),multiple = T),
          justified = T, individual = F, size = 'sm' 
        ),
        actionButton('addsymbolTit', label = c('Add Character'))
      )
    }
  })
  #Y-axis title
  output$showsymbolY <- renderUI({
    if (input$chksymbolY == T) {
      tagList(
        radioGroupButtons(
          'symbolsY',
          label = c('Select from the list'),
          choices = intToUtf8(c(945, 946, 947, 948, 949, 950, 951,
                                952, 953, 954, 955, 956, 957, 958,
                                959, 960, 961, 963, 964, 965, 966,
                                967, 968, 969),multiple = T),
          justified = T, individual = F, size = 'sm' 
        ),
        actionButton('addsymbolY', label = c('Add Character'))
      )
    }
  })
  #X-axis title
  output$showsymbolX <- renderUI({
    if (input$chksymbolX == T) {
      tagList(
        radioGroupButtons(
          'symbolsX',
          label = c('Select from the list'),
          choices = intToUtf8(c(945, 946, 947, 948, 949, 950, 951,
                                952, 953, 954, 955, 956, 957, 958,
                                959, 960, 961, 963, 964, 965, 966,
                                967, 968, 969),multiple = T),
          justified = T, individual = F, size = 'sm' 
        ),
        actionButton('addsymbolX', label = c('Add Character'))
      )
    }
  })
  
  ## Adds speacial characters
  #Plot title
  observeEvent(input$addsymbolTit, {
    req(input$active_input, input$symbolsTit)
    
    symbol <- input$symbolsTit
    
    # Send to JavaScript (wraps selected text!)
    session$sendCustomMessage("wrapText", list(
      id    = input$active_input,
      open  = symbol,
      close = ""
    ))
  })
  #Y-axis title
  observeEvent(input$addsymbolY, {
    req(input$active_input, input$symbolsY)
    
    symbol <- input$symbolsY
    
    # Send to JavaScript (wraps selected text!)
    session$sendCustomMessage("wrapText", list(
      id    = input$active_input,
      open  = symbol,
      close = ""
    ))
  })
  #X-axis title
  observeEvent(input$addsymbolX, {
    req(input$active_input, input$symbolsX)
    
    symbol <- input$symbolsX
    
    # Send to JavaScript (wraps selected text!)
    session$sendCustomMessage("wrapText", list(
      id    = input$active_input,
      open  = symbol,
      close = ""
    ))
  })
  
  ## Rendering options for markdown
  #Plot-title
  output$showMarkdownTit <- renderUI({
    if (isTRUE(input$markdownTit)) {
      choice_labels <- c(
        "<b>B</b>", 
        "<i>i</i>", 
        "x&#178", 
        "x&#8322",
        "&#8629;"
      )
      choice_values <- c(
        "**", "*", "<sup>", 
        "<sub>", "<br>")
      
      combNames <- setNames(choice_values, choice_labels)
      tagList(
        radioGroupButtons(
          'mdTit',label=NULL,
          choices = combNames,
          justified=  T, individual = F, size = 'sm'
        ),
        actionButton('addmdTit', label = c('Add Markdown'))
      )
    }
  })
  observeEvent(input$addmdTit, {
    # Safety checks
    req(input$active_input, input$mdTit)
    
    # Get the wrapper safely
    wrapper <- markdown_wrappers()[[input$mdTit]]
    
    if (is.null(wrapper)) {
      shinyWidgets::show_toast(
        title = "Warning",
        text = "Unknown markdown tag selected", type = "warning")
      return()
    }
    
    # Send to JavaScript (wraps selected text!)
    session$sendCustomMessage("wrapText", list(
      id    = input$active_input,
      open  = wrapper$open,
      close = wrapper$close
    ))
  })
  #Y-axis title
  output$showMarkdownY <- renderUI({
    if (isTRUE(input$markdownY)) {
      choice_labels <- c(
        "<b>B</b>", 
        "<i>i</i>", 
        "x&#178", 
        "x&#8322",
        "&#8629;"
      )
      choice_values <- c(
        "**", "*", "<sup>", 
        "<sub>", "<br>"
      )
      combNames <- setNames(choice_values, choice_labels)
      tagList(
        radioGroupButtons(
          'mdy',label=NULL,
          choices = combNames,
          justified=  T, individual = F, size = 'sm'
        ),
        actionButton('addmdY', label = c('Add Markdown'))
      )
    }
  })
  observeEvent(input$addmdY, {
    req(input$active_input, input$mdy)
    wrapper <- markdown_wrappers()[[input$mdy]]
    
    if (is.null(wrapper)) {
      shinyWidgets::show_toast(
        title = "Warning",
        text = "Unknown markdown tag selected", type = "warning")
      return()
    }
    
    # Send to JavaScript (wraps selected text!)
    session$sendCustomMessage("wrapText", list(
      id    = input$active_input,
      open  = wrapper$open,
      close = wrapper$close
    ))
  })
  #X-axis title
  output$showMarkdownX <- renderUI({
    if (isTRUE(input$markdownX)) {
      choice_labels <- c(
        "<b>B</b>", 
        "<i>i</i>", 
        "x&#178", 
        "x&#8322",
        "&#8629;"
      )
      choice_values <- c(
        "**", "*", "<sup>", 
        "<sub>", "<br>"
      )
      combNames <- setNames(choice_values, choice_labels)
      tagList(
        radioGroupButtons(
          'mdx',label=NULL,
          choices = combNames,
          justified=  T, individual = F, size = 'sm'
        ),
        actionButton('addmdX', label = c('Add Markdown'))
      )
    }
  })
  observeEvent(input$addmdX, {
    req(input$active_input, input$mdx)
    
    wrapper <- markdown_wrappers()[[input$mdx]]
    
    if (is.null(wrapper)) {
      shinyWidgets::show_toast(
        title = "Warning",
        text = "Unknown markdown tag selected", type = "warning")
      return()
    }
    
    # Send to JavaScript (wraps selected text!)
    session$sendCustomMessage("wrapText", list(
      id    = input$active_input,
      open  = wrapper$open,
      close = wrapper$close
    ))
  })
  
  
  ## X-Axis text position horizontally
  xcolPosH <- reactive({
    if(input$Xrotate == 0){
      return(0.5)
    }else {
      return(1)
    }
    # seq(0.5, 1, length.out = 90)
  })
  ## X-Axis text position vertically
  xcolPosV <- reactive({
    if(input$Xrotate == 90){
      return(0.5)
    }else {
      return(1)
    }
  })
  
  ## Y axis log transformation
  logaxis <- reactive({
    logscale <- switch(input$logscale,
                       none = 'identity', log10 = 'log10', log2 = 'log2')
  })
  ## Plot Title Text alignment 
  titlePlot <- reactive({
    plotT <- switch(input$titlePos, left = 0, center = 0.5, right = 1)
  })
  ## Y axis scientific transformation
  Ycontax <- reactive({
    if (input$labelY=='Scientific'){
      scale_y_continuous (labels=scales::scientific, trans = logaxis(),
                          expand = c(0,0.1))
    }else{
      scale_y_continuous (trans = logaxis(), expand = c(0,0.1))
    }
  })
  ## Y axis maximum and minimum range
  yaxisMin <- reactive({
    if (is.na(input$minY) == TRUE) {
      min_val <- NA
    } else{
      if (logaxis()!='identity' && input$minY==0){
        min_val <- NA
      } else{
        min_val <- input$minY
      }
    }
  })
  yaxisMax <- reactive({
    if (is.na(input$maxY) == TRUE) {
      req(orderdata())
      max_val <- max(orderdata()$value, na.rm = TRUE)* 1.02
    } else{
      max_val <- input$maxY
    }
  })
  
  ### Plot Customization ###
  
  #Storing reactive values for individual point shape for different plot types
  shapeBox <- reactiveValues(shapes = list()) #Box-Whisker plot
  shapeRain <- reactiveValues(shapes = list()) #Rain-cloud plot
  shapeJitter <- reactiveValues(shapes = list()) #Jitter plot
  
  #Count column numbers
  colCount <- reactive({
    if (isTRUE(input$dataGroup)) {
      return(unique(orderdata()$groups))
    } else {
      return(data())
    }
  })
  
  # Datapoint shapes For Box-Whisker Plot
  dpshapeBox <- reactive({
    if (isFALSE(input$pointDistBox)) {
      #Assigns same point shape to all
      dpshapes <- rep(c(input$pointshapeBox), length(colCount()))
    } else if (isTRUE(input$pointDistBox)) {
      #Assigns individual point shapes
      dpshapes <- lapply(seq_along(colCount()), function(i) {
        input[[paste("pointshapeBox", i, sep = '_')]]
      })
    }
    return(c(dpshapes))
  })
  pointShapeBoxOut <- reactive({
    req(data(), current_colnames())
    if (isTRUE(input$dataGroup)){
      cols <- length(colCount())
      coln <- colCount()
    } else {
      cols <- ncol(data())
      coln <- colnames(data())
    }
    # Generates the list of buttons for individual columns
    div(
      class = "truncated_title_point",
      lapply(seq_len(cols), function(i) {
        id <- paste("pointshapeBox", i, sep = '_')
        initial_val <- isolate(shapeBox$shapes[[id]])
        if (is.null(initial_val)) initial_val <- 21 
        radioGroupButtons(
          inputId = id,
          label = paste("Point Shape (",coln[i], ")", sep = ''),
          choices = iconlist,
          justified = T, size = 'sm', selected = initial_val
        )
      }
      ))
  })
  output$pointInpUIBox <- renderUI({
    req(isFALSE(input$dataGroup)) #Important
    pointShapeBoxOut()
  })
  output$pointInpUIBoxG <- renderUI({
    req(isTRUE(input$dataGroup)) #Important
    pointShapeBoxOut()
  })
  observe({
    ## To observe any changes in new point shape box inputs and update accordingly
    req(data(),isTRUE(input$pointDistBox))
    ids <- paste("pointshapeBox", seq_along(colCount()), sep = '_')
    
    for (id in ids) {
      curr_input <- input[[id]]
      if (isTruthy(input$reuseset)){
        shapeBox$shapes[[id]] <- shapeBox$shapes[[id]]
      } else {
        shapeBox$shapes[[id]] <- curr_input
      }
    }
  })
  
  #Datapoint shapes for Jitter Plot
  dpshape <- reactive({
    if (isFALSE(input$pointDist)) {
      #Assigns same point shape to all
      dpshapes <- rep(c(input$pointshape), ncol(data()))
    } else if (isTRUE(input$pointDist)) {
      #Assigns individual point shapes
      dpshapes <- lapply(seq_along(colCount()), function(i) {
        input[[paste("pointshape", i, sep = '_')]]
      })
    }
    return(c(dpshapes))
  })
  output$pointInpUI <- renderUI({
    req(data()) 
    cols <- ncol(data())
    coln <- colnames(data())
    # Generate the list of buttons for individual columns
    div(
      class = "truncated_title_point",
      lapply(seq_len(cols), function(i) {
        id <- paste("pointshape", i, sep = '_')
        initial_val <- isolate(shapeJitter$shapes[[id]])
        if (is.null(initial_val)) initial_val <- 21 
        radioGroupButtons(
          inputId = id,
          label = paste("Point Shape (",coln[i], ")", sep = ''),
          choices = iconlist,
          justified = T, size = 'sm', selected = initial_val
        )
      }
      ))
  })
  observe({
    ## To observe any changes in new point shape jitter inputs and update accordingly
    req(data(),isTRUE(input$pointDist))
    ids <- paste("pointshape", seq_along(colCount()), sep = '_')
    
    for (id in ids) {
      curr_input <- input[[id]]
      if (isTruthy(input$reuseset)){
        shapeJitter$shapes[[id]] <- shapeJitter$shapes[[id]]
      } else {
        shapeJitter$shapes[[id]] <- curr_input
      }
    }
  })
  
  # Datapoint shapes For Raincloud Plot
  dpshapeRain <- reactive({
    if (isFALSE(input$pointDistRain)) {
      #Assigns same point shape to all
      dpshapes <- rep(c(input$pointshapeRain), ncol(data()))
    } else if (isTRUE(input$pointDistRain)) {
      #Assigns individual point shapes
      dpshapes <- lapply(seq_along(data()), function(i) {
        input[[paste("pointshapeRain", i, sep = '_')]]
      })
    }
    return(c(dpshapes))
  })
  output$pointInpUIRain <- renderUI({
    req(data()) 
    cols <- ncol(data())
    coln <- colnames(data())
    # Generate the list of buttons for individual columns
    div(
      class = "truncated_title_point",
      lapply(seq_len(cols), function(i) {
        id <- paste("pointshapeRain", i, sep = '_')
        initial_val <- isolate(shapeRain$shapes[[id]])
        if (is.null(initial_val)) initial_val <- 21 
        radioGroupButtons(
          inputId = id,
          label = paste("Point Shape (",coln[i], ")", sep = ''),
          choices = iconlist,
          justified = T, size = 'sm', selected = initial_val
        )
      }
      ))
  })
  observe({
    ## To observe any changes in new point shape raincloud inputs and update accordingly
    req(data(),isTRUE(input$pointDistRain))
    ids <- paste("pointshapeRain", seq_along(colCount()), sep = '_')
    
    for (id in ids) {
      curr_input <- input[[id]]
      if (isTruthy(input$reuseset)){
        shapeRain$shapes[[id]] <- shapeRain$shapes[[id]]
      } else {
        shapeRain$shapes[[id]] <- curr_input
      }
    }
  })
  
  ## Internal value adjusting for ggplot features
  #Point scatter for box plot
  addScatterBox <- reactive({
    if (input$boxtype == "boxpoint") {
      scatter <- (input$scatterBox)/25
    }
  })
  #Point size for box plot
  addPointBox <- reactive({
    if (input$boxtype == "boxpoint") {
      (input$pointsizeBox)/30
    } else if (input$boxtype == "boxOnly"){
      return(0)
    }
  })
  
  
  ## Plot Area Processing ##
  plotBorder <- reactive({
    if (isTRUE(input$plotThemeBorder)){
      element_rect(size=input$axisline/40)
    } else{
      element_blank()
    }
  })
  majGridInpX <- reactive({
    if (isTRUE(input$plotThemeGrid) && 
        isTRUE(input$majGrid) && has_element(input$gridOpt,'X')){
      element_line(size = 0.5, linetype = 'solid',
                   colour = input$gridCol)
    }else {
      element_blank()
    }
  })
  minGridInpX <- reactive({
    if (isTRUE(input$plotThemeGrid) &&
        isTRUE(input$minGrid) && has_element(input$gridOpt,'X')){
      element_line(size = 0.25, linetype = 'solid',
                   colour = input$gridCol)
    }else {
      element_blank()
    }
  })
  majGridInpY <- reactive({
    if (isTRUE(input$plotThemeGrid) &&
        isTRUE(input$majGrid) && has_element(input$gridOpt,'Y')){
      element_line(size = 0.5, linetype = 'solid',
                   colour = input$gridCol)
    }else {
      element_blank()
    }
  })
  minGridInpY <- reactive({
    if (isTRUE(input$plotThemeGrid) &&
        isTRUE(input$minGrid) && has_element(input$gridOpt,'Y')){
      element_line(size = 0.25, linetype = 'solid',
                   colour = input$gridCol)
    }else {
      element_blank()
    }
  })
  
  #Resets summary stat info display button until statistical tests are performed
  observe({
    req(input$dpview)
    if (!isTruthy(input$runAnalysisFinal) && isTRUE(input$dpview)){
      updatePrettySwitch(session,'dpview', value = FALSE)
    }
    
  })
  #Summary Stat Info processing
  addDataPointLabel <- reactive({
    
    if (!isTruthy(input$runAnalysisFinal) && isTRUE(input$dpview)){
      req(descStat())
      show_alert(
        title = "Missing Data",
        text = 'Please run statistical analysis first.', type = 'warning')
      return(NULL)
    }
    
    temp <- descStat() # Draws from descriptive statistical analysis
    newdf <- data.frame()
    if(isFALSE(input$dpview) || !isTruthy(input$runAnalysisFinal)){
      text <- rep("",ncol(data()))
    } else {
      if (input$dpviewInfo == 'mean'){
        text <- temp$Mean
      } else if (input$dpviewInfo == 'median'){
        text <- temp$Median
      } else if (input$dpviewInfo == 'count'){
        text <- temp$N
      } else if (input$dpviewInfo == 'sd'){
        text <- temp$`Std. Dev.`
      } else if (input$dpviewInfo == 'sem'){
        text <- temp$`Std. Err.`
      }
    }
    #Setting up X and Y coordinates to position the Desc Stat Info
    minval <- min(data(), na.rm = TRUE)
    textPosSeq <- seq(-yaxisMax(),yaxisMax(), length.out=100)
    
    Ycord <- as.data.frame(rep(minval)+textPosSeq[input$dpviewPos], ncol(data()))
    Xcord <- if (isTRUE(input$reverseX)) data.frame(ncol(data()):1) else data.frame(1:ncol(data()))
    
    finaldf <- cbind(formatC(text,format = 'f', digits = safe_as_numeric(input$dpviewDecmP)),
                     Xcord, Ycord)
    colnames(finaldf) <- c('text', 'x', 'y')
    finaldf$cond <- gsub("[.]",' ',colnames(data()))
    
    #Readjusting X-Y position for grouped data (there is spacing between groups)
    if(isTRUE(input$dataGroup)){
      req(dfGmap())
      
      ccp <- c(dfGmap()[[1]],dfGmap()[[2]])
      
      tempDf <- str_split_fixed(finaldf$cond,':',2)|> as.data.frame()
      rownames(tempDf) <- NULL
      finaldf <- cbind(finaldf,tempDf)
      for(i in 1:nrow(tempDf)){
        finaldf$x[i] <- ccp[tempDf[i,2]]-1+ccp[tempDf[i,1]]
      }
      
      #Revereses desc stat info X position
      if (isTRUE(input$reverseX)){
        if(isTRUE(input$grpSwitch)){
          finaldf <- finaldf |> arrange(V1) |> group_by(V1) |>
            mutate(x = rev(x)) |> ungroup()
        } else{
          finaldf <- finaldf |> arrange(V1) |> group_by(V2) |>
            mutate(x = rev(x)) |> ungroup()
        }
      }else{
        finaldf <- finaldf |> arrange(V1)
      }
      
      return(finaldf)
    }else{
      return(finaldf)
    }
  })
  
  ### Plot Theme processing ###
  
  ## Generating different theme types for shapes##
  addTheme <- reactive({
    if (isTRUE(input$dataGroup)){
      ## When data is grouped
      if (input$choosethemeII == 'paletteG'){
        #Creates individual color inputs
        usertheme <- lapply(seq_along(colCount()), function(i) {
          input[[paste("colorsG", i, sep = '_')]]
        })
        usertheme <- unlist(usertheme)
      } else{
        #Creates a gradient of colours based on total number of variables per group
        usertheme <- colorRampPalette(c("#EEE1EF", "#554994"))(length(colCount()))
      }
    } else {
      # When data is ungrouped
      if (input$choosetheme == 'default') {
        #Creates a gradient of colours based on total number of data columns
        usertheme <- colorRampPalette(c("#EEE1EF", "#554994"))(ncol(data()))
      } else if (input$choosetheme == 'preset') {
        # Makes gradients to generate preset color shades
        usertheme <- switch(
          input$boxtheme,
          purples = colorRampPalette(c("#EEE1EF", "#554994"))(ncol(data())),
          greens = colorRampPalette(c("#B5D1AE", "#122740"))(ncol(data())),
          greens2 = colorRampPalette(c("#9ADA81", "#33B061","#054239"))(ncol(data())),
          pinks = colorRampPalette(c('#FDE4DE', '#F56093'))(ncol(data())),
          oranges = colorRampPalette(c('#FFAE01', '#C70E00'))(ncol(data())),
          blues = colorRampPalette(c('#1BFFFF', '#2E3192'))(ncol(data())),
          golds = colorRampPalette(c('#FDD700', '#8C3617'))(ncol(data())),
          rainbows = colorRampPalette(c('#bae1ff', '#ffb3ba'))(ncol(data())),
          season = colorRampPalette(c('#F4E867', '#DA4B82', '#387494'))(ncol(data())),
          heatmap = colorRampPalette(c('#2066A8', '#EDEDED', '#AE282C'))(ncol(data())),
          colorblind = colorRampPalette(c('#F5EADA','#768267', '#304659'))(ncol(data())),
          greys = colorRampPalette(c('#C0C0C2', '#373737'))(ncol(data()))
        )
      }
      else if (input$choosetheme == 'gradient') {
        #Users can create a gradient
        usertheme <- colorRampPalette(c(input$grad1, input$grad2))(ncol(data()))
      }
      else if (input$choosetheme == 'palette') {
        #Creates individual color inputs
        usertheme <- lapply(seq_along(colCount()), function(i) {
          input[[paste("colors", i, sep = '_')]]
        })
      }
    }
    #Passes through grayscale filter to check contrast
    if (input$grayscale == "Yes") {
      theme <- ColToGray(usertheme)
    } else{
      theme <- usertheme
    }
    return(theme)
  })
  ## Render condition specific datapoint color options ##
  observeEvent(input$askPlotTypeII, {
    req(input$askPlotTypeII)
    
    if (input$askPlotTypeII == 'jitter') {
      updatePickerInput(
        session = session, inputId = 'dpcolor',
        choices = c(
          "Default (Black)" = 'default',
          "Make Gradient" = 'dpgradient',
          "Select Individual" = 'dppalette'
        )
      ) 
    } 
  })
  ## Generating different theme types for data point outline ##
  dpcolors <- reactive({
    if (input$dataGroup == T){
      if (input$dpcolorG == 'defaultG') {
        dpcolors <- rep(c('#000000'), length(unique(orderdata()$groups)))
      } else if (input$dpcolorG == 'borderG') {
        dpcolors <- as.list(bordercolor())
      }
    } else {
      dpcolors <- rep(c('#000000'), length(data()))
      if (input$dpcolor == 'default') {
        dpcolors <- rep(c('#000000'), length(colCount()))
      } else if (input$dpcolor == 'box') {
        dpcolors <- paletteTheme()
      } else if (input$dpcolor == 'border') {
        dpcolors <-  bordercolor()
      } else if (input$dpcolor == 'dpgradient') {
        dpcolors <- colorRampPalette(c(input$dpgrad1, input$dpgrad2))(length(colCount()))
      } else if (input$dpcolor == 'dppalette') {
        dpcolors <- lapply(seq_along(colCount()), function(i) {
          input[[paste("colorsdp", i, sep = '_')]]}
        )
      }
    }
    #Passes through grayscale filter to check contrast
    if (input$grayscale == "Yes") {
      theme <- ColToGray(dpcolors)
    } else{
      theme <- dpcolors
    }
    return(theme)
  })
  ## Render condition specific datapoint fill color options ##
  
  observeEvent(input$askPlotTypeII, {
    req(input$askPlotTypeII)
    
    if (input$askPlotTypeII == 'jitter') {
      updatePickerInput(
        session = session, inputId = 'dpfill',
        choices = c(
          "Default (Black)" = 'default',
          "Make Gradient" = 'dpgradient',
          "Select Individual" = 'dppalette'
        )
      )
    }
  })
  ## Generating different theme types for data point fill ##
  dpfillVal <- reactive({
    if (input$dataGroup == T){
      if (input$dpfillG == 'defaultG') {
        dpcolors <- rep(c('#000000'), length(unique(orderdata()$groups)))
      } else if (input$dpfillG == 'borderG') {
        dpcolors <- as.list(bordercolor())
      }
    } else {
      dpcolors <- rep(c('#000000'), length(data()))
      if (input$dpfill == 'default') {
        dpcolors <- rep(c('#000000'), length(colCount()))
      } else if (input$dpfill == 'box') {
        dpcolors <- paletteTheme()
      } else if (input$dpfill == 'border') {
        dpcolors <-  bordercolor()
      } else if (input$dpfill == 'dpgradient') {
        dpcolors <- colorRampPalette(c(input$dpgradF1, input$dpgradF2))(length(colCount()))
      } else if (input$dpfill == 'dppalette') {
        dpcolors <- lapply(seq_along(colCount()), function(i) {
          input[[paste("fillsdp", i, sep = '_')]]}
        )
      }
    }
    #Passes through grayscale filter to check contrast
    if (input$grayscale == "Yes") {
      theme <- ColToGray(dpcolors)
    } else{
      theme <- dpcolors
    }
    return(theme)
  })
  
  ## Individual palette theme generator processing
  #Creating reactive values to store selected palette colors
  palTheme <- reactiveValues(palette = list()) #Ungrouped Shape colors 
  palThemeG <- reactiveValues(palette = list()) #Grouped Shape colors
  dpPalTheme <- reactiveValues(palette = list()) #Datapoint outline colors
  dpPalFillTheme <- reactiveValues(palette = list()) #Datapoint fill colors
  
  #Choosing grouping parameter levels for grouped data to set factor
  colLevelG <- reactive({
    req(any(
      obsBtn$demoVal,
      obsBtn$submitVal,
      obsBtn$pasteVal
    ))
    checkColon <- has_element(str_detect(colnames(data()),':'),FALSE)
    if (isFALSE(checkColon)){
      temp <- str_split_fixed(colnames(data()), ':', 2)
    } else {
      show_alert(
        title = "Invalid Column Header",
        text = "Column header missing ':'",
        type = 'error')
      return(NULL)
    }
    row.names(temp) <- NULL
    if(isTRUE(input$grpSwitch)){
      colnames(temp) <- c('Conditions','Groups')
      colFact <- unique(temp[,1])
      fillFact <- unique(temp[,2])
    } else { 
      colnames(temp) <- c('Groups','Conditions')
      fillFact <- unique(temp[,1])
      colFact <- unique(temp[,2])
    }
    return(fillFact)
  })
  #Processing number of color inputs to show for setting individual colors for shapes
  coltabs <- reactive({
    req(colCount())
    if(isTRUE(input$dataGroup)){
      coltabname <- unique(factor(orderdata()$groups, levels = colLevelG()))
    } else {
      coltabname <- colnames(data())
    }
    
    div(
      class = "truncated_title",
      lapply(seq_along(colCount()), function(i) {
        #sets initial values for color picker from stored reactive palette
        if(isTRUE(input$dataGroup)){
          id <- paste("colorsG", i, sep = '_')
          initial_val <- isolate(palThemeG$palette[[id]])
        }else{
          id <- paste("colors", i, sep = '_')
          initial_val <- isolate(palTheme$palette[[id]])
        }
        #Sets a default initial value if there is no stored reactive palette 
        if (is.null(initial_val)) initial_val <- "#CCCCCC"
        
        div(colorPickr(
          id,
          label = as.character(coltabname[i]),
          selected = initial_val,
          pickr_width = '20%'
        ), style = "width:100px")
      }), style = "display:inline-flex; flex-wrap:wrap !important; gap:5px;")
  })
  #Renders colour input buttons
  output$coltabsOut <- renderUI({
    req(isFALSE(input$dataGroup))#important
    coltabs()
  })
  output$coltabsOutG <- renderUI({
    req(isTRUE(input$dataGroup))#important
    coltabs()
  })
  #Reads user input individual colors and stores for ggplot
  paletteTheme <- reactive({
    req(colCount())
    if(isFALSE(input$dataGroup)){
      # for ungrouped data
      ids <- paste("colors", seq_along(colCount()), sep = '_')
      if (input$choosetheme == 'palette') {
        cols <- lapply(ids, function(x) {
          if (!is.null(input[[x]])) {
            #Reads directly from the color input
            return(input[[x]])
          } 
          else if (!is.null(palTheme$palette[[x]])) {
            #Reads from stored reactive palettes 
            return(palTheme$palette[[x]])
          } 
          else {
            #default
            return('#CCCCCC')
          }
        })
        if(input$grayscale == "Yes"){cols <- ColToGray(cols)}
        return(as.character(unlist(cols)))
      }else {
        #If no individual color is chosen
        return(as.character(addTheme()))
      }
    } else{
      #For grouped data
      ids <- paste("colorsG", seq_along(colCount()), sep = '_')
      if (input$choosethemeII == 'paletteG') {
        cols <- lapply(ids, function(x) {
          if (!is.null(input[[x]])) {
            #Reads directly from the color input
            return(input[[x]])
          } 
          else if (!is.null(palThemeG$palette[[x]])) {
            #Reads from stored reactive palettes
            return(palThemeG$palette[[x]])
          } 
          else {
            #Default
            return('#CCCCCC')
          }
        })
        if(input$grayscale == "Yes"){cols <- ColToGray(cols)}
        return(as.character(unlist(cols)))
      } else {
        #If no individual color is chosen
        return(as.character(addTheme()))
      }
    }
  })
  # observeEvent(input$exampleFile,{
  #   req(data())
  #   req(demoSettFile())
  # })
  observe({
    #To observe any changes in new palette color inputs and update accordingly
    #when previous setting file is uploaded 
    if(isTRUE(input$dataGroup)){
      req(input$choosethemeII == 'paletteG')
      ids <- paste("colorsG", seq_along(colCount()), sep = '_')
    }else{
      req(input$choosetheme == 'palette')
      ids <- paste("colors", seq_along(colCount()), sep = '_')
    }
    
    for (id in ids) {
      curr_input <- input[[id]]
      #Failsafe if no current input is present
      if (is.null(curr_input)) next
      
      if (isTRUE(input$dataGroup)) {
        if (!identical(palThemeG$palette[[id]], curr_input)) {
          #Resets reactive palette storage to new input color
          palThemeG$palette[[id]] <- curr_input
        } else {
          #Keeps reactive palette storage if there is no new input color
          palThemeG$palette[[id]] <- palThemeG$palette[[id]]
        }
      } else {
        if (!identical(palTheme$palette[[id]], curr_input)) {
          palTheme$palette[[id]] <- curr_input
        } else {
          palTheme$palette[[id]] <- palTheme$palette[[id]]
        }
      }
    }
  })
  
  #Processing number of color inputs to show for setting individual colors for datapoint outline
  dpcoltabs <- reactive({
    req(colCount())
    if(isTRUE(input$dataGroup)){
      coltabname <- unique(factor(orderdata()$groups,levels = colLevelG()))
    } else{
      coltabname <- colnames(data())
    }
    
    div(
      class = "truncated_title",
      lapply(seq_along(colCount()), function(i) {
        id <- paste("colorsdp",i, sep = '_')
        initial_val <- isolate(dpPalTheme$palette[[id]])
        if (is.null(initial_val)) initial_val <- '#CCCCCC'
        
        div(colorPickr(
          inputId = id,
          label = as.character(coltabname[i]),
          selected = initial_val,
          pickr_width = '20%'
        ), style = "width:100px;")
      }),style="display:inline-flex; flex-wrap:wrap !important; gap:5px;")
  })
  
  dpPaletteTheme <- reactive({
    req(colCount())
    
    ids <- paste("colorsdp", seq_along(colCount()), sep = '_')
    
    if (input$dpcolor == 'dppalette') {
      cols <- lapply(ids, function(x) {
        if (!is.null(dpPalTheme$palette[[x]])) {
          return(dpPalTheme$palette[[x]])
        } 
        else if (!is.null(input[[x]])) {
          return(input[[x]])
        } 
        else {
          return('#CCCCCC')
        }
      })
      if(input$grayscale == "Yes"){cols <- ColToGray(cols)}
      return(as.character(unlist(cols)))
    } else {
      return(as.character(dpcolors()))
    }
  })
  observe({
    ## To observe any changes in new palette color inputs and update accordingly
    req(input$dpcolor == 'dppalette')
    ids <- paste("colorsdp", seq_along(colCount()), sep = '_')
    
    for (id in ids) {
      curr_input <- input[[id]]
      if (is.null(curr_input)) next
      
      if (!identical(dpPalTheme$palette[[id]], curr_input)) {
        dpPalTheme$palette[[id]] <- curr_input
      } else{
        dpPalTheme$palette[[id]] <- dpPalTheme$palette[[id]]
      }
    }
    
  })
  output$dpcoltabsOut <- renderUI({
    dpcoltabs()
  })
  
  #Processing number of color inputs to show for setting individual colors for datapoint fill
  dpfilltabs <- reactive({
    req(colCount())
    if(isTRUE(input$dataGroup)){
      coltabname <- unique(factor(orderdata()$groups,levels = colLevelG()))
    } else{
      coltabname <- colnames(data())
    }
    div(
      class = "truncated_title",
      lapply(seq_along(colCount()), function(i) {
        id <- paste("fillsdp",i, sep = '_')
        initial_val <- isolate(dpPalFillTheme$palette[[id]])
        if (is.null(initial_val)) initial_val <- '#CCCCCC'
        
        div(colorPickr(
          inputId = id,
          label = as.character(coltabname[i]),
          selected = initial_val,
          pickr_width = '20%'
        ), style = "width:100px;")
      }),style="display:inline-flex; flex-wrap:wrap !important; gap:5px;")
  })
  
  dpPaletteFillTheme <- reactive({
    req(colCount())
    
    ids <- paste("fillsdp", seq_along(colCount()), sep = '_')
    
    if (input$dpfill == 'dppalette') {
      cols <- lapply(ids, function(x) {
        if (!is.null(dpPalFillTheme$palette[[x]])) {
          return(dpPalFillTheme$palette[[x]])
        } 
        else if (!is.null(input[[x]])) {
          return(input[[x]])
        } 
        else {
          return('#CCCCCC')
        }
      })
      if(input$grayscale == "Yes"){cols <- ColToGray(cols)}
      return(as.character(unlist(cols)))
    } else {
      return(as.character(dpfillVal()))
    }
  })
  observe({
    ## To observe any changes in new palette color inputs and update accordingly
    req(input$dpfill == 'dppalette')
    ids <- paste("fillsdp", seq_along(colCount()), sep = '_')
    
    for (id in ids) {
      curr_input <- input[[id]]
      
      if (is.null(curr_input)) next
      
      if (!identical(dpPalFillTheme$palette[[id]], curr_input)) {
        dpPalFillTheme$palette[[id]] <- curr_input
      } else{
        dpPalFillTheme$palette[[id]] <- dpPalFillTheme$palette[[id]]
      }
    }
  })
  output$dpfilltabsOut <- renderUI({
    dpfilltabs()
  })
  
  
  ## Setting theme for boxes in Violin plot type ##
  boxVioTheme <-reactive({
    if (input$boxColVio == 'Shape'){
      #same as the violin shape
      paletteTheme() 
    }else{
      #user select
      if (isTRUE(input$dataGroup)){
        rep(input$boxColCust,length(colCount()))
      }else {
        rep(input$boxColCust,ncol(data()))
      }
    }
  })
  
  ## Setting theme/ shade value for shape-border ##
  bordercolor <- reactive({
    if(isTRUE(input$dataGroup)){
      if (input$boxbordercolG == 'light') {
        boxborder <- lighten(paletteTheme(), (input$shadevalueG/100))
      } else {
        boxborder <- darken(paletteTheme(), (input$shadevalueG/100))
      }
    } else{
      if (input$boxbordercol == 'light') {
        boxborder <- lighten(paletteTheme(), (input$shadevalue/100))
      } else {
        boxborder <- darken(paletteTheme(), (input$shadevalue/100))
      }
    }
    return(boxborder)
  })
  
  ## Shape Opacity ##
  shapeOpacity <- reactive({
    if(isTRUE(input$dataGroup)){
      opacity <- input$shapeAlphaG/100
    }else {
      opacity <- input$shapeAlpha/100
    }
    return(safe_as_numeric(opacity))
  })
  
  ## Font-family
  fontfamily <- reactive({
    # Load the fonts
    loadfonts(device = 'win', quiet = TRUE)
    input$font
  })
  
  ## Processing the gap for raincloud plot
  
  gap <- reactive({
    if (input$slabSide == 'right'){
      return(0-(input$slabDistance/300))
    }else{
      return(1+(input$slabDistance/300))
    }
  })
  
  ## Summary Statistic layer for stat bars for Jitter and Raincloud plot type##
  summary_layers <- reactive({
    current_sum_val <- if (input$askPlotTypeII == 'jitter') {
      input$sum_typeJitter
    } else if (input$askPlotTypeII == 'viopoint') {
      input$sum_typeRain
    } else if (input$askPlotTypeII == 'bar') {
      if (input$barFunc == 'mean'){
        input$sum_typeBarMean
      }else {
        input$sum_typeBarMedian
      }
    }
    statLine <- if (input$askPlotTypeII == 'jitter') {
      input$statLine
    } else if (input$askPlotTypeII == 'viopoint') {
      input$statLineRain
    }
    
    statWidth <- if (input$askPlotTypeII == 'jitter') {
      input$statWidth
    } else if (input$askPlotTypeII == 'viopoint') {
      input$statWidthRain
    }
    
    statColor <- if (input$askPlotTypeII == 'jitter') {
      input$statColour
    } else if (input$askPlotTypeII == 'viopoint') {
      input$statColourRain
    }
    #Making ggproto layers here to easily switch position later as chosen by users
    summary_layers <- switch(
      current_sum_val,
      "mean_only" = stat_summary(
        fun = mean, fun.min = mean, fun.max = mean,
        geom = "crossbar", width = statWidth/100, color = statColor, linewidth = (statLine/80)),
      "mean_sd" = list(
        stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
                     geom = "errorbar", color = statColor, linewidth = (statLine/50), width = statWidth/150),
        stat_summary(fun = mean, fun.min = mean, fun.max = mean,
                     geom = "crossbar", width = statWidth/100, color = statColor, linewidth = (statLine/80))),
      
      "mean_sem" = list(
        stat_summary(fun.data = mean_se, geom = "errorbar",
                     color = statColor, linewidth = (statLine/50), width = statWidth/150),
        stat_summary(fun = mean, fun.min = mean, fun.max = mean,
                     geom = "crossbar", width = statWidth/100, color = statColor, linewidth = (statLine/80))),
      "median_only" = stat_summary(
        fun = median, fun.min = median, fun.max = median,
        geom = "crossbar", width = statWidth/100, color = statColor, linewidth = (statLine/80)),
      "median_ci" = list(
        stat_summary(fun.data = median_hilow, fun.args = list(conf.int = 0.95),
                     geom = "errorbar", color = statColor, linewidth = (statLine/50), width = statWidth/150),
        stat_summary(fun = median, fun.min = median, fun.max = median,
                     geom = "crossbar", width = statWidth/100, color = statColor, linewidth = (statLine/80)))                                       
    )
  })
  
  ## Summary Statistic layer for stat bars for Bar plot type##
  summary_layers_bar <- reactive({
    current_sum_val <- if (input$askPlotTypeII == 'bar') {
      if (input$barFunc == 'mean'){
        input$sum_typeBarMean
      }else {
        input$sum_typeBarMedian
      }
    }
    
    statLine <- input$statLineBar
    statWidth <- input$statWidthBar
    statColor <- input$statColourBar
    #Making ggproto layers here to easily switch position later as chosen by users
    summary_layers <- switch(
      current_sum_val,
      "mean_only" = stat_summary(
        fun = mean, fun.min = mean, fun.max = mean,
        geom = "crossbar", width = 0, color = statColor, linewidth = (statLine/80)),
      "mean_sd" = list(
        stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
                     geom = "errorbar", color = statColor, linewidth = (statLine/50), width = statWidth/150)),
      
      "mean_sem" = list(
        stat_summary(fun.data = mean_se, geom = "errorbar",
                     color = statColor, linewidth = (statLine/50), width = statWidth/150)),
      "median_only" = stat_summary(
        fun = median, fun.min = median, fun.max = median,
        geom = "crossbar", width = 0, color = statColor, linewidth = (statLine/80)),
      "median_ci" = list(
        stat_summary(fun.data = median_hilow, fun.args = list(conf.int = 0.95),
                     geom = "errorbar", color = statColor, linewidth = (statLine/50), width = statWidth/150))                                       
    )
  })
  
  
  ### Main graph processing ###
  
  plotType <- reactive({
    if (input$dataGroup == T) {
      input$askPlotTypeIIG
    } else {
      input$askPlotTypeII
    }
  })
  ## Overriding legend properties for grouped plots
  grpLegend <- reactive({
    leg <- guides(
      fill = guide_legend(override.aes = list(alpha = 1, color=NA), title = paste('<b>',input$legTitle,'</b>')),
      shape = guide_legend(override.aes = list(alpha = 1), title =  paste('<b>',input$legTitle,'</b>')),
      color = guide_legend(override.aes = list(alpha = 1), title =  paste('<b>',input$legTitle,'</b>'))
    )
    return(leg)
  })
  ## Processing for inside legend position selector
  leg_pos <- reactiveVal(c(0.87,0.85))
  observeEvent(input$plot_click, {
    leg_pos(c(input$plot_click$x, input$plot_click$y))
  })
  # Renders a plot box to choose XY position for placing legend for grouped plot
  output$legPosPlot <- renderPlot({
    ggplot(data.frame(x = 0:1, y = 0:1), aes(x, y)) +
      geom_blank() +
      xlim(0, 1) + ylim(0, 1) +
      theme_void() +
      theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
            panel.background = element_rect(fill = "#fcfcfc", color = NA)) +
      geom_point(aes(x = leg_pos()[1], y = leg_pos()[2]), 
                 color = "red", size = 6, shape = 3) + 
      annotate("text", x = 0.5, y = 0.05, label = "Click to place legend", size = 5)
  })
  
  ## Processing Connecting lines for grouped plots
  lineData <- reactive({
    req(data())
    if (isTRUE(input$dataGroup)){
      checkColon <- has_element(str_detect(colnames(data()),':'),FALSE)
      
      if (isFALSE(checkColon)){
        temp <- str_split_fixed(colnames(data()), ':', 2)
      } else {
        show_alert(
          title = "Invalid Column Header",
          text = 'Column header missing `:`.',
          type = 'error')
        return(NULL)
      }
      row.names(temp) <- NULL
      tempDf <- as.data.frame(apply(data(),MARGIN=2, FUN=median, na.rm=TRUE)) |>
        t() |> as.data.frame()
      if (isTRUE(input$grpSwitch)){
        name <- c('para','groups') 
        lev <- unique(temp[,2])
      } else {
        name <- c('groups','para')
        lev <- unique(temp[,1])
      }
      df <- tempDf |> pivot_longer(names_to = name,
                                   names_sep = ':',
                                   values_to = 'val',
                                   cols = everything())
      df$groups <- factor(df$groups, levels = lev)
      
    }else {
      if (input$askPlotTypeII == 'box' || input$askPlotTypeII == 'violin'){
        df <- as.data.frame(apply(data(),MARGIN=2, FUN=median, na.rm=TRUE))
      } else if (input$askPlotTypeII == 'bar'){
        if (input$barFunc == 'median'){
          df <- as.data.frame(apply(data(),MARGIN=2, FUN=median, na.rm=TRUE)) 
        } else {
          df <- as.data.frame(apply(data(),MARGIN=2, FUN=mean, na.rm=TRUE))
        }
      } else if(input$askPlotTypeII == 'jitter') {
        if (isTRUE(str_detect(input$sum_typeJitter,'median'))){
          df <- as.data.frame(apply(data(),MARGIN=2, FUN=median, na.rm=TRUE))
        } else {
          df <- as.data.frame(apply(data(),MARGIN=2, FUN=mean, na.rm=TRUE))
        }
      } else {
        if (isTRUE(str_detect(input$sum_typeRain,'median'))){
          df <- as.data.frame(apply(data(),MARGIN=2, FUN=median, na.rm=TRUE))
        } else {
          df <- as.data.frame(apply(data(),MARGIN=2, FUN=mean, na.rm=TRUE))
        }
      }
      colnames(df) <- 'val'
      df$para <- row.names(df)
      rownames(df) <- NULL
    }
    return(df)
  })
  
  ## Temporary geom_text condition
  descText <- reactive({
    is_bold <- "bold" %in% input$dpviewMD
    is_ital <- "italics" %in% input$dpviewMD
    
    open  <- paste0(if(is_bold) "<b>" else "", if(is_ital) "<i>" else "")
    close <- paste0(if(is_ital) "</i>" else "", if(is_bold) "</b>" else "")
    
    geom_richtext(addDataPointLabel(), mapping = aes (x = x, y = y,
                                                      label = paste(open,text,close),
                                                      family = fontfamily(),
                                                      hjust = 0.5),
                  fill = '#FFFFFF00',
                  size = input$dpviewSize,
                  label.colour = NA) 
    
  })
  
  ### Main ggplot input code ###
  plotinput <- reactive({
    x <- orderdata()
    
    if (isTRUE(input$dataGroup)) {
      checkColon <- has_element(str_detect(colnames(data()),':'),FALSE)
      if (isFALSE(checkColon)){
        temp <- str_split_fixed(colnames(data()), ':', 2)
      } else {
        show_alert(
          title = "Invalid Column Header",
          text = 'Column header missing `:`.',
          type = 'error')
        return(NULL)
      }
      row.names(temp) <- NULL
      if(isTRUE(input$grpSwitch)){
        colnames(temp) <- c('Conditions','Groups')
        colFact <- unique(temp[,1])
        fillFact <- unique(temp[,2])
      } else { 
        colnames(temp) <- c('Groups','Conditions')
        fillFact <- unique(temp[,1])
        colFact <- unique(temp[,2])
      }
      x_axis_col <- gsub('[.]', ' ', colFact)
      x_axis <- c(factor(x$variable, levels = colFact, labels = x_axis_col))
      fillPara <- factor(x$groups, levels = fillFact)
      colPara <- factor(x$groups, levels = fillFact)
      legendPos <- input$legPos
    } else {
      x_axis_col <- gsub('[.]', ' ', colnames(data()))
      x_axis <- factor(x$variable, levels = colnames(data()), labels = x_axis_col)
      fillPara <- x_axis
      colPara <- x$variable
      legendPos <- 'none'
    }
    
    g <- ggplot(x, aes(x = x_axis, y = value))
    
    ### For Violin Plot ###
    if (plotType() =='violin'){
      
      if (input$viotype == 'Violin-Box'){
        p <-  g+
          geom_violin(
            mapping = aes(fill = fillPara, color = fillPara),
            position = position_dodge(width = input$innerDistVio/100),
            show.legend = F,
            lwd = input$linewidthVio/70,
            trim = input$endTrim, alpha=(shapeOpacity()),
            scale = "count")+
          scale_fill_manual(values = paletteTheme())+
          new_scale_fill()+
          geom_boxplot(
            width = input$boxWidthVio/150, size = input$linewidthVio/70,
            position = position_dodge(width = input$innerDistVio/100),
            mapping=aes(fill = fillPara,
                        color= fillPara),
            outlier.shape = NA,
            show.legend = F)+
          scale_color_manual(values = bordercolor())+
          scale_fill_manual(values = boxVioTheme())+
          new_scale_fill()+
          new_scale_color()+
          ## Extra transparent geom_boxplot element to override legends from above two
          geom_violin(
            mapping = aes(fill = fillPara, color = fillPara),
            color = NA,
            alpha = 0,
            show.legend = NA)+
          scale_color_manual(values = bordercolor())+
          scale_fill_manual(values = paletteTheme())+
          grpLegend()
      } else {
        if (isTRUE(input$askQuantLine)){
          p <-  g+
            geom_violin(
              mapping = aes(fill = fillPara, color = fillPara),
              position = position_dodge(width = input$innerDistVio/100),
              show.legend = F,
              lwd = input$linewidthVio/70,
              trim = input$endTrim, alpha=(shapeOpacity()),
              scale = "count")+
            geom_violin(
              mapping = aes(fill = fillPara, color = fillPara),
              fill = 'transparent',
              show.legend = F,
              position = position_dodge(width = input$innerDistVio/100),
              draw_quantiles = c(0.25,0.75),
              quantile.linetype = 'dashed',
              quantile.linewidth = input$quantLineSize/50,
              trim = input$endTrim,
              lwd = 0
            )+
            geom_violin(
              mapping = aes(fill = fillPara, color = fillPara),
              fill = 'transparent',
              show.legend = F,
              position = position_dodge(width = input$innerDistVio/100),
              draw_quantiles = c(0.5),
              quantile.linewidth = input$quantLineSize/40,
              trim = input$endTrim,
              lwd = 0
            )+
            scale_color_manual(values = bordercolor())+
            scale_fill_manual(values = paletteTheme())+
            new_scale_fill()+
            new_scale_color()+
            geom_violin(
              mapping = aes(fill = fillPara, color = fillPara),
              color = NA,
              alpha = 0,
              show.legend = NA)+
            scale_color_manual(values = bordercolor())+
            scale_fill_manual(values = paletteTheme())+
            grpLegend()
        } else {
          p <-  g+
            geom_violin(
              mapping = aes(fill = fillPara, color = fillPara),
              position = position_dodge(width = input$innerDistVio/100),
              show.legend = F,
              lwd = input$linewidthVio/70,
              trim = input$endTrim, alpha=(shapeOpacity()),
              scale = "count")+
            scale_color_manual(values = bordercolor())+
            scale_fill_manual(values = paletteTheme())+
            new_scale_fill()+
            new_scale_color()+
            geom_violin(
              mapping = aes(fill = fillPara, color = fillPara),
              color = NA,
              alpha = 0,
              show.legend = NA)+
            scale_color_manual(values = bordercolor())+
            scale_fill_manual(values = paletteTheme())+
            grpLegend()
        }
      }
      
    } else if (plotType() =='box'){
      ### For Box-Whisker Plot ###
      
      if (input$boxtype == "boxpoint"){
        p <-  g+
          geom_boxplot(
            mapping = aes(fill = fillPara, color= fillPara),
            position = position_dodge(width = input$innerDistBox/100),
            width = input$boxwidth/100, alpha=(shapeOpacity()),
            lwd = input$linewidthBox/40, fatten = 2,
            show.legend = F,
            notch = input$notch,
            outliers = input$outlier,
            outlier.shape = safe_as_numeric(8),
            outlier.colour = 'red',
            outlier.size=3,
            staplewidth = 0.3)+
          scale_color_manual(values = bordercolor())+
          scale_fill_manual(values = paletteTheme())+
          new_scale_color()+
          new_scale_fill()+
          geom_beeswarm(
            mapping = aes(shape = fillPara, fill= fillPara, color = fillPara),
            dodge.width = input$innerDistBox/100,
            show.legend = FALSE,
            size = addPointBox(),
            cex = addScatterBox(),
            alpha = input$pointAlpha/100,
            method = input$pointMethodBox,
            corral = 'wrap'
            # shape = safe_as_numeric(input$pointshape)
          )+
          scale_color_manual(values = unlist(dpPaletteTheme()))+
          scale_fill_manual(values = unlist(dpPaletteFillTheme()))+
          scale_shape_manual(values=safe_as_numeric(dpshapeBox()))+
          new_scale_color()+
          new_scale_fill()+
          ## Extra transparent geom_boxplot element to override legends from above two
          geom_boxplot(
            mapping = aes(fill = fillPara, color= fillPara),
            color=NA,
            alpha = 0,
            show.legend = NA
          )+
          scale_color_manual(values = bordercolor())+
          scale_fill_manual(values = paletteTheme())+
          grpLegend()
      } else if (input$boxtype == "boxOnly"){
        p <-  g+
          geom_boxplot(
            mapping = aes(fill = fillPara, color= fillPara),
            position = position_dodge(width = input$innerDistBox/100),
            width = input$boxwidth/100, alpha=(shapeOpacity()),
            lwd = input$linewidthBox/40, fatten = 2,
            show.legend = NA,
            notch = input$notch,
            outliers = input$outlier,
            outlier.shape = safe_as_numeric(8),
            outlier.colour = 'red',
            outlier.size=3,
            staplewidth = 0.3)+
          scale_color_manual(values = bordercolor())+
          scale_fill_manual(values = paletteTheme())+
          grpLegend()
      }
    } else if (plotType() =='jitter'){
      ### For Jitter Plot ###
      
      jL <- list(
        geom_beeswarm(
          mapping = aes(shape = fillPara,
                        fill= fillPara,
                        color = fillPara
          ),
          # dodge.width = input$innerDistBox/100,
          size = input$pointsize/30,
          cex = input$scatter/25,
          alpha = input$pointAlpha/100,
          method = input$pointMethod,
          corral = 'wrap'
        ),
        scale_color_manual(values = unlist(dpPaletteTheme())),
        scale_fill_manual(values = unlist(dpPaletteFillTheme())),
        scale_shape_manual(values=safe_as_numeric(dpshape()))
      )
      if (input$askSummPos == 'top'){
        p <-  g+ jL+
          summary_layers()
      } else {
        p <-  g+
          summary_layers()+ jL
      }
    } else if (plotType() == 'viopoint'){
      ### For Raincloud Plot ###
      
      vpL1 <- list(
        stat_halfeye(
          mapping = aes(fill = fillPara, slab_color = fillPara),
          side=input$slabSide,
          point_colour = NA,
          alpha = (shapeOpacity()),
          justification = gap(),
          .width = 0,
          width = 0.6,
          trim = input$endTrimRain,
          slab_linewidth = input$linewidthRain/40,
          linetype= 'solid'
        ),
        scale_fill_manual(values = paletteTheme()),
        scale_color_manual(values = bordercolor(),
                           aesthetics = 'slab_color')
      )
      vpL2 <- list(
        new_scale_fill(),
        new_scale_color(),
        geom_beeswarm(
          aes(shape=fillPara, fill = fillPara, color=fillPara),
          size = (input$pointsizeRain/30),
          cex = (input$scatterRain/50),
          alpha = (input$pointAlpha/100),
          method = input$pointMethod,
          corral = 'wrap'),
        scale_shape_manual(values=safe_as_numeric(dpshapeRain())),
        scale_fill_manual(values = unlist(dpPaletteFillTheme())),
        scale_color_manual(values=unlist(dpPaletteTheme()))
      )
      if (input$askSummPosRain == 'top'){
        p <-  g+ vpL1 + vpL2 +
          summary_layers()
      } else {
        p <-  g+ vpL1 +
          summary_layers() + vpL2
      }
      
    } else if (plotType() == 'bar'){
      ### For Bar Plot ###
      
      if (input$askSide == 'Bothside'){
        q <-  g+
          stat_summary(
            fun = input$barFunc,
            geom = "col",
            mapping = aes(fill = fillPara),
            color = bordercolor(),
            width = input$barwidth / 100, 
            alpha = (input$shapeAlpha / 100),
            linewidth = input$linewidthBar / 40
          )+
          summary_layers_bar()+
          scale_fill_manual(values = paletteTheme())
      } else{
        q <-  g+summary_layers_bar()+
          stat_summary(
            fun = input$barFunc,
            geom = "col",
            fill = input$plotColor,
            color = input$plotColor,
            width = 0.9, 
            # alpha = (input$shapeAlpha / 100),
            linewidth = input$linewidthBar / 40
          )+
          stat_summary(
            fun = input$barFunc,
            geom = "col",
            mapping = aes(fill = fillPara),
            color = bordercolor(),
            width = input$barwidth / 100,
            alpha = (input$shapeAlpha / 100),
            linewidth = input$linewidthBar / 40
          )+
          scale_fill_manual(values = paletteTheme())}
      if (isTRUE(input$askJitter)){
        p <- q+ new_scale_fill()+
          geom_beeswarm(
            aes(fill = fillPara, color = fillPara, shape = fillPara),
            size = (input$pointsize/30),
            cex = (input$scatter/25),
            method = input$pointMethod,
            # shape = safe_as_numeric(input$pointshape),
          )+
          scale_color_manual(values = unlist(dpPaletteTheme()))+
          scale_fill_manual(values = unlist(dpPaletteFillTheme()))+
          scale_shape_manual(values=safe_as_numeric(dpshape()))
      } else {p <- q}
    }
    
    #Other common ggproto objects
    pN <- p +
      theme_classic() +
      labs(
        y = input$aytitle,
        x = input$axtitle) +
      ggtitle(label = input$plotTitle) +
      Ycontax() +
      theme(
        plot.title = element_textbox_simple(size = input$plotFont/1.5,
                                            family = fontfamily(),
                                            fill = "#FFFFFF00",
                                            width = unit(input$bWidthTitle*1.2, "mm"),
                                            box.colour = '#000000',
                                            linewidth = safe_as_numeric(input$lineTitle/50),
                                            linetype = "solid",
                                            hjust = safe_as_numeric(titlePlot()),
                                            halign = safe_as_numeric(titlePlot()),
                                            padding = unit(input$padTitle/5,'pt'),
                                            margin = margin(t = safe_as_numeric(input$verAlign/2)*-1,
                                                            b = safe_as_numeric(input$verAlign/2),
                                                            unit = "pt") 
        ),
        plot.title.position = "panel",
        axis.text.x = element_markdown(size = input$Xfontcol, color = "black",
                                       angle = safe_as_numeric(input$Xrotate),
                                       hjust = xcolPosH(),
                                       halign = xcolPosH(),
                                       valign = xcolPosV(),
                                       vjust = xcolPosV(), lineheight = 1),
        axis.title.x = element_textbox(size = input$Xfontsz, color = "black",
                                       width = unit(input$Xlinebreak*5,"pt"),
                                       hjust = 0.5,
                                       halign = 0.5,
                                       padding = margin(5,0,0,0)),
        axis.text.y = element_markdown(size = input$Yfontcol, color = "black"),
        axis.title.y = element_textbox(size = input$Yfontsz, color = "black",
                                       width = unit(input$Ylinebreak*5, "pt"),
                                       orientation = "left-rotated",
                                       hjust = 0.5,
                                       halign = 0.5,
                                       box.color = 'black'),
        axis.ticks = element_line(size = input$tickwidth/40),
        axis.ticks.length = unit(input$ticklength/80,'cm'),
        legend.position = legendPos,
        legend.text = element_markdown(size = input$legTextSize),
        legend.title = element_markdown(size = input$legTitleSize),
        legend.key.height = unit(input$legSize/100, 'cm'),
        legend.key.width = unit(input$legSize/100, 'cm'),
        legend.position.inside = leg_pos(),
        legend.background = element_rect(linewidth = input$legBorderSize/100, colour = 'black'),
        axis.line = element_line(linewidth = input$axisline/40),
        text = element_text(family = fontfamily()),
        panel.grid.major.x  = majGridInpX(),
        panel.grid.minor.x = minGridInpX(),
        panel.grid.major.y  = majGridInpY(),
        panel.grid.minor.y = minGridInpY(),
        panel.background = element_rect(fill=input$plotColor),
        plot.margin = margin(t = plotTopM(), r = plotRightM(), l = 10, b = 30, unit = "pt"),
        panel.border = plotBorder())+
      descText()+ #Geom_text for desc info view
      #scale_y_continuous logic
      coord_cartesian(ylim = c(ifelse(is.na(input$minY), 0,yaxisMin()), yaxisMax()), clip = 'off')  
    
    # For adding connecting line for repeated measured data
    if (isTRUE(input$askConnectLine) && c(isTRUE(input$askPaired) || 
                                          isTRUE(input$askPairedssT))){
      if (isTRUE(input$dataGroup)){
        pS <- pN + geom_line(data = lineData(),
                             mapping = aes(x = para, y = val, group = groups, color = groups),
                             # color = order(rep(paletteTheme(),length(unique(lineData()$para)))),
                             position = position_dodge(
                               width = ifelse(input$askPlotTypeIIG == 'violin',
                                              input$innerDistVio/100,
                                              input$innerDistBox/100)),
                             linewidth = input$connectLineSize/90,
                             linetype = input$connectLineType)+
          scale_color_manual(values = paletteTheme())
      } else{
        pS <-  pN + geom_line(data = lineData(),
                              mapping = aes(x = para, y = val, group = 1),
                              color = input$connectLineCol,
                              linewidth = input$connectLineSize/90,
                              linetype = input$connectLineType)
      }
      
    } else {
      pS <- pN 
    }
    
    # For adding significance annotation brackets
    if(isFALSE(btn_val())){
      pF <- pS 
    } else {
      pF <-  pS +
        geom_segment(segAdd(),
                     mapping =aes(x = x, xend = x, y = y,yend = yendL),
                     linewidth = input$bracWidth/67,
                     color = input$bracCol)+ 
        
        geom_segment(segAdd(),
                     mapping =aes(x = xend, xend = xend, y = yend, yend = yendR),
                     linewidth = input$bracWidth/67,
                     color = input$bracCol) +
        
        geom_richtext(segAdd(),mapping=aes(x=safe_as_numeric(xT), y=safe_as_numeric(yT),
                                           label=text, family = fontfamily(),
                                           hjust =  pvalHalign(),
                                           angle = ifelse(isTRUE(input$flipPlot), 270, 0),
                                           text.colour = input$pvalCol),
                      label.padding = unit(c(1),"pt"),
                      fill = ifelse(isTRUE(input$flipPlot),input$plotColor, 'white'),
                      # fill = '#FFFFFF00',
                      label.colour= NA, size = segAdd()$size, vjust = segAdd()$vjust)+
        
        geom_segment(segAdd(),
                     mapping=aes(x = x, xend = xend,y = y, yend = yend),
                     linewidth = input$bracWidth/67,
                     linejoin = 'mitre',
                     color = input$bracCol)
    }
    ## For Y-axis break (to be added later)
    # if (isTRUE(input$addYBreak)){
    #   req(input$minYBreak)
    #   req(input$maxYBreak)
    #   
    #   if (is.null(input$minYBreak) && is.null(input$maxYBreak)){
    #     showNotification("Define break points on Y-axis.", type =  'message')
    #   } else {
    #     pG <- pF + scale_y_break(
    #       breaks = c(input$minYBreak, input$maxYBreak),
    #       space = input$breakGap/100,
    #       scales = input$breakScale,
    #       expand = F
    #     )
    #   }
    # } else {
    #   pG <- pF
    # }
    # For flipping Y axis
    if (isTRUE(input$flipPlot)){
      pR <- pF + coord_flip(ylim = c(ifelse(is.na(input$minY), 0, yaxisMin()), yaxisMax()),
                            clip = 'off')
    } else {
      pR <- pF
    }
    # For reversing X axis
    if (isTRUE(input$reverseX)){
      pFinal <- pR + scale_x_discrete(limits = rev)
    } else {
      pFinal <- pR
    }
    return(pFinal)
    
  })
  
  ### Graph Output Processing ###
  
  output$graphFinal <- renderPlot({
    if (isTruthy(input$runAnalysisFinal) &&
        !isTruthy(input$exampleFile) && isTRUE(input$askComp)){
      validate(
        need(length(input$grplist) >= 1,
             "Please select at least 1 option to proceed.")
      )
      validate(
        need(isTRUE(stored_status()),
             "Data has changed. Please rerun the statistical analysis.")
      )
    }
    plotinput()
  })
  
  ## Processing graph download handler
  output$downloadBPlot <- downloadHandler(
    filename = function() {
      paste0("sensabled_plot", ".", as.character(input$selectFileType), sep='')
    },
    content = function(file) {
      ggsave(
        file,
        plot = plotinput(),
        # device = input$selectFileType,
        width = plotwidth() / 72,
        height = plotheight() / 72,
        units = "in",
        dpi = safe_as_numeric(input$selectDPI)
      )
    }
  )
  
  
  ## Save Plot As modal settings
  observeEvent(input$saveBtn,{
    showModal(modalDialog(
      title = "Save Plot Settings",
      div(
        # style = "display: flex; flex-direction: column; gap: 10px; width: 32%;",
        selectInput("selectFileType", "Save as", choices = c('PNG' = 'png', 'JPEG' = 'jpeg', 'TIFF' = 'tiff', 
                                                             'SVG' = 'svg')),
        radioGroupButtons("selectDPI", 'Select Resolution (DPI)',
                          choices = c(72, 96, 150, 300, 400, 600),
                          size = 'normal', selected = 150),
        
      ),
      easyClose = TRUE, size = 'm',
      footer = tagList(downloadButton("downloadBPlot", "Download Plot",
                                      icon = icon("download"), width = "100%"),
                       modalButton("Cancel"))
    ))
  })
  
  ## Upload setting button render
  observe(
    if(is.null(input$usesetting)){
      shinyjs::disable('reuseset')
    }else{
      shinyjs::enable('reuseset')
    }
  )
  
  ## Main Plot Display ##
  plotContent <- reactive({
    div(
      style = paste0("display:block; margin-bottom: 20px; background-color:", input$canvasTheme, "; width:100%; height:auto; padding:50px;"),
      uiOutput("FinalPlot")
    )
  })
  
  # Conditional content for Graph panel
  output$graph_main_content <- renderUI({
    
    if (!isTruthy(input$submitFile) && !isTruthy(input$pasteBtn) && !isTruthy(input$exampleFile)) {
      # No data uploaded or empty data
      div(
        style = "display: flex; justify-content: center; align-items: center;
          height: 60vh; flex-direction: column; text-align: center; color: #856404;
          background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 10px;
          padding: 30px; margin: 20px; z-index:100;",
        icon("exclamation-triangle", style = "font-size: 60px; margin-bottom: 20px; color: #f39c12;"),
        h3(style = "color: #d35400; margin: 0 0 10px 0;", "No data to plot"),
        p(style = "font-size: 18px; max-width: 600px;",
          "Please go to the ", strong("File Upload"), " tab,",
          "upload an Excel file, select columns, and click ", strong("Upload Datasheet"), " or Upload Pasted Data to begin.")
      )
    } else if (input$dataGroup == T) {
      col_names <- colnames(data())  
      checkColon <- has_element(str_detect(col_names,':'),FALSE)
      if (isTRUE(checkColon)) {
        div(
          style = "display: flex; justify-content: center; align-items: center;
                height: 60vh; flex-direction: column; text-align: center; color: #856404;
                background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 10px;
                padding: 30px; margin: 20px; z-index:100;",
          icon("exclamation-triangle", style = "font-size: 60px; margin-bottom: 20px; color: #f39c12;"),
          h3(style = "color: #d35400; margin: 0 0 10px 0;", "Incompatible Data Format"),
          p(style = "font-size: 18px; max-width: 600px;",
            "For ", strong("Grouped Data"), " mode, column headers must follow the format: ",
            strong("Group:Condition"), "(e.g., G1:A, G1:B, G1:C, G2:A, G2:B, G2:C)."),
          br(),
          p(style = "font-size: 14px; max-width: 600px;",
            "See example data for reference", br(),
            "Current headers: ", strong(paste(col_names, collapse = ", ")))
        )
      } else{
        # Data is available
        plotContent()
      } 
    } else {
      # Data is available
      plotContent()
    }
  })
  ## Set plot width 
  plotwidth <- reactive({
    width <- 200 #fallback
    if (is.na(input$width)){
      show_alert(
        title = "Invalid Value",
        text = "Plot width cannot be blank",
        type='error')
    } else if(input$width>800){
      width <- 800
      show_alert(
        title = "Invalid Value",
        text = "Plot width cannot be more than 800",
        type='error')
    }else if(input$width<200){
      width <- 200
      show_alert(
        title = "Invalid Value",
        text = "Plot width cannot be lesser than 200",
        type='error')
    }else{
      width <- input$width
    }
    
    return(width)
  })
  #Checks plot width does not exceeds set limits and resets
  observe({
    req(input$width)
    if(input$width>800){
      updateNumericInput(session, 'width', value = 800)
    }
    if(input$width<200){
      updateNumericInput(session, 'width', value = 200)
    }
  })
  ## Set plot height
  plotheight <- reactive({
    height <- 200 
    if (is.na(input$height)){
      show_alert(
        title = "Invalid Value",
        text = "Plot height cannot be blank",
        type='error')
    } else if(input$height>600){
      height <- 600
      show_alert(
        title = "Invalid Value",
        text = "Plot height cannot be more than 600",
        type='error')
    }else if(input$height<200){
      height <- 200
      show_alert(
        title = "Invalid Value",
        text = "Plot height cannot be lesser than 200",
        type='error')
    }else{
      height <- input$height
    }
    
    return(height)
  })
  #Checks plot height does not exceeds set limits and resets
  observe({
    req(input$height)
    if(input$height>600){
      updateNumericInput(session, 'height', value = 600)
    }
    if(input$height<200){
      updateNumericInput(session, 'height', value = 200)
    }
  })
  ## Resets plot dimension to default when reset button is clicked
  observeEvent(input$resetSize,{
    tagList(
      updateNumericInput(session,'width',
                         min = 100, max = 800, value = 500),
      updateNumericInput(session,'height',
                         min = 100, max = 800, value = 400)
    )
  })
  
  ## Aspect ratio calculation and updation
  currentRatio <- reactiveVal(500 / 400)
  observeEvent(input$lockRatio, {
    if (isTRUE(input$lockRatio)) {
      req(input$height > 0)
      currentRatio(input$width / input$height)
    }
  })
  
  observeEvent(input$width, {
    if (isTRUE(input$lockRatio)) {
      newHeight <- round(input$width / currentRatio())
      if (newHeight != input$height) {
        updateNumericInput(session, "height", value = newHeight)
      }
    }
  })
  
  observeEvent(input$height, {
    if (isTRUE(input$lockRatio)) {
      newWidth <- round(input$height * currentRatio())
      if (newWidth != input$width) {
        updateNumericInput(session, "width", value = newWidth)
      }
    }
  })
  ## Plot zoom in
  observeEvent(input$zoomIn, {
    updatePrettyToggle(session, 'lockRatio', value = TRUE)
    newWidth <- round(input$width + 50)
    if (newWidth != input$width) {
      shinyjs::delay(200,
                     updateNumericInput(session, "width", value = newWidth))
    }
  })
  ## Plot zoom out
  observeEvent(input$zoomOut, {
    updatePrettyToggle(session, 'lockRatio', value = TRUE)
    newWidth <- round(input$width - 50)
    if (newWidth != input$width) {
      shinyjs::delay(200,
                     updateNumericInput(session, "width", value = newWidth))
    }
  })
  
  #Plot width adjust when demo value is loaded
  observeEvent(input$exampleFile,{
    if (!is.null(demoFile()) && isTRUE(input$dataGroup)){
      updateNumericInput(session, "width", value = 750)
    }
  })
  
  ## Final plot render options
  output$FinalPlot <- renderUI({
    withWaiter(
      plotOutput(
        "graphFinal",
        width = plotwidth(),
        height = plotheight()
      ),
      html = spin_loaders(id = 3, color = "#DFDFDF", style = NULL),
      color = "#FCFFFC"
    )
  })
  #A warning when user tries to trim violin shape ends
  observe({
    if (isFALSE(input$endTrim)){
      shinyWidgets::show_toast(
        title = "Warning",
        text = "Extended ends could be deceptive!", type = 'warning')
    }
  }) 
  observe({
    if (isFALSE(input$endTrimRain)){
      shinyWidgets::show_toast(
        title = "Warning",
        text = "Extended ends could be deceptive!", type = 'warning')
    }
  }) 
  
  ##################
  ### Statistics ###
  ##################
  
  ### Settings for Statistical Analysis tests to show on side tab ###
  
  # Choice names for sample test type
  sampChoice <- reactive({
    #Check icon to show suggested test
    suggestIcon <- icon("circle-check", class = "text-warning")
    if (ncol(data()) == 2) {
      #When there are only two samples
      list(tags$span(
        `data-toggle` = "tooltip",
        `data-placement` = "right",
        title = "Suggested Test",
        "Two-sample test", suggestIcon
      ),
      "Several samples test")
    } else if (ncol(data()) > 2) {
      #When there are more than two samples
      list("Two-sample test",
           tags$span(
             `data-toggle` = "tooltip",
             `data-placement` = "right",
             title = "Suggested Test",
             "Several samples test", suggestIcon
           ))
    } else {
      #When there are only one sample (not possible)
      list("Two-sample test", "Severak samples test")
    }
  })
  
  # Choice values for sample test types
  selcChoice <- reactive({
    #When there are only two samples, selects two-sample test
    if (ncol(data()) == 2) {"tSt"}
    #When there are more than two samples, selects several sample test
    else if (ncol(data()) > 2) {"sSt"} 
    #When there are only one sample, selects none
    else {character(0)}
  })
  
  # Choice names for parametric test types
  sampChoicePara <- reactive({
    #Check icon to show suggested test
    suggestIcon <- icon("circle-check", class = "text-warning")
    #Reads normality test report and checks for significant p value (<0.05)
    normP <- safe_as_numeric(normTest()$`P Value`)
    #Suggests parametric when no significant p value
    if (isFALSE(any(normP<0.05))) {
      list(tags$span(
        `data-toggle` = "tooltip",
        `data-placement` = "right",
        title = "Suggested Test",
        "Parametric", suggestIcon
      ),
      "Nonparametric")
    } else {
      #Suggests nonparametric when there is a significant p value
      list("Parametric",
           tags$span(
             `data-toggle` = "tooltip",
             `data-placement` = "right",
             title = "Suggested Test",
             "Nonparametric", suggestIcon
           ))
    }
  })
  #Choice values for parametric test types
  selcChoicePara <- reactive({
    #Selects parametric tests when there is no significant p value 
    #in normality test and vice versa
    normP <- safe_as_numeric(normTest()$`P Value`)
    if (isFALSE(any(normP<0.05))) "para" else "nonpara"
  })
  
  #Rendering inputs for selecting type of statistical tests to perform
  output$testInput <- renderUI({
    req(data()) #important
    tagList(
      #two-sample or several samples test list
      radioGroupButtons(
        "ttestType",
        "Select Test Type",
        choiceNames =  sampChoice(),
        choiceValues = c('tSt','sSt'),
        selected = selcChoice(),
        direction = "vertical",
        justified = TRUE,
        width = '100%'
      ),
      #parametric or nonparametric test list
      radioGroupButtons(
        'paratestType',
        "Select Test Type",
        choiceNames = sampChoicePara(),
        choiceValues = c('para','nonpara'),
        selected = selcChoicePara(),
        direction = 'vertical',
        justified = TRUE,
        width = '100%'
      ), 
      HTML('<p style = "color: grey; font-size:13px; margin-top:-10px;">&#x26A0;
      Checkmarked tests are recommended based on your data structure. 
           Please verify these assumptions before proceeding with your analysis &#x26A0;</p>'),
      conditionalPanel(
        condition = "input.ttestType == 'tSt'",
        #Asks whether sample is paired in two-sample test
        prettySwitch(
          'askPaired', 'Paired Samples',
          value = F, fill = T, status = 'success'
        )),
      conditionalPanel( 
        condition = "input.ttestType == 'sSt'",
        #Asks whether to perform post-hoc or multiple comparisons for several samples
        prettySwitch(
          'askComp', 'Perform Multiple Comparisons',
          fill = T, status = 'success', value = T
        ),
        #Asks whether the samples are repeated measures samples
        prettySwitch(
          'askPairedssT', 'Repeated-Measured Samples',
          value = F, fill = T, status = 'success'
        )
      )
    )
  })
  
  ## PostHoc Selection Modal ##
  
  observeEvent(input$runAnalysis, {
    showModal(modalDialog(
      title = "Post Hoc Analysis",
      tagList(
        conditionalPanel(
          condition = "input.askComp == true",
          # Post hoc comparisons for several sample test type
          conditionalPanel(
            condition = "input.ttestType == 'sSt'",
            # Comparison type for ungrouped data
            conditionalPanel(
              condition = "!input.dataGroup",
              pickerInput(
                inputId = 'compList',
                label = 'Select Group Comparison Type',
                choices = c(
                  'Compare against the control' = 'controlC',
                  'Compare against each other' = 'groupC'
                ),
                selected = 'groupC', width = '100%')
            ),
            ## Branch 1. Pairwise / all-vs-all comparisons (groupC) ##
            #For both grouped and ungrouped data
            conditionalPanel(
              condition = "input.compList == 'groupC'",
              #Users selects data columns for post hoc analysis   
              pickerInput(
                inputId = "statCols",
                label = "Select Groups",
                choices = colnames(data()),
                selected = colnames(data()),
                multiple = TRUE,
                width = "100%",
                options = list(
                  `min-options` = 3,
                  `min-options-text` = "Min 3 selections allowed!"
                )
              ),
              #Correction method for nonparametric, unpaired, non-grouped data
              conditionalPanel(
                condition = "input.paratestType == 'nonpara' &&
                !input.askPairedssT && !input.dataGroup",
                pickerInput(
                  inputId = 'askCorrection',
                  label = "P Value Correction Method",
                  choices = c("None" = 'none', 'Bonferroni' = 'bonferroni',
                              'Holm-Sidak' = 'holm'),
                  selected = 'bonferroni', width = '100%')
              ),
              #Correction method for parametric, paired, non-grouped data
              conditionalPanel(
                condition = "input.paratestType == 'para' &&
                input.askPairedssT && !input.dataGroup",
                pickerInput(
                  inputId = 'askCorrectionP',
                  label = "P Value Correction Method",
                  choices = c("None" = 'none', 'Bonferroni' = 'bonferroni',
                              'Holm-Sidak' = 'holm'),
                  selected = 'bonferroni',
                  width = '100%'
                )
              ),
              # Correction method for nonparametric, grouped data (includes Tukey for post-hoc)
              conditionalPanel(
                condition = "input.paratestType == 'nonpara' && input.dataGroup",
                pickerInput(
                  inputId = 'askCorrectionG',
                  label = "P Value Correction Method",
                  choices = c("None" = 'none', 'Tukey' = 'tukey',
                              'Bonferroni' = 'bonferroni', 'Holm-Sidak' = 'holm'),
                  selected = 'bonferroni',
                  width = '100%'
                )
              )
            ),
            
            ## Branch 2. Compare against a control group (controlC) ##
            conditionalPanel(
              condition = "input.compList == 'controlC'",
              #Selects which group is the control
              pickerInput(
                inputId = 'askControl',
                label = 'Select Control',
                choices = current_colnames(),
                selected = current_colnames()[1],
                width = '100%'
              ),
              #Renders selection input options for data columns except control
              uiOutput('testCompInp'),
              
              # Correction method for non-parametric, unpaired (control comparison)
              conditionalPanel(
                condition = "input.paratestType == 'nonpara' && !input.askPairedssT",
                pickerInput(
                  inputId = 'askCorrectionC',
                  label = "P Value Correction Method",
                  choices = c("None" = 'none', 'Bonferroni' = 'bonferroni',
                              'Holm-Sidak' = 'holm'),
                  selected = 'bonferroni', width = '100%')
              ),
              # Correction method for parametric, paired (control comparison)
              conditionalPanel(
                condition = "input.paratestType == 'para' && input.askPairedssT",
                pickerInput(
                  inputId = 'askCorrectionCG',
                  label = "P Value Correction Method",
                  choices = c("None" = 'none', 'Bonferroni' = 'bonferroni',
                              'Holm-Sidak' = 'holm'),
                  selected = 'bonferroni', width = '100%')
              )
            ),
            ## Branch 3. Additional comparisons ##
            # Correction method for nonparametric, paired, ungrouped data
            conditionalPanel(
              condition = "input.paratestType == 'nonpara' &&
              input.askPairedssT && !input.dataGroup",
              pickerInput(
                inputId = 'askCorrectionPC',
                label = "P Value Correction Method",
                choices = c("None" = 'none', 'Bonferroni' = 'bonferroni',
                            'Holm-Sidak' = 'holm'),
                selected = 'bonferroni', width = '100%')
            ),
            
            # Correction method for parametric, paired, grouped data
            conditionalPanel(
              condition = "input.paratestType == 'para' && input.dataGroup",
              pickerInput(
                inputId = 'askCorrectionPCG',
                label = "P Value Correction Method",
                choices = c("None" = 'none', 'Bonferroni' = 'bonferroni',
                            'Holm-Sidak' = 'holm'),
                selected = 'bonferroni', width = '100%')
            )
          )
        ), 
        # Shows options for data columns to be selected for two-sample test (max 2)
        conditionalPanel(
          condition = "input.ttestType == 'tSt'",
          pickerInput(
            'statTwoCol',
            'Select Groups',
            choices = colnames(data()),
            selected = c(colnames(data())[1],colnames(data())[2]),
            multiple = T,
            options = list(
              `max-options` = 2,
              `max-options-text` = "Max 2 selections allowed!"
            )
          )
        ), 
        # Shows message when no post hoc comparisons are selected
        conditionalPanel(
          condition = "input.askComp==false && input.ttestType=='sSt'",
          p('Turn on "Multiple Comparison" to compare between groups or run only omnibus test.', 
            style = 'font-weight:bolder;')
        )
      ),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        ## Final button to send options for running statistical analysis and show report
        actionButton("runAnalysisFinal", "Run Analysis", class = "btn-primary")
      )
    ))
  })
  
  ## Selection input options for data columns except control (controlC)
  output$testCompInp <- renderUI({
    if (input$compList == 'controlC'){
      pickerInput(
        inputId = "statContCols",
        label = "Select Groups", 
        choices = (setdiff(colnames(data()),input$askControl)),
        selected = (setdiff(colnames(data()),input$askControl)),
        multiple = TRUE,
        width = "100%",
        options = list(
          `min-options` = 2,
          `min-options-text` = "Min 2 selections allowed!"
        )
      )
    }
  })
  # Failsafe if there is a mismatch in column names in dataset and selected columns 
  # from all pairwise column list 
  validStatCols <- reactive({
    req(data())
    if (length(intersect(input$statCols,colnames(data())))<1){
      return(colnames(data()))
    } else {
      return(input$statCols)
    }
  })
  
  ## Submit Analysis Button processing (Pre-post hoc test selection)
  vars <-  reactiveValues(count = 0)
  
  output$submitAnalysis <- renderUI({
    req(data()) 
    col_names <- colnames(data())  
    if(isTRUE(input$dataGroup)){
      #Checks correct column names for grouped data
      checkColon <- has_element(str_detect(col_names,':'),FALSE)
      validate(need(isFALSE(checkColon), "")) 
    }
    popover(# Popover guide for users to run analysis when Demo Data is used
      id = "demo_guide",
      #Button to start statistical analysis
      trigger = actionButton('runAnalysis',
                             submitLabel()[[1]],
                             icon = icon(submitLabel()[[2]])),
      "Click to run statistical analysis",
      placement = "bottom",
      options = list(trigger = "manual")
    )
  })
  
  observe({
    #Adds one count to vars after clicking first Submit Analysis button
    #Checks how many time the button was clicked
    if (!is.null(input$runAnalysis)){
      input$runAnalysis
      isolate({
        vars$count <- vars$count + 1
      })
    }
  })
  submitLabel <- reactive({
    #Dynamically checks Submit Analysis button click count and changes label
    if(!is.null(input$runAnalysis)){
      if(vars$count >=2){
        label <- "Update Analysis"
        icon <- "arrow-rotate-right"
      } else {
        label <- "Submit"
        icon <- "circle-arrow-right"
      }
      df <- list(label,icon)
    }
  })
  
  ### All the Statistics test processing ###
  
  
  ###Normality test###
  
  normTest <- reactive({
    req(data()) #important
    normdf <- data.frame()
    dataN <- data() 
    
    if (nrow(dataN) > 5000 || nrow(dataN) < 3) {
      #Safe range to perform Shapiro Wilk test.
      data.frame(
        Test_Report = c(
          'Can not perform Shapiro test for sample number less than 3 or more than 5000.'
        )
      )
    } else{
      # Shapiro-Wilk test for normal/ skewed distribution check
      for (i in 1:ncol(dataN)) {
        tempdf <- tidy(stats::shapiro.test(dataN[, i]))
        normdf <- rbind(normdf, tempdf)
      }
      testrep <- data.frame(normdf[, 1:3])
      newcol <- data.frame("condition" = as.character(colnames(dataN)))
      testrep <- cbind(testrep, newcol)
      testrep <- testrep
      df <- data.frame()
      #Assigns Yes/No to test results if it passes normality test based on p value
      for (i in 1:nrow(testrep)) {
        if (safe_as_numeric(testrep[i, 2]) < 0.05) {
          tempdf2 <- c('No')
          df <- rbind(df, tempdf2)
        } else if (safe_as_numeric(testrep[i, 2]) > 0.05) {
          tempdf2 <- c("Yes")
          df <- rbind(df, tempdf2)
        }
      }
      #Report table preparation 
      pval <- testrep[,2]
      astrM <- asterisk(pval)#converts asterisk from p values
      testrep <- cbind(testrep, df, astrM)
      testrep <- data.frame(testrep[, 4], testrep[1], testrep[2], testrep[6], testrep[5])
      colnames(testrep) <- c( 'Condition', 'Shapiro-Wilk Statistics',
                              'P Value', 'Significance', 'Passed normality test (P<0.05)?')
      #removing unnecessary characters from condition names
      removeStr <- c("*", "<br>", "<sub>", "<sup>", "</sub>", "</sup>")
      for (i in 1:nrow(testrep)) {
        for (pattern in removeStr){
          testrep[i,1] <- gsub(pattern, "", testrep[i,1], fixed = TRUE)
        }
        testrep[i,1] <- gsub('.', " ", testrep[i,1], fixed = TRUE)
      }
      return(testrep)
    }
  })
  
  ###Descriptive Statistics###
  
  descStat <- reactive({
    req(data()) #important
    descTab <- data.frame()
    #Basic descriptive summary statistics calculation
    for (i in 1:ncol(data())) {
      descStat <- data.frame(as.matrix(summary(na.omit(data()[[i]]))))
      descStat <- rbind(length(na.omit(data()[[i]])), descStat)
      descSD <- apply(data()[i], 2, sd, na.rm = TRUE) #SD
      descErr <- descSD / sqrt(length(na.omit(data()[[i]]))) #Std Error
      descStat <- rbind(descStat, descSD, descErr)
      colnames(descStat) <- c(colnames(data()[i]))
      descTab <- append(descTab, descStat)
    }
    descTab <- data.frame(descTab, check.names = F)
    #Report table preparation
    cols <- c(
      'Condition', 'N', 'Minimum', "1st Quartile", 'Median',
      'Mean', '3rd Quartile', 'Maximum', 'Std. Dev.', 'Std. Err.'
    )
    descTab <- data.frame(t(descTab))
    descTab <- cbind(colnames(data()),descTab)
    colnames(descTab) <- cols
    #removing unnecessary characters from condition names
    removeStr <- c("*", "<br>", "<sub>", "<sup>", "</sub>", "</sup>")
    for (i in 1:nrow(descTab)) {
      for (pattern in removeStr){
        descTab[i,1] <- gsub(pattern, "", descTab[i,1], fixed = TRUE)
      }
      descTab[i,1] <- gsub('.', " ", descTab[i,1], fixed = TRUE)
    }
    return(descTab)
  })
  
  #Levene's test for equal variance for full stat report
  levTest <- reactive({
    req(data()) #important
    levdf <- data.frame(tidy(car::leveneTest(value ~ variable,
                                             na.omit(orderdata()))))
    if (levdf$p.value < 0.05)
      (temp <- c(
        'The groups do not have equal variances on the dependent variable'
      ))
    else{
      temp <- c('The groups have approximately equal variances on the dependent variable')
    }
    astrM <- asterisk(levdf$p.value)#converts asterisk from p values
    # Formatting p value digits
    levdf <- data.frame(formatC(levdf[, 1], format = 'g', digits = 3), levdf[, 3],
                        formatC(levdf[, 2], format = 'g', digits = 3),
                        astrM, temp)
    colnames(levdf) <- c('F Statistics', 'DF', 'P Value', 'Significance', 'Remarks')
    return(levdf)
  })
  
  ### Two Sample test processing ###
  
  tsTest <- reactive({
    req(data())#important
    testdata <- data()
    # Selected columns for t test (if more than two is present)
    colname <- (input$statTwoCol) 
    
    # Subset only if selectedCols exist and are valid
    newColNames <- current_colnames()
    selected <- input$selectedCols %||% newColNames
    newColNames <- intersect(selected, newColNames)
    
    #Finds the column index in the main data for selected columns 
    x <- match(colname[1],newColNames)
    y <- match(colname[2],newColNames)
    
    if (input$paratestType == "para") {
      #Sets equal variance parameter for two-sample test for parametric data
      #based on Levene's Test
      if(safe_as_numeric(levTest()$`P Value`)>0.05){
        varEql <- TRUE
      } else { 
        varEql <- FALSE
      }
      #student's t-test (eql var = T) or Welch's t-test (eql var = F)
      testrow <- stats::t.test(testdata[x], testdata[y], alternative = c('two.sided'), 
                               paired = input$askPaired,
                               var.equal = varEql)
      testrow <- tidy(testrow)
      astrN <- asterisk(testrow$p.value)#converts asterisk from p values
      
      #Report table preparation
      colHead <- c('Comparisons', 'Difference in Means', 'Confidence Interval',
                   't Statistics', 'P Value', 'Significance')
      df <- data.frame(paste(colname[1],' vs ', colname[2], sep = ''),
                       safe_as_numeric(formatC(abs(testrow$estimate),digits = 3, format = 'f')),
                       paste(safe_as_numeric(formatC(abs(testrow$conf.low),digits = 3, format = 'f')),
                             ', ', safe_as_numeric(formatC(abs(testrow$conf.high),digits = 3, format = 'f')), sep=''),
                       abs(testrow$statistic),
                       formatC(testrow$p.value, format='g', digits = 3),
                       astrN)
      colnames(df) <- colHead
    } else if (input$paratestType == "nonpara"){
      #Wilcoxon rank sum test (paired) or Mann-whitney U test (unpaired)
      testrow <- tidy(stats::wilcox.test(safe_as_numeric(unlist(testdata[x])),
                                         safe_as_numeric(unlist(testdata[y])),
                                         alternative = c('two.sided'),
                                         paired = input$askPaired, 
                                         correct = T, digits.rank = 7))
      #Getting test rank to show in report
      testRank <- mwz(testdata[x],testdata[y])
      astrN <- asterisk(testrow$p.value)#converts asterisk from p values
      #Report table preparation
      colHead <- c('Comparisons','Mann Whitney U Statistics', 'z Statistics', 
                   paste0('Rank Sum ', colname[1],sep=''),
                   paste0('Rank Sum ', colname[2], sep=''),
                   'Difference in Rank Sum',
                   'P Value', 'Significance')
      # Formatting p value digits
      df <- data.frame(paste(colname[1],' vs ', colname[2], sep = ''),
                       formatC(safe_as_numeric(testrow$statistic),format='f', digits = 3),
                       testRank$z_Statistics, testRank$RankSum_X, testRank$RankSum_Y,
                       abs(testRank$RankSum_X-testRank$RankSum_Y),
                       formatC(safe_as_numeric(testrow$p.value), format = 'g',digits = 3),
                       astrN
      )
      colnames(df) <- colHead
    }
    #removing unnecessary characters from condition names
    removeStr <- c("*", "<br>", "<sub>", "<sup>", "</sub>", "</sup>")
    for (i in 1:nrow(df)) {
      for (pattern in removeStr){
        df[i,1] <- gsub(pattern, "", df[i,1], fixed = TRUE)
      }
      df[i,1] <- gsub('.', " ", df[i,1], fixed = TRUE)
    }
    return(df)
  }) |> bindEvent(input$runAnalysisFinal)
  
  ### Several Sample test processing ###
  
  ssTest <- reactive({
    req(data())#important
    
    if (isTRUE(input$askComp) && input$ttestType == 'sSt'){
      #When pairwise comparison option is active
      #Selects columns based on type of pairwise comparisons
      if(input$compList == 'controlC'){
        colname <- c(input$askControl,input$statContCols)
      } else if (input$compList == 'groupC') {
        colname <- (validStatCols())
      }
    } else if (isFALSE(input$askComp) && input$ttestType == 'tSt'){
      #When pairwise comparison option is inactive (fail-safe)
      colname <- (input$statTwoCol)
    } else{
      #When pairwise comparison option is inactive
      # Subset only if selectedCols exist and are valid (in case)
      newColNames <- current_colnames()
      selected <- input$selectedCols %||% newColNames
      newColNames <- intersect(selected, newColNames)
      colname <- newColNames
    }
    widedata <- data() |> dplyr::select(all_of(colname))
    removeStr <- c("*", "<br>", "<sub>", "<sup>", "</sub>", "</sup>")
    newColNames <- current_colnames()
    
    # Subset only if selectedCols exist and are valid
    selected <- input$selectedCols %||% newColNames
    newColNames <- intersect(selected, newColNames)
    
    #removing unnecessary characters from column names
    for (i in 1:length(newColNames)) {
      for (pattern in removeStr){
        newColNames[i] <- gsub(pattern, "", newColNames[i], fixed = TRUE)
      }
    }
    colnames(widedata) <- newColNames
    
    ## ANOVA Test for parametric data
    if(input$paratestType == 'para'){
      
      ## For Parametric Test ##
      
      if(isTRUE(input$askPairedssT)){
        # For Paired data
        widedata$ID <- row.names(widedata)
        if(isTRUE(input$dataGroup)){
          # For Grouped data
          longdata <- widedata |> pivot_longer(cols = -ID, 
                                               names_to = c('para','groups'),
                                               names_sep = ':',
                                               values_to = 'val')
          aovTest <- rstatix::anova_test(data = longdata, dv = val,
                                         within = c(para, groups), 
                                         wid = ID)
        }else {
          #For ungrouped data
          longdata <- widedata |> pivot_longer(cols = -ID, 
                                               names_to = 'para', values_to = 'val')
          aovTest <- rstatix::anova_test(longdata, dv = val, 
                                         wid = ID, within = para)
        }
        #Greenhouse-Geisser correction for sphericity
        aovTest <- get_anova_table(aovTest, correction = 'GG') 
      } else {
        # For Independent data
        if (isTRUE(input$dataGroup)){
          # For Grouped data
          longdata <- widedata |> pivot_longer(cols=everything(),
                                               names_to = c('para','groups'),
                                               names_sep = ':',
                                               values_to = 'val')
        } else {
          # For Ungrouped data
          longdata <- widedata  |>  pivot_longer(cols = everything(), 
                                                 names_to = 'para', 
                                                 values_to = 'val')
        }
        if (isTRUE(input$dataGroup)){
          # For Groupued data
          # Heteroscedasticity Correction if Levene test gives significant p value
          het_sce <- ifelse (safe_as_numeric(levTest()$`P Value`)>0.05, FALSE, TRUE)
          aovTest <- rstatix::anova_test(longdata, val ~ para*groups, type= 3,
                                         white.adjust = het_sce)
        } else {
          # For Ungrouped data
          if(safe_as_numeric(levTest()$`P Value`)>0.05){
            aovTest <- rstatix::anova_test(longdata, val ~ para, type = 3)
          }else{
            aovTest <- rstatix::welch_anova_test(longdata, val ~ para)
          }
        }   
      }
      
      if(isTRUE(input$dataGroup)){
        aovTest <- data.frame(aovTest$Effect,aovTest$F, aovTest$DFn, aovTest$DFd, aovTest$p)
      } else {
        aovTest <- data.frame(aovTest$F, aovTest$DFn, aovTest$DFd, aovTest$p)
      }
      asterN <- asterisk(aovTest[,ncol(aovTest)])#converts asterisk from p values
      #Report table preparation
      aovTest <- cbind(aovTest,asterN)
      if (input$dataGroup == T){
        colnames(aovTest) <- c('Effect','F Statistics', 'DFn', 'DFd', 'P Value',
                               'Significance')
        effName <- ifelse(is.null(input$legTitle), 'Treatment', input$legTitle)
        aovTest$Effect <- c(effName,'Groups',paste0(effName,' to Groups'))
      }else{
        colnames(aovTest) <- c('F Statistics', 'DFn', 'DFd', 'P Value', 'Significance')
      }
      # Formatting p value digits
      aovTest$`P Value` <- formatC(aovTest$`P Value`, format = 'g', digits = 3)
      return(aovTest)
    } else {
      
      ## For Nonparametric Test ##
      
      if(isTRUE(input$askPairedssT)){
        # For Paired data
        widedata$ID <- row.names(widedata)
        if (isTRUE(input$dataGroup)){
          # For grouped data
          longdata <- widedata |> pivot_longer(cols = -ID, 
                                               names_to = c('para','groups'),
                                               names_sep = ':',
                                               values_to = 'val') |> drop_na(val)
          model <- ARTool::art(data = longdata, 
                               formula = val ~ factor(para)*factor(groups) + (1|ID))
          nparaTest <- anova(model)
        } else {
          # For ungrouped data
          longdata <- widedata |> pivot_longer(cols = -ID, 
                                               names_to = 'para', values_to = 'val') |> drop_na(val)
          nparaTest <- rstatix::friedman_test(longdata, val ~ para | ID)
        }
        
      } else {
        # For Unpaired data
        if (isTRUE(input$dataGroup)){
          # For grouped data
          longdata <- widedata |> pivot_longer(cols = everything(), 
                                               names_to = c('para','groups'),
                                               names_sep = ':',
                                               values_to = 'val')|> drop_na(val)
          model <- ARTool::art(data = longdata, 
                               formula = val ~ factor(para)*factor(groups))
          nparaTest <- anova(model)
        } else{
          # For ungrouped data
          longdata <- widedata |> pivot_longer(cols = everything(), 
                                               names_to = 'para', values_to = 'val')
          nparaTest <- rstatix::kruskal_test(longdata, val ~ para)
        }
        
      }
      
      if (isTRUE(input$dataGroup)){
        nparaTest <- data.frame(nparaTest$Term, nparaTest$`F`, nparaTest$Df,
                                nparaTest$Df.res, nparaTest$`Pr(>F)`)
        
      }else {
        nparaTest <- data.frame(nparaTest$statistic, nparaTest$df, nparaTest$p)
      }
      
      asterN <- asterisk(nparaTest[,ncol(nparaTest)])#converts asterisk from p values
      #Report table preparation
      nparaTest <- cbind(nparaTest,asterN)
      if (input$dataGroup == T){
        colnames(nparaTest) <- c('Effect','F Statistics', 'DFn', 'DFd', 'P Value', 'Significance' )
        effName <- ifelse(is.null(input$legTitle), 'Treatment', input$legTitle)
        nparaTest$Effect <- c(effName,'Groups',paste0(effName,' to Groups'))
      } else{
        colnames(nparaTest) <- c('H Statistics', 'DF', 'P Value', 'Significance' )
      }
      # Formatting p value digits
      nparaTest$`P Value` <- formatC(nparaTest$`P Value`, format = 'g', digits = 3) 
      return(nparaTest)
    }
  })  |> bindEvent(input$runAnalysisFinal)
  
  ### Post Hoc Pairwise Test Processing ###
  
  phTest <- reactive({
    req(data(), current_colnames(), isFALSE(input$dataGroup)) #important
    
    if (isTRUE(input$askComp) && input$ttestType == 'sSt'){
      #When pairwise comparison option is active
      #Selects columns based on type of pairwise comparisons
      if(input$compList == 'controlC'){
        colname <- c(input$askControl,input$statContCols)
      } else if (input$compList == 'groupC') {
        colname <- (validStatCols())
      }else{
        #(fail-safe)
        # Subset only if selectedCols exist and are valid 
        newColNames <- current_colnames()
        selected <- input$selectedCols %||% newColNames
        newColNames <- intersect(selected, newColNames)
        colname <- newColNames
      }
      widedata <- data() |>
        dplyr::select(all_of(colname))
      
      
      longdata <- widedata |>  pivot_longer(cols=everything(), 
                                            names_to = 'para',
                                            values_to = 'val') 
      longdata$para <- factor(longdata$para, levels = colname)   
      
      req(longdata)
      
      phDf <- NULL
      removeStr <- c("*", "<br>", "<sub>", "<sup>", "</sub>", "</sup>")
      
      if (isFALSE(input$askPairedssT)){ 
        ## For independent samples (not repeated samples)
        
        # Parametric test (Control vs Groups comparisons)
        if(input$compList == 'controlC' && input$paratestType == "para"){
          if (levTest()$`P Value`<0.05){
            # Unequal Variance - Dunnett T3 Test
            T3 <- PMCMRplus::tamhaneDunnettTest(x = longdata$val, 
                                                g = relevel(factor(longdata$para), 
                                                            ref = input$askControl))
            phDf <- data.frame(paste(colnames(T3$p.value), '-', 
                                     rownames(T3$p.value)), T3$statistic,
                               T3$p.value, stringsAsFactors = F)
            astrN <- asterisk(phDf[,3])#converts asterisk from p values
            # Formatting p value digits
            phDf[,3] <- ifelse( phDf[,3]== 0 | phDf[,3] < 2.2e-16,
                                "< 2.2e-16",
                                formatC(phDf[,3], format = "g", digits = 4))
            #Report table preparation
            phDf <- cbind(phDf,astrN)
            colnames(phDf) <- c('Comparisons', 'T Statistics', 'P Value', 'Significance')
          } else {
            # Equal Variance - Dunnett's Test
            dT <- DescTools::DunnettTest(x = longdata$val, 
                                         g = relevel(factor(longdata$para),
                                                     ref = input$askControl))
            #Report table preparation
            phDf <- as.data.frame(dT[[1]])
            phDf <- data.frame(rownames(phDf),phDf[,1], paste0(formatC(phDf[,2], format = 'f', digits = 3)
                                                               ,' to ',formatC(phDf[,3], format = 'f', digits = 3), sep=''),
                               phDf[,4], asterisk(phDf[,4]))
            colnames(phDf) <- c('Comparisons','Difference Between Means',
                                '95% CI of Mean Diff', 'P Value', 'Significance')
            # Formatting p value digits
            phDf[,4] <- ifelse( phDf[,4]== 0 | phDf[,4] < 2.2e-16,
                                "< 2.2e-16",
                                formatC(phDf[,4], format = "g", digits = 4))
          }
          
        } else if (input$compList == 'groupC' && input$paratestType == "para" &&
                   levTest()$`P Value`<0.05) {
          # Parametric all pairwise groups
          
          # For unequal variance - Games-Howell
          
          phDf <- rstatix::games_howell_test(longdata, val ~ para, detailed = T)
          
          astrN <- asterisk(phDf$p.adj)[[1]]#converts asterisk from p values
          #Report table preparation
          phDf <- data.frame(paste0(phDf$group1,'-',phDf$group2, sep=''), -(phDf$estimate),
                             paste0(formatC(-(phDf$conf.low), format = 'f', digits = 3)
                                    ,' to ',formatC(-(phDf$conf.high), format = 'f', digits = 3)),
                             phDf$df, phDf$p.adj, astrN)
          colnames(phDf) <- c('Comparisons', 'Difference Between Means', 
                              '95% CI of Mean Diff','DFd', 'P Value Adjusted', 'Significance')
          # Formatting p value digits
          phDf$`P Value Adjusted` <- formatC(phDf$`P Value Adjusted`, format = 'g', digits = 3)
          
        } else if (input$compList == 'groupC' && input$paratestType == "para" &&
                   levTest()$`P Value`>0.05) {
          # For Equal variance - Tukey's HSD
          
          phDf <- rstatix::tukey_hsd(longdata, val ~ para, detailed = T)
          
          astrN <- asterisk(phDf$p.adj)[[1]]#converts asterisk from p values
          # Report table preparation
          phDf <- data.frame(paste0(phDf$group1,'-',phDf$group2, sep=''), -(phDf$estimate),
                             paste0(formatC(-(phDf$conf.low), format = 'f', digits = 3)
                                    ,' to ',formatC(-(phDf$conf.high), format = 'f', digits = 3)),
                             phDf$p.adj, astrN)
          colnames(phDf) <- c('Comparisons', 'Difference Between Means', 
                              '95% CI of Mean Diff', 'P Value Adjusted', 'Significance')
          # Formatting p value digits
          phDf$`P Value Adjusted` <- formatC(phDf$`P Value Adjusted`, format = 'g', digits = 3)
          
        } else if (input$compList == 'controlC' && input$paratestType == "nonpara"){
          
          # For nonparametric (Control vs Groups)
          
          #  Dunn's test
          phDf <- longdata |> rstatix::dunn_test(val ~ para,
                                                 p.adjust.method = input$askCorrectionC, 
                                                 detailed = T) |> data.frame()
          astrN <- asterisk(phDf$p.adj)#converts asterisk from p values
          # Report table preparation
          phDf <- data.frame(paste0(phDf$group1,'-',phDf$group2, sep=''), -(phDf$estimate), -(phDf$statistic),
                             phDf$p, phDf$p.adj, astrN)
          colnames(phDf) <- c('Comparisons', 'Mean Rank Difference', 'Z Statistic',
                              'Raw P Value', 'Adjusted P Value', 'Significance')
          # Filter out only Control vs Groups comparisons
          phDf <- phDf |> dplyr::filter(grepl(input$askControl, Comparisons))
          # Formatting p value digits
          phDf$`Raw P Value` <- formatC(phDf$`Raw P Value`, format = 'g', 
                                        digits = 3)
          phDf$`Adjusted P Value` <- formatC(phDf$`Adjusted P Value`, format = 'g', 
                                             digits = 3)
        } else if (input$compList == 'groupC' && input$paratestType == "nonpara"){
          # For nonparametric (all pairwise groups)
          # Dunn's test
          phDf <- longdata |> rstatix::dunn_test(val ~ para, p.adjust.method = input$askCorrection, 
                                                 detailed = T) |> data.frame()
          astrN <- asterisk(phDf$p.adj)#converts asterisk from p values
          # Report table preparation
          phDf <- data.frame(paste0(phDf$group1,'-',phDf$group2, sep=''), -(phDf$estimate), -(phDf$statistic),
                             phDf$p, phDf$p.adj, astrN)
          colnames(phDf) <- c('Comparisons', 'Mean Rank Difference', 'Z Statistic',
                              'Raw P Value', 'Adjusted P Value', 'Significance')
          # Formatting p value digits
          phDf$`Raw P Value` <- formatC(phDf$`Raw P Value`, format = 'g', 
                                        digits = 3)
          phDf$`Adjusted P Value` <- formatC(phDf$`Adjusted P Value`, format = 'g', 
                                             digits = 3)
        }
      } else {
        ## For repeated measurement ##
        
        if (input$paratestType == 'nonpara'){
          # For nonparametric test
          # Conover's test
          cT <- PMCMRplus::frdAllPairsConoverTest(as.matrix(widedata),
                                                  p.adjust=input$askCorrectionPC)
          cT_rawP <- PMCMRplus::frdAllPairsConoverTest(as.matrix(widedata),
                                                       p.adjust='none')
          cT <- summary(cT)
          cT_rawP <- summary(cT_rawP)
          #function to convert row and col names into a comparison list
          phDf <- TabToVec(cT) 
          cT_rawP <- TabToVec(cT_rawP)
          # Removing unnecessary characters from comparison names for conover's test here
          # It somehow does not work later
          phDf[,1] <- gsub('-','vs',phDf[,1])
          phDf[,1] <- gsub('-',' ',phDf[,1])
          for (pattern in removeStr){
            phDf[,1] <- gsub(pattern, "", phDf[,1], fixed = TRUE)
          }
          astrN <- asterisk(phDf[,3])#converts asterisk from p values
          # Report table preparation
          phDf <- data.frame(phDf[,1], phDf[,2], cT_rawP[,3], phDf[,3], astrN)
          # Formatting p value digits
          phDf[,3] <- ifelse( phDf[,3]== 0 | phDf[,3] < 2.2e-16,
                              "< 2.2e-16",
                              formatC(phDf[,3], format = "g", digits = 3))
          phDf[,4] <- ifelse( phDf[,4]== 0 | phDf[,4] < 2.2e-16,
                              "< 2.2e-16",
                              formatC(phDf[,4], format = "g", digits = 3))
          colnames(phDf) <- c('Comparisons', 'T Statistics', 'Raw P Value',
                              'Adjusted P Value', 'Significance')
          if (input$compList=='controlC'){
            #Filters only control vs groups
            phDf <- phDf |> dplyr::filter(grepl(input$askControl,Comparisons))
            return(phDf)
          }else{
            return(phDf)
          }
        } else {
          # For Parametric test 
          # Repeated Anova - pairwise-t-test
          if (input$compList=='controlC'){
            # For control vs groups
            ptT <- rstatix::pairwise_t_test(longdata, val ~ para, p.adjust.method = input$askCorrectionCG,
                                            paired = T, ref.group = input$askControl)
          } else {
            # For all groups
            ptT <- rstatix::pairwise_t_test(longdata, val ~ para, p.adjust.method = input$askCorrectionP,
                                            paired = T)
          }
          # Report table preparation
          phDf <- data.frame(paste0(ptT$group1,'-',ptT$group2), ptT$statistic,
                             ptT$df, ptT$p,ptT$p.adj,asterisk(ptT$p.adj))
          colnames(phDf) <- c('Comparisons', 'T Statistics', 'Dfd', 
                              'Raw P Value', 'Adjusted P Value', 'Significance')
          # Formatting p value digits
          phDf[,5] <- ifelse(phDf[,5]==0 | phDf[,5]<2.2e-16, "2.2e-16", 
                             formatC(phDf[,5], format = 'g', digits = 3))
          phDf[,4] <- ifelse(phDf[,4]==0 | phDf[,4]<2.2e-16, "2.2e-16", 
                             formatC(phDf[,4], format = 'g', digits = 3))
        }
      }
      
      for (pattern in removeStr){
        phDf[,1] <- gsub(pattern, "", phDf[,1], fixed = TRUE)
      }
      phDf[,1] <- gsub('.', " ", phDf[,1], fixed = TRUE)
      phDf[,1] <- gsub('-', " vs ", phDf[,1], fixed = TRUE)
      return(phDf)
    } else {
      #If Pairwise comparisons not selected for several samples test then it returns nothing
      return(NULL)
    }
  })  |> bindEvent(input$runAnalysisFinal)
  
  
  ### Grouped data posthoc test ###
  
  phTestG <- reactive({
    req(data(), isTRUE(input$dataGroup)) #important
    
    removeStr <- c("*", "<br>", "<sub>", "<sup>", "</sub>", "</sup>")
    
    # Subset only if selectedCols exist and are valid
    newColNames <- current_colnames()
    selected <- input$selectedCols %||% newColNames
    newColNames <- intersect(selected, newColNames)
    colname <- newColNames
    #Removing unnecessary characters from column names
    for (i in 1:length(newColNames)) {
      for (pattern in removeStr){
        newColNames[i] <- gsub(pattern, "", newColNames[i], fixed = TRUE)
      }
      newColNames[i] <- gsub(' ', '.', newColNames[i], fixed = TRUE)
    }
    #Validates presence of ':' for group name separation
    checkColon <- has_element(str_detect(newColNames,':'),FALSE)
    validate(need(isFALSE(checkColon), ""))
    
    widedata <- data() |>  dplyr::select(all_of(validStatCols()))
    
    colnames(widedata) <- newColNames
    
    if(isTRUE(input$askPairedssT)){
      # For paired ore repeated samples data
      widedata$ID <- row.names(widedata)
      longdata <- widedata |>  pivot_longer(cols=-ID, 
                                            names_to = c('para','groups'),
                                            names_sep = ':',
                                            values_to = 'val')|> drop_na(val)
    }else{
      # For unpaired data
      longdata <- widedata |>  pivot_longer(cols=everything(), 
                                            names_to = c('para','groups'),
                                            names_sep = ':',
                                            values_to = 'val')|> drop_na(val)
    }
    longdata$para <- factor(longdata$para)
    longdata$groups <- factor(longdata$groups)
    
    if(input$paratestType == 'nonpara'){
      #For Nonparametric test (all pairwise groups)
      
      if(isTRUE(input$askPairedssT)){
        # For paired or repeated samples data
        # Aligned Ranked Transform test
        model <- ARTool::art(data = longdata, 
                             formula= val ~ para*groups + (1|ID))
      } else {
        # For unpaired data
        model <- ARTool::art(data = longdata, 
                             formula= val ~ para*groups)
      }
      #ART Contrasts for pairwise comparisons
      phDfA <- art.con(model, ~para,
                       adjust = input$askCorrectionG) |> summary()
      phDfB <- art.con(model, ~groups,
                       adjust = input$askCorrectionG) |> summary()
      phDfAB <- art.con(model, ~para*groups,
                        adjust = input$askCorrectionG) |> summary()
      
      new_cols <- c("Comparison", "Mean Aligned Rank", "Standard Error", "Df",
                    "t Ratio", "P Value", "Significance")
      # Report table preparation for three effect groups
      phDf_format <- function(df){
        df <- df |> mutate(sig = symnum(p.value, corr = F, na = F, 
                                        cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                        symbols = c("***", "**", "*", ".", " ")),
                           p.value = formatC(p.value, format = 'g', digits = 3)) 
        
        df$sig <- unlist(asterisk(df$p.value))#converts asterisk from p values
        colnames(df) <- new_cols
        return(df)
      }
      #Combining three effect groups
      phDfA <- phDf_format(phDfA)
      phDfB <- phDf_format(phDfB)
      phDfAB <- phDf_format(phDfAB)
      #Removing unnecessary characters from comparisons list
      phDfAB$`Comparison` <- gsub(",",", ",phDfAB$`Comparison`)
      phList <- list(phDfA, phDfB, phDfAB)
      removeStr <- c("*", "<br>", "<sub>", "<sup>", "</sub>", "</sup>")
      
      phList <- lapply(phList, function(df) {
        if (nrow(df) > 0 && is.character(df[[1]])) {
          for (pattern in removeStr){
            df[[1]] <- gsub(pattern, "", df[[1]], fixed = TRUE)
          }
          df[[1]] <- gsub(".", " ", df[[1]], fixed = TRUE)
          df[[1]] <- gsub(" - ", " vs ", df[[1]], fixed = TRUE)
        }
        df
      })
      #Changing effect name according to user input
      effName <- ifelse(is.null(input$legTitle), 'Treatment', input$legTitle)
      names(phList) <- c("Groups", effName,
                         paste("Groups:", effName, collapse = '') )
      
    } else {
      # For Parametric Test (all pairwise groups)
      if(isTRUE(input$askPairedssT)){
        # For paired or repeated samples data
        model <- lme4::lmer(data = longdata, 
                            formula = val ~ para*groups + (1|ID))
      } else {
        # For unpaired data
        model <- stats::aov(data = longdata, 
                            formula = val ~ para*groups)
      }
      #Emmeans for pairwise comparisons
      phDfA <- emmeans::emmeans(model, pairwise~para,
                                adjust = input$askCorrectionPCG)$contrasts |> summary()
      phDfB <- emmeans::emmeans(model, pairwise~groups,
                                adjust = input$askCorrectionPCG)$contrasts |> summary()
      phDfAB <- emmeans::emmeans(model, pairwise~para*groups,
                                 adjust = input$askCorrectionPCG)$contrasts |> summary()
      # Report table preparation for three effect groups
      new_cols <- c("Comparison", "Mean Aligned Rank", "Standard Error", "Df",
                    "t Ratio", "P Value", "Significance")
      
      phDf_format <- function(df){
        df <- df |> mutate(sig = symnum(p.value, corr = F, na = F, 
                                        cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                        symbols = c("***", "**", "*", ".", " ")),
                           p.value = formatC(p.value, format = 'g', digits = 3)) 
        
        df$sig <- unlist(asterisk(df$p.value))
        colnames(df) <- new_cols
        return(df)
      }
      #Combining three effect groups
      phDfA <- phDf_format(phDfA)
      phDfB <- phDf_format(phDfB)
      phDfAB <- phDf_format(phDfAB)
      #Removing unnecessary characters from comparisons list
      phDfAB$`Comparison` <- gsub(" ",", ",phDfAB$`Comparison`)
      phList <- list(phDfA, phDfB, phDfAB)
      
      removeStr <- c("*", "<br>", "<sub>", "<sup>", "</sub>", "</sup>")
      
      phList <- lapply(phList, function(df) {
        if (nrow(df) > 0 && is.character(df[[1]])) {
          for (pattern in removeStr){
            df[[1]] <- gsub(pattern, "", df[[1]], fixed = TRUE)
          }
          df[[1]] <- gsub(".", " ", df[[1]], fixed = TRUE)
          df[[1]] <- gsub(" - ", " vs ", df[[1]], fixed = TRUE)
          df[[1]] <- gsub(", -,", " vs ", df[[1]], fixed = TRUE)
        }
        df
      })
    }
    return(phList)
  })  |> bindEvent(input$runAnalysisFinal)
  
  ### Test Summary Description Logic ###
  
  testSumm <- reactive({
    if (input$ttestType == 'tSt') {
      # Two-sample tests
      if (input$paratestType == 'nonpara') { 
        # nonparametric
        if (isTRUE(input$askPaired)) {
          #paired
          "Experimental Summary: Wilcoxon signed-rank test (paired samples)"
        } else {
          #unpaired
          "Experimental Summary: Mann-Whitney U test (independent samples)"
        }
      } else {  
        # parametric
        if (isTRUE(input$askPaired)) {
          #paired
          "Experimental Summary: Paired Student's t-test"
        } else {
          #unpaired
          if (safe_as_numeric(levTest()$`P Value`) < 0.05) {
            #unequal variance
            "Experimental Summary: Welch's t-test (unequal variances assumed)"
          } else {
            #equal variance
            "Experimental Summary: Student's t-test (equal variances assumed)"
          }
        }
      }
    } else {  
      #Several samples test
      base_test <- ""
      posthoc_text <- ""
      
      if (input$paratestType == 'nonpara') {
        #nonparametric
        if(isTRUE(input$dataGroup)){
          #grouped data
          base_test <- "Aligned Rank Transform (ART) test"
          posthoc_text <- " with ART-C post-hoc test"
          
        } else {
          if (isTRUE(input$askPairedssT)) {
            #paired
            base_test <- "Friedman test (nonparametric repeated measures)"
            if (isTRUE(input$askComp)) {
              posthoc_text <- " with Conover's post-hoc test"
            }
          } else {
            #unpaired
            base_test <- "Kruskal-Wallis test (nonparametric independent samples)"
            if (isTRUE(input$askComp)) {
              posthoc_text <- " with Dunn's post-hoc test"
            }
          }
        }
        
      } else {  
        # parametric
        if (isTRUE(input$askPairedssT)) {
          #repeated samples
          base_test <- "Repeated-measures ANOVA (with Greenhouse-Geisser correction for sphericity)"
          if (isTRUE(input$askComp)) {
            posthoc_text <- " with pairwise paired t-tests (p-value adjusted)"
          }
        } else {  
          # independent samples
          if (safe_as_numeric(levTest()$`P Value`) < 0.05) {
            #unequal variance
            if (isTRUE(input$dataGroup)){
              base_test <- "Two-way ANOVA (unequal variances)"
            } else {
              base_test <- "Welch's one-way ANOVA (unequal variances)"
            }
          } else {
            #equal variance
            if (isTRUE(input$dataGroup)){
              base_test <- "Two-way ANOVA (equal variances assumed)"
            } else {
              base_test <- "One-way ANOVA (equal variances assumed)"
            }
          }
          
          if (isTRUE(input$askComp)) {
            if (input$compList == 'controlC') {
              #Control vs group
              if (safe_as_numeric(levTest()$`P Value`) < 0.05) {
                #unequal variance
                posthoc_text <- " with Dunnett's T3 post-hoc test (comparisons against control)"
              } else {
                #equal variance
                posthoc_text <- " with Dunnett's post-hoc test (comparisons against control)"
              }
            } else {
              #All pairwise
              if (safe_as_numeric(levTest()$`P Value`) < 0.05) {
                #unequal variance
                posthoc_text <- " with Games-Howell post-hoc test (all pairwise comparisons)"
              } else {
                #equal variance
                posthoc_text <- " with Tukey's HSD post-hoc test (all pairwise comparisons)"
              }
            }
            if (isTRUE(input$dataGroup)){
              posthoc_text <- " with pairwise t-tests (p-value adjusted)"
            }
          } else {
            posthoc_text <- ""
          }
        }
      }
      paste0("Experimental Summary: ", base_test, posthoc_text)
    }
  })
  
  ### Effect Size Calculation Logic ###
  
  effSize <- reactive({
    req(data()) #important
    if (input$ttestType == 'tSt') {
      # Two sample test
      if (isTRUE(input$askComp)){
        colname <- (input$statTwoCol)
      } else{
        # Subset only if selectedCols exist and are valid
        newColNames <- current_colnames()
        selected <- input$selectedCols %||% newColNames
        newColNames <- intersect(selected, newColNames)
        colname <- newColNames
      }
      widedata <- data() |> dplyr::select(all_of(colname))
      longdata <- widedata |> pivot_longer(names_to = 'variable', values_to = 'value',
                                           cols = everything())
      #checks for varaince
      if(safe_as_numeric(levTest()$`P Value`)>0.05){
        varEql <- TRUE
      } else {
        varEql <- FALSE
      }
      #Cohen's d test 
      effDf <- rstatix::cohens_d(data = longdata,
                                 formula = value ~ variable,
                                 paired = ifelse(isTRUE(input$askPairedssT), TRUE, FALSE),
                                 var.equal = varEql)
      # Report table prepration
      # 0.2 (small effect), 0.5 (moderate effect), and 0.8 (large effect)
      effDf <- data.frame("Cohen's d" = effDf$`effsize`,
                          "Sample Magnitude" = effDf$`magnitude`, check.names = F)
    } else {
      # Several sample test
      if (isTRUE(input$askPairedssT)){
        #Repeated measured samples
        widedata <- data() |> mutate(ID = rownames(data()))
        longdata <- widedata |> pivot_longer(cols = -ID,
                                             names_to = 'variable',
                                             values_to = 'value')
      } else {
        #Independent samples
        longdata <- orderdata()
      }
      if (input$paratestType == 'nonpara'){
        # Nonparametric tests
        # Friedman's Effect Size
        if (isTRUE(input$askPairedssT)){
          #Repeated measured samples
          if(isFALSE(input$dataGroup)){
            # ungrouped data
            effDf <- rstatix::friedman_effsize(data = longdata, 
                                               formula = value ~ variable | ID)
            effDf <- data.frame("Kendall's W" = effDf$`effsize`,
                                "Sample Magnitude" = effDf$`magnitude`, check.names = F)
          } else{
            #grouped data
            effDfV <- longdata |> rstatix::friedman_effsize(value ~ variable | ID)
            effDfG <- longdata |> rstatix::friedman_effsize(value ~ groups | ID)
            
            longdata_VG <- longdata %>%
              mutate(combined_group = paste(variable, groups, sep = ":"))
            effDfVG <- longdata_VG |> rstatix::friedman_effsize(value ~ combined_group | ID)
            
            # Report table preparation
            # 0.1 - < 0.3 (small effect), 0.3 - < 0.5 (moderate effect) and >= 0.5 (large effect)
            effName <- ifelse(is.null(input$legTitle), 'Treatment', input$legTitle)
            effDf <- data.frame("Groups" = c(effName,'Groups',paste0(effName,' to Groups')) ,
                                "eta2 [H]" = c(effDfV$`effsize`,
                                               effDfG$`effsize`,
                                               effDfVG$`effsize`),
                                "Sample Magnitude" = c(effDfV$`magnitude`,
                                                       effDfG$`magnitude`,
                                                       effDfVG$`magnitude`),
                                check.names = F)
          }
        } else {
          # Independent Samples
          #Kruskal-Wallis's Effect Size
          if (isFALSE(input$dataGroup)){
            #ungrouped data
            effDf <- longdata |> 
              rstatix::kruskal_effsize(value ~ variable)
            effDf <- data.frame("eta2 [H]" = effDf$`effsize`,
                                "Sample Magnitude" = effDf$`magnitude`, check.names = F)
          } else {
            #grouped data
            effDfV <- longdata |> rstatix::kruskal_effsize(value ~ variable)
            effDfG <- longdata |> rstatix::kruskal_effsize(value ~ groups)
            longdata_VG <- longdata %>%
              mutate(combined_group = paste(variable, groups, sep = ":"))
            effDfVG <- longdata_VG |> rstatix::kruskal_effsize(value ~ combined_group)
            effName <- ifelse(is.null(input$legTitle), 'Treatment', input$legTitle)
            #Report table preparation
            # 0.01- < 0.06 (small effect), 0.06 - < 0.14 (moderate effect) and >= 0.14 (large effect)
            effDf <- data.frame("Groups" = c(effName,'Groups',paste0(effName,' to Groups')) ,
                                "eta2 [H]" = c(effDfV$`effsize`,
                                               effDfG$`effsize`,
                                               effDfVG$`effsize`),
                                "Sample Magnitude" = c(effDfV$`magnitude`,
                                                       effDfG$`magnitude`,
                                                       effDfVG$`magnitude`),
                                check.names = F)
          }
          
        }
      } else {
        #Parametric tests
        mag <- function(df){
          #setting effect size magnitude
          temp <- data.frame()
          for (i in 1:length(df)){
            if (df[i]<=0.01) {
              magn <- 'small'
            }else if (df[i]>0.01 && df[i]<=0.06){
              magn <- 'moderate'
            } else {
              magn <- 'large'
            }
            temp <- rbind(temp,magn)
          }
          return(temp)
        }
        if(isTRUE(input$dataGroup)){
          #grouped data
          #Partial Eta Squared value for effect size
          res.aov <- aov(data = longdata, value ~ variable*groups)
          etaDf <- rstatix::partial_eta_squared(res.aov)
          magDf <- mag(etaDf)
          effName <- ifelse(is.null(input$legTitle), 'Treatment', input$legTitle)
          effDf <- data.frame("Groups" = c(effName,'Groups',paste0(effName,' to Groups')),
                              "Partial Eta2" = etaDf,
                              "Sample Magnitude" = unlist(magDf), check.names = F)
          rownames(effDf) <- NULL
        }else {
          #ungrouped data
          if (isTRUE(input$askPairedssT)){
            # Repeated measured samples
            #Partial Eta Squared value for effect size
            res.aov <- aov(data = longdata, value ~ variable)
            etaDf <- partial_eta_squared(res.aov)
            magDf <- mag(etaDf)
            effDf <- data.frame("Partial Eta2" = etaDf,
                                "Sample Magnitude" = unlist(magDf), check.names = F)
          } else {
            # Independent samples
            #Eta Squared value for effect size
            res.aov <- aov(data = longdata, value ~ variable)
            etaDf <- eta_squared(res.aov)
            magDf <- mag(etaDf)
            effDf <- data.frame("Eta2" = etaDf,
                                "Sample Magnitude" = unlist(magDf), check.names = F)
          } 
          rownames(effDf) <- NULL
        }
      }
    }
    return(effDf)
  }) 
  
  
  #### Significance Test report display ####
  
  #Upon Clicking Final Run Analysis button checks for correct selection and
  #proceeds otherwise shows warning
  observeEvent(input$runAnalysisFinal,{ 
    # Two-sample test
    req(data())
    if (input$ttestType == "tSt") {
      if (length(input$statTwoCol) != 2) {
        show_alert(
          title = "Warning",
          text = "For two-sample test, select exactly 2 groups.",
          type = "warning"
        )
        return()  #Stop here, keep modal open
      }
    }
    
    # Several-sample test
    if (input$ttestType == "sSt") {
      if (isTRUE(input$askComp)) {
        if (input$compList == "controlC") {
          # Need control and at least 2 comparisons (total >=3)
          if (is.null(input$statContCols) || length(input$statContCols) < 2) {
            show_alert(
              title = "Warning",
              text = "For control comparison, select at least 2 groups to compare against control.",
              type = "warning"
            )
            return()
          }
        } else if (input$compList == "groupC") {
          # Need at least 3 groups for all pairwise comparisons
          if (is.null(validStatCols()) || length(validStatCols()) < 3) {
            show_alert(
              title = "Warning",
              text = "For pairwise comparisons, select at least 3 groups.",
              type = "warning"
            )
            return()
          }
        }
      } else {
        # No multiple comparisons (need at least 3 columns in data)
        if (ncol(data()) < 3) {
          show_alert(
            title = "Warning",
            text = "For several-sample test without multiple comparisons, data must have at least 3 columns.",
            type = "warning"
          )
          return()
        }
      }
    }
    
    showPageSpinner(
      type = getOption("page.spinner.type", default = 5),
      caption = getOption("page.spinner.caption", "Running Analysis")
    )
    Sys.sleep(runif(min = 2,max = 4,n=1)) #Illusion of analysis processing
    hidePageSpinner()
    removeModal()
  },ignoreNULL = T)
  
  #Normality test report table 
  normTestTable <- reactive({
    req(normTest())
    tableNorm <- normTest()
    #Formatting p value digits
    tableNorm$`P Value` <- formatC(tableNorm$`P Value`, format = 'g', digits = 3)
    tableNorm$`Shapiro-Wilk Statistics` <- formatC(tableNorm$`Shapiro-Wilk Statistics`,
                                                   format = 'g', digits = 3)
    #Filters only control vs groups comparisons when Control vs Groups selected
    if (input$ttestType == 'tSt' && ncol(data()) != length(input$statTwoCol)){
      tableNorm <- tableNorm |> dplyr::filter(Condition %in% input$statTwoCol)
    }else {
      tableNorm <- tableNorm
    }
    return(tableNorm)
  })
  output$normStatTab <- renderTable({
    normTestTable()
  }, striped = T, width = '100%', align = 'l') |> bindEvent(input$runAnalysisFinal)
  
  #Rendering option for QQ-plot for all data columns for showing distribution
  QQplot <- reactive({
    req(data())
    if (input$ttestType == 'tSt'){
      df <- data() |> dplyr::select(input$statTwoCol)
    } else {
      df <- data()
    }
    removeStr <- c("*", "<br>", "<sub>", "<sup>", "</sub>", "</sup>")
    plot_list <- lapply(1:ncol(df), function(i) {
      force(i) 
      for (pattern in removeStr){
        colnames(df)[[i]] <- gsub(pattern, "", colnames(df)[[i]], fixed = TRUE)
      }
      colnames(df) <- gsub("."," ",colnames(df), fixed = TRUE)
      ggplot(df, aes(sample = .data[[colnames(df)[i]]])) +
        qqplotr::stat_qq_band(bandType = "pointwise", fill = "lavender", alpha = 0.5) +
        qqplotr::stat_qq_point(shape = 1) + 
        qqplotr::stat_qq_line() +
        ggtitle(colnames(df)[i]) +
        theme_classic() +
        labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
        theme(
          panel.border = element_rect(fill = NA, linewidth = 1.2),
          plot.title = element_text(size = 18, face = 'bold'),
          axis.line = element_line(linewidth = 0.8),
          axis.text = element_text(size = 14),
          axis.title =  element_text(size = 16)
        )
    })
    patchwork::wrap_plots(plot_list)
  })
  output$normQQPlot <- renderPlot({
    QQplot()
  })
  
  #QQPlot download handler
  output$dnldQQ <- downloadHandler(
    filename = function(){
      c("QQ_Plot.png")
    },
    content = function(file){
      ggsave(
        file,
        plot = QQplot(),
        width = 750 / 72,
        height = 600 / 72,
        units = "in",
        dpi = safe_as_numeric(150)
      )
    }
  )
  # Shows QQ plot inside a modal upon button clicking
  observeEvent(input$showQQ,{
    showModal(modalDialog(
      title = "QQ Plots for Probability Distribution",
      plotOutput('normQQPlot', height = 600),
      size = 'xl',
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        downloadButton('dnldQQ', label = "Download Plot", icon = icon("download"))
      )
    ))
  })
  
  ## Significance test report title selection ##
  
  sigRepTit <- reactive({
    ## For parametric Two sample t-test or Anova-test titles
    if(safe_as_numeric(levTest()$`P Value`)<0.05){
      #Unequal variance
      tTitle <- "Welch's t-test report" #two samples
      sTitle <- "Welch's One-way ANOVA test report" #several samples
    } else {
      #Equal variance
      tTitle <- "Student's t-test report" #two samples
      sTitle <- "One-way ANOVA test report" #several samples
    }
    if (isTRUE(input$dataGroup)){
      #Grouped data
      sTitle <- "Two-way ANOVA test report" #several samples
    }
    
    ## For nonparametric Two sample test titles
    if (isTRUE(input$askPaired)){
      #Paired sample
      mwTitle <- "Wilcoxon signed-rank test report"
    } else {
      #Unpaired samples
      mwTitle <-  "Mann-Whitney U test report"
    }
    ## For nonparametric Several sample test titles
    if (isTRUE(input$askPairedssT)){
      #Paired sample
      npTitle <- "Friedman test report"
      
    } else {
      #Unpaired samples
      npTitle <-  "Kruskal Wallis test report"
    }
    if (isTRUE(input$dataGroup)){
      # Grouped daata
      npTitle <- "Aligned Rank Transform test report"
    }
    
    if (isTRUE(input$askComp)) {
      # When pairwise comparison is active
      # Default correction text handling 
      get_corr_text <- function(corr) {
        if (corr == 'none') 'no' 
        else if (corr == 'holm') 'holm-sidak' 
        else if (corr == 'tukey') 'Tukey'  # only possible in some nonpara grouped cases
        else corr
      }
      
      if (isTRUE(input$askPairedssT)) {  
        #Repeated measure sample
        if (input$paratestType == 'nonpara') {  
          # Non-parametric
          if (isTRUE(input$dataGroup)) {  
            # Grouped data 
            # ART for factorial/repeated
            corr <- input$askCorrectionG
            corr_text <- get_corr_text(corr)
            posthoc_text <- paste("Post-hoc multifactor contrasts following Aligned Rank Transform (ART-C) with", 
                                  corr_text, "correction")
            
          } else {  
            # Ungrouped data 
            # Friedman + Conover
            corr <- input$askCorrectionPC
            corr_text <- get_corr_text(corr)
            posthoc_text <- paste("Conover's post-hoc test following Friedman test with", 
                                  corr_text, "correction")
          }
          
        } else {  
          # Parametric test 
          # Determine which correction input to use
          corr <- if (input$compList == 'controlC') input$askCorrectionCG else input$askCorrectionP
          corr_text <- get_corr_text(corr)
          
          if (input$compList == 'controlC') {
            posthoc_text <- paste("Paired t-tests against the control group with", corr_text, "correction")
          } else {
            posthoc_text <- paste("Pairwise paired t-tests with", corr_text, "correction")
          }
          
        }
        
      } else {  
        # Independent samples
        
        if (input$paratestType == 'nonpara') {  
          # Non-parametric
          if (isTRUE(input$dataGroup)) {  
            # Grouped data 
            # ART for factorial/independent
            corr <- input$askCorrectionG
            corr_text <- get_corr_text(corr)
            posthoc_text <- paste("Post-hoc multifactor contrasts following Aligned Rank Transform (ART-C) with", 
                                  corr_text, "correction")
            
          } else {  
            # Ungrouped data
            # Kruskal-Wallis + Dunn
            # Determine which correction input to use
            corr <- if (input$compList == 'controlC') input$askCorrectionC else input$askCorrection
            corr_text <- get_corr_text(corr)
            
            if (input$compList == 'controlC') {
              #Control vs Groups
              posthoc_text <- paste("Dunn's post-hoc tests for comparisons against the control with", 
                                    corr_text, "correction")
            } else {
              #All pairwise groups
              posthoc_text <- paste("Dunn's post-hoc test following Kruskal-Wallis test with", 
                                    corr_text, "correction")
            }
          }
        } else {  
          # Parametric test
          if (input$compList == 'controlC') {
            #Control vs Groupes
            posthoc_text <- "Dunnett's post-hoc test following one-way ANOVA"
          } else {
            #All pairwise groups
            if (isTRUE(input$dataGroup)) {
              # Grouped data
              corr <- input$askCorrectionPCG
              corr_text <- get_corr_text(corr)
              posthoc_text <- paste("Pairwise independent samples t-tests with", corr_text, "correction")
            } else {
              # Ungrouped data
              if (safe_as_numeric(levTest()$`P Value`) < 0.05) {
                posthoc_text <- "Games-Howell post-hoc test following one-way ANOVA"
              } else {
                posthoc_text <- "Tukey's HSD post-hoc test following one-way ANOVA"
              }
            }
          }
        }
      }
    } else {
      posthoc_text <- ""
    }
    # Combining final title 
    finalTitles <- list(tTitle, sTitle, mwTitle, npTitle, posthoc_text)
  })
  
  ## Generates a significance report summary (color-coded) for Omnibus test
  npResult <- reactive({
    req(ssTest()) #important
    if (isTRUE(input$dataGroup)){
      #Grouped data
      statM <- ifelse(input$paratestType=='nonpara', 'medians', 'means')
      npRes <- data.frame()
      for (i in 1:nrow(ssTest())){
        pState <- ifelse(safe_as_numeric(ssTest()[i,5])<=0.05,'a','no')
        pState2 <- ifelse(safe_as_numeric(ssTest()[i,5])<=0.05,' of at least two conditions within the "',' of conditions within the "')
        pStyle <- ifelse(safe_as_numeric(ssTest()[i,5])<=0.05,'#B9EBDE','#F7D7D7')
        temp1 <- paste0("There is ",pState," significant difference between ",statM, 
                        pState2,ssTest()[i,1],'" Effect.')
        temp2 <- paste0("padding:5px; background-color:",pStyle,
                        "; border: 1px solid; border-radius:5px;")
        temp3 <- ifelse(safe_as_numeric(ssTest()[i,5])<=0.05,'yes','no')
        temp <- cbind(temp1,temp2, temp3)
        npRes <- rbind(npRes,temp)
      }
    } else {
      #Ungrouped data
      statM <- ifelse(input$paratestType=='nonpara', 'medians', 'means')
      pState <- ifelse(safe_as_numeric(ssTest()$`P Value`)<=0.05,'a','no')
      pState2 <- ifelse(safe_as_numeric(ssTest()$`P Value`)<=0.05,' of at least two conditions within the',' of conditions within the')
      pStyle <- ifelse(safe_as_numeric(ssTest()$`P Value`)<=0.05,'#B9EBDE','#F7D7D7')
      temp1 <- paste0("There is ",pState," significant difference between ",statM, 
                      pState2," groups.")
      temp2 <- paste0("padding:5px; background-color:",pStyle,
                      "; border: 1px solid; border-radius:5px;")
      temp3 <- ifelse(safe_as_numeric(ssTest()$`P Value`)<=0.05,'yes','no')
      npRes <- data.frame(temp1,temp2, temp3)
    }
    
    return(npRes)
  })
  #Combining Omnibus and Post-Hoc test report for displaying
  sigRepContent <- reactive({
    req(sigRepTit())#important
    
    titles <- sigRepTit()
    npResult <- lapply(1:nrow(npResult()),function(i){
      p(npResult()[i,1],style = npResult()[i,2])
    })
    if (input$paratestType == 'para'){
      #Parametric data
      tagList(
        #Levene's test
        p("Levene's test report for equal variance", style = "font-weight:bolder;"),
        tableOutput('leveVarTab'),
        
        if (input$ttestType == 'tSt'){
          #Two samples test
          tagList(
            p(titles[1], style = "font-weight:bolder;"),
            tableOutput('tsTestTab')
          )
        } else {
          #Several samples test
          if (isTRUE(input$askComp)){
            tagList(
              #Omnibus test
              p(titles[2], style = "font-weight:bolder;"),
              tableOutput('ssTestTab'),
              npResult,
              br(),
              #Post hoc test
              p(titles[5],style='font-weight:bolder'),
              tableOutput('phTestTab')
            )
          } else {
            tagList(
              #Only omnibus test
              p(titles[2], style = "font-weight:bolder;"),
              tableOutput('ssTestTab'),
              npResult
            )
          }
        }
      )
    } else {
      #Nonparametric data
      if (input$ttestType == 'tSt'){
        # Two-sampled test
        tagList(
          #Two-samples test
          p(titles[3], style = "font-weight:bolder;"),
          tableOutput('tsTestTab')
        )
      } else {
        #Several sampled test
        if (isTRUE(input$askComp)){
          tagList(
            #Omnibus test
            p(titles[4], style = "font-weight:bolder;"),
            tableOutput('ssTestTab'),
            npResult,
            br(),
            #Post hoc test
            p(titles[5], style = "font-weight:bolder;"),
            tableOutput('phTestTab')
          )
        } else{
          tagList(
            #Only omnibus test
            p(titles[4], style = "font-weight:bolder;"),
            tableOutput('ssTestTab'),
            npResult
          )
        }
      }
    }
  })
  
  ## Significance Table Outputs ##
  
  output$descStatTab <- renderTable({
    req(descStat())#important
    df <- descStat()
    shinyWidgets::show_toast(
      title= "Analysis complete!", type = "success", timer = 2000)
    #Filtering comparisons when Control vs Groups are selected
    if (input$ttestType == 'tSt'  && ncol(data()) != length(input$statTwoCol)){
      df <- df |> dplyr::filter(Condition %in% input$statTwoCol)
    }else {
      df <- df
    }
    return(df)
  }, striped = T, width = '100%', align = 'l') |> bindEvent(input$runAnalysisFinal)
  
  output$leveVarTab <- renderTable({
    return(levTest())
  }, striped = T, width = '100%', align = 'l')  |> bindEvent(input$runAnalysisFinal)
  
  output$ssTestTab <- renderTable({
    return(ssTest())
  }, striped = T, width = '100%', align = 'l') |> bindEvent(input$runAnalysisFinal)
  
  output$tsTestTab <- renderTable({
    return(tsTest())
  }, striped = T, width = '100%', align = 'l')  |> bindEvent(input$runAnalysisFinal)
  
  output$phTestTab <- renderTable({
    if(isTRUE(input$dataGroup)){
      return(do.call(rbind,phTestG()))
    } else{
      return(phTest())
    }
    
  }, striped = T, width = '100%', align = 'l')  |> bindEvent(input$runAnalysisFinal)
  
  output$effSizeTab <- renderTable({
    effSize()
  }, striped = T, width = '100%', align = 'l')  |> bindEvent(input$runAnalysisFinal)
  
  ## Statistical Report Displaying Accordions Rendering options ##
  
  output$StatAccordion <- renderUI({
    accordion( multiple = T, class = 'statAcc', open = T,
               p(testSumm(), style = "font-weight:bolder; padding: 10px;
          border-radius:5px; border:2px solid; font-size:15px;"),
               accordion_panel(
                 title = "Statistical Significance Test",
                 uiOutput('compSigReport')
               ),
               accordion_panel(
                 title = "Sample Effect Size",
                 tableOutput('effSizeTab')
               ),
               accordion_panel(
                 title = "Normality Test",
                 p("Shapiro-Wilk normality test report", style = 'font-weight:bolder;'),
                 tableOutput('normStatTab'),
                 actionButton('showQQ', 'Show QQ Plot'),
                 #Copies normality report to clipboard
                 rclipButton('normCopy', label = 'Copy Result',
                             clipText = format_tsv(normTestTable()))
               ),
               accordion_panel(
                 title = "Descriptive Statistics",
                 tableOutput('descStatTab'),
                 #Copies Summary statistics report to clipboard
                 rclipButton('descCopy', label = 'Copy Result',
                             clipText = format_tsv(descStat()))
               )
    )
  }) |> bindEvent(input$runAnalysisFinal)
  
  observeEvent(input$normCopy,{
    shinyWidgets::show_toast(title = "Data table copied to clipboard!",
                             type = c("success"),
                             timer = 2000)
  })
  observeEvent(input$descCopy,{
    shinyWidgets::show_toast(title = "Data table copied to clipboard!",
                             type = c("success"),
                             timer = 2000)
  })
  output$compSigReport <- renderUI({
    sigRepContent()
  }) |> bindEvent(input$runAnalysisFinal)
  
  ## Generates comparison group list virtual-select input for significance bracket addition
  observeEvent(list(input$runAnalysisFinal, input$exampleFile),{
    req(data(), nrow(data()) > 0)
    if (input$ttestType=='sSt'){
      if(isTRUE(input$dataGroup)){
        p <- do.call(rbind,phTestG())
        pattern <- paste(unique(orderdata()$variable),collapse = '|')
        p <- p |> rowwise() |> 
          dplyr::filter(str_detect(Comparison, pattern)) 
        grps <- p
        
      }else{
        grps <- phTest()
      }
    }else{
      grps <- tsTest()
    }
    
    if(isTruthy(input$exampleFile) && !is.null(data())){
      # Pre-selects comparisons groups when demo data loaded
      if (isTRUE(input$dataGroup)){
        first <- as.character(grps[c(1, 12, 23), 1, drop = TRUE])
      }else{
        first <- grps[c(1, 3, 5), 1]
      }
    } else {
      # Pre-selects first option 
      first <- as.character(grps[1, 1, drop = TRUE])
    }
    #Virtual selection input rendering option
    output$statGroups <- renderUI({
      virtualSelectInput('grplist',
                         label = 'Select Groups',
                         choices = grps[,1],
                         multiple = T,
                         selected = first,
                         showDropboxAsPopup = TRUE,
                         popupDropboxBreakpoint = "3000px", 
                         width = "100%",
                         dropboxWrapper = "body",
                         autoSelectFirstOption = TRUE,
                         showSelectedOptionsFirst = TRUE)
    })
    ## Opens P value annotation related accordion panel in graph tab when 
    # Statistical analysis is performed
    observeEvent(list(input$runAnalysisFinal, input$exampleFile),{
      accordion_panel_set(id = "genAcc",
                          values = "annotePanel",
                          session = session)
    })
    # observe({
    #   req(input$askPvalType == 'both')
    #   if(input$pvalVpos == 'bottom'){
    #   updateNoUiSliderInput(session, 'pvalTVpos', value = 100 )}
    # })
    
    # Rendering options for Statistical report download button
    output$statDnld <- renderUI({
      if(isTRUE(input$dataGroup)){
        checkColon <- has_element(str_detect(colnames(data()),':'),FALSE)
        validate(need(isFALSE(checkColon), ""))
      }
      tagList(
        downloadButton(
          'statReport',
          'Download Stat Report',
          icon= icon('file-arrow-down'),
          class = "btn-primary"
        ),
        p("&#9888;Add Significance Brackets and P Value to Your Plot from 'Graph' Tab&#9888;",
          style="color:#999; width:100; text-align:center")
      )
      
    })
  }) 
  
  # Disable or enable Adding Significance Bracket button based on whether at least 
  # one option is selected
  observeEvent(input$grplist, {
    if (length(input$grplist) < 1) {
      shinyjs::disable("addBrackets")
    } else {
      shinyjs::enable("addBrackets")
    }
  }, ignoreNULL = FALSE)
  
  
  ### Significance brackets processing ###
  
  #P value horizontal position over bracket
  pvalHalign <- reactive({
    if(input$pvalHpos=='left'){
      h <- 0
    }else if (input$pvalHpos=='right'){
      h <- 1
    } else{
      h <- 0.5
    }
  })
  
  ### Processing statistics bracket and p value text positioning ###
  
  ## Grouped Data mapping
  
  dfGmap <- reactive({
    req(isTRUE(input$dataGroup))
    checkColon <- has_element(str_detect(current_colnames(),':'),FALSE)
    
    if (isFALSE(checkColon)){
      
      temp <- str_split_fixed(current_colnames(), ':', 2)
    } else {
      show_alert(
        title = "Invalid Column Header",
        text = "Column header missing `:`.",
        type = 'error')
      return(NULL)
    }
    
    row.names(temp) <- NULL
    if(isTRUE(input$grpSwitch)){
      colnames(temp) <- c('Conditions','Groups')
      colFact <- unique(temp[,1])
      fillFact <- unique(temp[,2])
    } else { 
      colnames(temp) <- c('Groups','Conditions')
      fillFact <- unique(temp[,1])
      colFact <- unique(temp[,2])
    }
    if(isTRUE(input$reverseX)){
      fillFact <- rev(fillFact)
      colFact <- rev(colFact)
    }
    x_axis_col <- gsub('.', ' ', colFact, fixed = TRUE)
    
    x_axis <- c(factor(orderdata()$variable, levels = colFact,
                       labels = x_axis_col))
    x <- input$innerDistVio/100
    
    
    n <- length(fillFact)
    dodge_x <- function(x_base, n, x){
      start <- x_base - (x / 2)
      end   <- x_base + (x / 2)
      slotW <- x / n
      
      seq(from = start + (slotW / 2), 
          to = end - (slotW / 2), 
          length.out = n)
    }
    s <- dodge_x(x_base = 1, n = n, x = x)
    cc <- setNames(s,gsub('[.]',' ',fillFact))
    cp <- setNames(safe_as_numeric(unique(x_axis)),gsub('[.]',' ',unique(x_axis)))
    ccp <- list(cc,cp, colFact, fillFact)
    return(ccp)
  })
  
  ## Storing column names for validation
  stored_status <- reactiveVal(FALSE)
  observeEvent(data(),{
    stored_status(FALSE)
  })
  observeEvent(input$dataGroup,{
    stored_status(FALSE)
  })
  observe({
    req(input$runAnalysisFinal)
    stored_status(TRUE)
  })
  
  ### Main calculation for XY coordinates and P value text for displaying ###
  
  statBrackets <- reactive({
    req(input$grplist, data(),
        isTRUE(stored_status()), isTRUE(input$askComp)) ## Very Important
    
    #Make 2 groups from selected comparison list
    groups <- as.data.frame(str_split(input$grplist,' vs '))
    
    groups <- as.data.frame(t(groups))
    rownames(groups) <- NULL
    
    removeStr <- c("*", "<br>", "<sub>", "<sup>", "</sub>", "</sup>")
    
    # Subset only if selectedCols exist and are valid
    newColNames <- current_colnames()
    selected <- input$selectedCols %||% newColNames
    newColNames <- intersect(selected, newColNames)
    
    #Removing unnecessary characters from columns name
    for (i in 1:length(newColNames)) {
      for (pattern in removeStr){
        newColNames[i] <- gsub(pattern, "", newColNames[i], fixed = TRUE)
      }
      newColNames[i] <- gsub('.', " ", newColNames[i], fixed = TRUE)
      newColNames[i] <- gsub('-', " vs ", newColNames[i], fixed = TRUE)
      newColNames[i] <- gsub(' - ', " vs ", newColNames[i], fixed = TRUE)
    }
    #reverses column name when X axis is reversed
    if (isTRUE(input$reverseX)){
      lv <- rev(newColNames)
    } else {
      lv <- newColNames
    }
    # X and Y coordinate finds of the groups selected to draw bracket lines
    if(isTRUE(input$dataGroup)){
      # Complex calculation for grouped data XY coordinates
      colFact <- dfGmap()[[3]] #grouping parameter
      fillFact <- dfGmap()[[4]] #parameters per groups
      # Removing unnecessary characters from group's name
      for (i in 1:length(colFact)) {
        for (pattern in removeStr){
          colFact[i] <- gsub(pattern, "", colFact[i], fixed = TRUE)
        }
        colFact[i] <- gsub('.', " ", colFact[i], fixed = TRUE)
      }
      for (i in 1:length(fillFact)) {
        for (pattern in removeStr){
          fillFact[i] <- gsub(pattern, "", fillFact[i], fixed = TRUE)
        }
        fillFact[i] <- gsub('.', " ", fillFact[i], fixed = TRUE)
      }
      # Get the Post hoc test report
      listp <- do.call(rbind,phTestG())
      # Match user selected comparison list from the test report and get the row indexes 
      listid <- match(input$grplist,listp[,1])
      
      # Get the row ID, p value, and significance text of the selected comparisons
      p <- str_split_fixed(listp[,1],' vs ',2)|> as.data.frame()
      p <- p  |> mutate(pval = safe_as_numeric(listp[,c(ncol(listp)-1)]), 
                        text = listp[,c(ncol(listp))], id = seq_along(1:nrow(p)))
      # Filtering only the user-selected comparisons
      p <- p[listid,]  
      # p <- p |> mutate(V1 = gsub('.', ' ', V1), V2 = gsub('.', ' ', V2))
      
      cc <- dfGmap()[[1]] #partial X axis coordinate of parameters per group (e.g., 0.7, 0.9, 1.1, 1.3)
      cp <- dfGmap()[[2]] #X axis coordinates of groups (e.g., 1, 2...)
      ccp <- c(cc,cp)
      # Removing unnecessary characters
      for (i in 1:length(ccp)) {
        for (pattern in removeStr){
          names(ccp)[i] <- gsub(pattern, "", names(ccp)[i], fixed = TRUE)
        }
        names(ccp)[i] <- gsub('.', " ", names(ccp)[i], fixed = TRUE)
      }
      #Complete X-axis coordinates of all parameters per group
      ccpL_df <- outer(cp - 1, cc, `+`) |> as.data.frame() |> 
        t()  # to map 'mx' (below)
      ccpL <- ccpL_df |> as.vector() |> sort()
      ccpL <- c(ccpL,cp)
      # Removing unnecessary characters
      for (i in 1:length(ccpL)) {
        for (pattern in removeStr){
          names(ccpL)[i] <- gsub(pattern, "", names(ccpL)[i], fixed = TRUE)
        }
        names(ccpL)[i] <- gsub('.', " ", names(ccpL)[i], fixed = TRUE)
      }
      # Final data frame of left and right coordinates of the brackets
      left <- data.frame()
      right <- data.frame()
      
      # Assigns selected groups with numerical X coordinates from ccp (before)
      for (i in 1:nrow(p)){
        if(isFALSE(str_detect(p[i,1],',')) && isFALSE(str_detect(p[i,2],','))){
          temp <- ccp[p[i,1]] |> unname() |> safe_as_numeric() 
          left <- rbind(left,temp)
          temp <- ccp[p[i,2]] |> unname() |> safe_as_numeric() 
          right <- rbind(right,temp) 
        } else {
          #Removes ',' from effect group names
          nL <- str_split(p[i,1],', ')|> unlist() 
          nR <- str_split(p[i,2],', ')|> unlist() 
          temp <- ccp[nL[2]]-1+ccp[nL[1]]
          left <- rbind(left,temp)
          temp <- ccp[nR[2]]-1+ccp[nR[1]]
          right <- rbind(right,temp)
        }
      }
      # Combines left and right coordinates with P value and
      # significance text in one data frame
      pnn <- data.frame(left, right, p[,3], p[,4])
      colnames(pnn) <- c('x','xend','pval','text')
      rownames(pnn) <- NULL
      
    }else{
      # Calculation for ungrouped data XY coordinates
      colnames(groups) <- c('left', 'right')
      left <- groups$left |> factor(levels = lv ) 
      right <- groups$right |> factor(levels = lv)
    }
    
    tipL <- 0.02 #assigns initial tip length for vertical lines of brackets
    gap <- input$gapWidth/1000 #gap between two brackets in one row
    
    # Calculating the long bracket's vertical end position
    if (isFALSE(input$askJitter)){
      # If data points are present (Bar or Jitter plot)
      if(input$askPlotTypeII=="bar"){
        # If Bar plot type is selected as the bar height may have different forms,
        # Mean, Median heights with or without error-bars
        #Std Error of mean calculation function
        sem <- function(x, na.rm = TRUE){
          if(na.rm) x <- x[!is.na(x)]
          sd(x) / sqrt(length(x))
        }
        #Confidence of interval (95%) calculation function
        ci95 <- function(x){
          x <- na.omit(x)
          # n <- length(x)
          # ci <- lapply(x,qt(p=0.05/2,df = n-1,lower.tail=F)*(sd(x)/sqrt(n))) |> as.data.frame()
          # xc <- lapply(x,t.test)
          # ci <- data.frame()
          # for (i in 1:length(x)){
          #   # temp <- xc[[i]]$`conf.int`[1]
          #   temp <- qt(p=0.05/2,df = n-1,lower.tail=F)*(sd(x[,i])/sqrt(n))
          #   ci <- rbind(ci,temp)
          # }
          # ci <- t(ci) |> as.data.frame()
          nMax <- data.frame()
          for (i in 1:ncol(x)){
            y <- x[,i]
            outl <- boxplot.stats(y)$out # not foolproof
            temp <- y[!unlist(y) %in% outl]
            nMax <- rbind(nMax,max(temp))
          }
          ci <- nMax |> t() |> as.data.frame()
          # out
          # iqr <- lapply(x,IQR) |> as.data.frame()
          # qnt75 <- sapply(x, quantile)[4,] |> as.data.frame() |> t() |> as.data.frame()
          # ci <- qnt75+(1.5*iqr)
          rownames(ci) <- NULL
          colnames(ci) <- newColNames
          return(ci)
        }
        if (input$barFunc == 'mean'){
          #Bar height mean
          if (input$sum_typeBarMean=='mean_only'){
            #No error bars
            mx <- apply(data(), MARGIN=2, FUN=mean, na.rm = TRUE) |> data.frame() |> t() 
          } else if (input$sum_typeBarMean=='mean_sd'){
            #SD error bars
            mx <- (apply(data(), MARGIN=2, FUN=sd, na.rm = TRUE) + 
                     apply(data(), MARGIN=2, FUN=mean, na.rm = TRUE)) |> data.frame() |> t()
          } else{
            #SEM error bars
            mx <- (apply(data(), MARGIN=2, FUN=sem, na.rm = TRUE)+
                     apply(data(), MARGIN=2, FUN=mean, na.rm = TRUE)) |>
              data.frame() |> t()
          }
        } else {
          #Bar height median
          if (input$sum_typeBarMedian=='median_only'){
            #No error bars
            mx <- apply(data(), MARGIN=2, FUN=median, na.rm = TRUE) |> data.frame() |> t()
            
          } else{
            #CI95 error bars
            # mx <- (ci95(data())*(1.5/sqrt(length(data())))+apply(data(), MARGIN=2, FUN=median))  
            mx <- ci95(data())
          }
        }
      }else{
        # If Jitter plot type is selected. Positions above max data point
        mx <- apply(data(), MARGIN=2, FUN=max, na.rm = TRUE) |> data.frame() |> t()
      }
    }else{
      # For every other plots
      mx <- apply(data(), MARGIN=2, FUN=max, na.rm = TRUE) |> data.frame() |> t()
    }
    
    row.names(mx)<-NULL
    mx <- data.frame(mx) #Maximum point/ tip based on shape
    #Adds group names to each mx value per column
    #For ungrouped data
    mx <- setNames(mx,newColNames)
    if(isTRUE(input$dataGroup)){
      #For grouped data
      longmx <- as.data.frame(mx) |> pivot_longer(names_to =c('group','variable'),
                                                  names_sep = ':', cols = everything(), values_to = 'value')
      longmx <- longmx |> pivot_longer(cols = c(group, variable), names_to = "type",
                                       values_to = "label") |> select(label, value)
      longmx_avg <- longmx |> dplyr::filter(label %in% colFact) |> group_by(label) |>
        dplyr::summarize(avg_val = mean(value))
      mx <- setNames(c(mx,longmx_avg$avg_val),ccpL)
    }  
    #Reverses mx values if X axis is reversed
    if (isTRUE(input$reverseX)){
      mx <- rev(mx)
    }else{
      mx <- mx
    }
    
    ### Y-coordinates calculation
    first <- (1+(input$firstBrack-50.5)/500) #Adjusting the first layer position
    base_y <- yaxisMax()*first #Base Y axis coordinate
    
    #Basic data frame to handle the bracket coordinates
    if(isTRUE(input$dataGroup)){
      statup <- data.frame(x = safe_as_numeric(unlist(pnn$x)),
                           xend = safe_as_numeric((unlist(pnn$xend))),
                           y = (base_y+(base_y*first)),
                           yend = (base_y+(base_y*first)))
    } else{
      statup <- data.frame(x = safe_as_numeric(unlist(left)),
                           xend = safe_as_numeric((unlist(right))),
                           y = (base_y+(base_y*first)),
                           yend = (base_y+(base_y*first)))
    }
    
    #To order the lowest x on left side always
    statup <- statup |> mutate(x_min=pmin(x,xend),x_max=pmax(x,xend)) |>
      dplyr::select(-x,-xend) |> rename(x = x_min, xend = x_max) |>
      dplyr::select(x, xend,everything())
    
    #difference between x and xend
    statup$diff <- statup$xend-statup$x
    
    # P value processing
    star4 <- paste0(rep('*', 4), collapse = "")
    star3 <- paste0(rep('*', 3), collapse = "")
    
    if(isTRUE(input$dataGroup)){  
      pnn_text <- pnn$`text`
      pnn_p <- pnn$`pval`
    }
    
    results_list <- list()
    for (i in 1:length(input$grplist)) {
      # current_tab <- if(isTRUE(input$dataGroup)) listp else current_tab
      # if(isTRUE(input$dataGroup)){
      #   n <- length(pnn_text)
      # } else {
      if(isFALSE(input$dataGroup)){
        # Get the two sample or several samples test result for ungrouped data
        current_tab <- if (input$ttestType == 'tSt') tsTest() else phTest()
        # Match the selected groups with the report and get the row index
        n <- match(input$grplist[i], current_tab[, 1])
      }
      # } 
      
      ## For only asterisk display ##
      t_val <- ""
      # Get the significance text value from filtered report
      if(isTRUE(input$dataGroup)){
        t_val <- pnn_text[[i]]
      }else {
        req(!is.na(n))
        t_val <- as.character(current_tab[n, ncol(current_tab)])
      }
      
      # For APA and NEJM styles, cap at 3 stars
      if (input$askPvalStyle != 'default') {
        if (t_val == star4) {
          t_val <- star3
        } else {
          t_val <- t_val
        }
      }
      
      ## For raw p value display ##
      
      # Get the p value from filtered report
      if(isTRUE(input$dataGroup)){
        p_raw <- safe_as_numeric(pnn_p[[i]])
      } else{
        p_raw <- current_tab[n, (ncol(current_tab) - 1)]
        p_raw <- ifelse (p_raw == "< 2.2e-16", safe_as_numeric(2.2e-16), safe_as_numeric(p_raw))
      }
      # Zero before point (APA has none, NEJM/Default has)
      s_prefix <- if (input$askPvalStyle == 'apa') "" else "0"
      ns_p <- if (p_raw>=1) p_raw else gsub('0.','.',p_raw)
      
      #Keep raw p value or convert them to APA or NEJM format
      if (input$askPvalStyle == 'default') {
        raw_val <- paste0('P = ', p_raw)
      } else {
        # APA and NEJM
        if (p_raw <= 0.001) {
          raw_val <- paste0("<i>p</i> < ", s_prefix, ".001")
        } else if (p_raw <= 0.01) {
          raw_val <- paste0("<i>p</i> < ", s_prefix, ".01")
        } else if (p_raw <= 0.05) {
          raw_val <- paste0("<i>p</i> < ", s_prefix, ".05")
        } else {
          s_prefix <- if(p_raw == 1) "" else s_prefix
          raw_val <- paste0("<i>p</i> = ", s_prefix, ns_p)
        }
      }
      
      if (input$askPvalType == 'star') {
        # Asterisks only show
        p_val <- t_val
      } else if (input$askPvalType == 'raw'){
        # Raw P value only show
        p_val <- raw_val
      } else {
        # Both combined show
        if(input$pvalVpos=='top'){
          #Over the bracket
          p_val <- paste(raw_val,'<br>',t_val, sep = '')
        } else {
          #under the bracket
          p_val <- paste(t_val,'<br>',raw_val, sep = '')
        }
        
      }
      # Change the asterisk from general 5 point to HTML 6 point character
      if (input$askPvalType == 'star' || input$askPvalType == 'both' ){
        p_val <- gsub("*",intToUtf8(0x2731), p_val, fixed = TRUE)
      }
      # adjust vertical height of ns and p value differently
      if(p_val=='ns' || isTRUE(str_detect(p_val,'P')) || isTRUE(str_detect(p_val, 'p'))){
        pVjust <- seq(from = 0, to = 0.75, length.out = 100)
      } else {
        pVjust <-  seq(from = 0.5, to = 1, length.out = 100)
      }
      #Combine p value, significance text, their vertical pos in one data frame
      results_list[[i]] <- data.frame(text = paste0('<b>',p_val,'</b>'),
                                      size = input$pvalSize/5,
                                      vjust = pVjust[input$pvalTVpos],
                                      stringsAsFactors = FALSE)
    }
    
    # Combine all results into the final data frame
    text <- do.call(rbind, results_list)
    
    colnames(text) <- c('text','size','vjust')
    # Combine the left, right coordinate data frame with the p value data frame
    statup <- cbind(statup,text)
    
    #Spreading a sequence between x and xend rowwise
    statup <- statup |> arrange(diff) |> rowwise() |> 
      mutate(seqq = list(seq(x, xend))) |>  ungroup() |> arrange(diff)
    
    #creating a large layers
    max_layers <- 30                              
    last_end <- rep(-Inf, max_layers) 
    
    # track last end time per layer
    statup$layer <- NA_integer_
    
    #assigning the y and yend to different layers
    for (i in seq_len(nrow(statup))) {
      for (l in seq_len(max_layers)) {
        if (last_end[l] <= statup$x[i]){
          statup$layer[i] <- l
          last_end[l] <- statup$xend[i]
          break
        }
      }
    }
    statup <- statup |> arrange(layer)
    
    #Adjusting the gaps between brackets
    factorD <- apply(data(), MARGIN=2, FUN=median, na.rm = TRUE) |> max() |> safe_as_numeric()
    step_size  <- (input$distWidth/100)*factorD
    
    # Converting layers to y-coordinates
    statup <- statup |> 
      mutate(y = base_y + (layer - 1) * step_size,
             yend = y)
    
    statup <- statup[,-c(9,10)] #removing unnecessary columns
    
    #Adding bracket vertical line Y coordinates
    #Left side
    if (isTRUE(input$dataGroup)){
      sL <- data.frame()
      for (i in 1:length(statup$x)){
        temp <- mx[[as.character(statup$x[i])]]+(mx[[as.character(statup$x[i])]])*tipL
        sL <- rbind(sL,temp)
      }
    } else {
      sL <- t(mx[statup$x])+(t(mx[statup$x])*tipL)
    }
    rownames(sL) <- NULL
    colnames(sL) <- 'yendL'
    #Right side
    if (isTRUE(input$dataGroup)){
      sR <- data.frame()
      for (i in 1:length(statup$x)){
        temp <- mx[[as.character(statup$xend[i])]]+(mx[[as.character(statup$xend[i])]])*tipL
        sR <- rbind(sR,temp)
      }
    } else {
      sR <- t(mx[statup$xend])+(t(mx[statup$xend])*tipL)
    }
    rownames(sR) <- NULL
    colnames(sR) <- 'yendR'
    #Combine with main data frame
    statup <- cbind(statup,sL,sR)
    
    # Text position
    if (input$pvalVpos=='top'){
      #  Over bracket
      statup$yT <- statup$y+statup$y*0.03 |> safe_as_numeric()
    } else {
      # Under bracket
      statup$yT <- statup$y-statup$y*0.03 |> safe_as_numeric()
    }
    # Text and p value horizontal alignment
    if (input$pvalHpos=='left'){
      # Left align
      if (isTRUE(input$flipPlot)){
        statup$xT <- safe_as_numeric(statup$xend)-0.05
      } else{
        statup$xT <- safe_as_numeric(statup$x)+0.05
      }
      
    } else if (input$pvalHpos=='right'){
      # Right align
      if (isTRUE(input$flipPlot)){
        statup$xT <- safe_as_numeric(statup$x)+0.05
      } else {
        statup$xT <- safe_as_numeric(statup$xend)-0.05
      }
      
    } else {
      #Central align (default)
      statup$xT <- apply(data.frame(safe_as_numeric(statup$x),safe_as_numeric(statup$xend)),
                         MARGIN=1,FUN=mean) |> safe_as_numeric()
    }
    
    # Rearranging rows based on original groups
    colnames(groups) <- c('x','xend')
    groups$x <- safe_as_numeric(groups$x)
    groups$xend <- safe_as_numeric(groups$xend)
    statup <- statup |> left_join(groups, by = c('x','xend')) 
    
    # Function to calculate bracket vertical line length or the end point
    endP <- function(y,diff,list){
      for(i in 1:length(list)){
        if (i-1!=0){
          if(y[i]==min(y)){
            list[i] <- list[i]
          }else {
            list[i] <- y[i-1]+y[i-1]*tipL
          }
        } 
      }
      return(list)
    }
    # Adjusting the vertical line end point and adding to main data frame for long brackets
    statup <- statup |> group_by(x) |>
      arrange(y) |>
      mutate(yendL=endP(y=y,diff=diff,list=yendL)) |>
      ungroup() |> 
      group_by(xend) |> 
      arrange(yend) |> 
      mutate(yendR=endP(y=y,diff=diff,list=yendR)) |> 
      ungroup()
    
    # Finer adjustment of tip length when layers are present
    # (never crosses lower layer bracket)
    for (i in 1:nrow(statup)){
      for (j in 1: nrow(statup)){
        if (statup[i,1]>statup[j,1]&&statup[i,1]<statup[j,2]){
          if(statup[i,3]>statup[j,3] && statup[i,5]==statup[j,5]){
            statup[i,9] <- statup[j,3]+statup[j,3]*tipL
          }
        }
        if (statup[i,2]>statup[j,1]&&statup[i,2]<statup[j,2]){
          if(statup[i,3]>statup[j,3]&& statup[i,5]==statup[j,5]){
            statup[i,10] <- statup[j,4]+statup[j,4]*tipL
          }
        }
      }
    }
    
    #Reducing x and xend for gaps if x and xend are same and on the same level
    for (i in 1:length(statup$x)){
      for (j in 1:length(statup$xend)){
        if (statup[i,1]==statup[j,2]){
          statup[i,1] <- statup[i,1]+gap
          statup[j,2] <- statup[j,2]-gap
        }
      }
    }
    
    if(input$askTipType == 'long'){
      return(statup)
    } else if (input$askTipType == 'short'){
      # Adjust tip length when short bracket is selected
      tipS <- input$tipLength/1000
      statup <- statup |> mutate(yendL=y-(y*tipS)) |> mutate(yendR=yend-(yend*tipS))
      return(statup)
    } else{
      # For Line only brackets
      statup <- statup |> mutate(yendL=y) |> mutate(yendR=yend)
      return(statup)
    }
  })
  
  ## Calculating top margin for main plot
  plotTopM <- reactive({
    if(isFALSE(input$flipPlot)){
      if (!isTruthy(input$runAnalysisFinal)) {
        return(50)
      }
      df <- tryCatch(segAdd(), error = function(e) NULL)
      
      if (is.null(df) || nrow(df) == 0) {
        return(50) # Default margin
      } else {
        layers <- length(unique(df$y))+1
        return((input$topMargin + layers * 8))
      }
    }else{
      return(50)
    }
  })
  #Calculating right margin for main plot when axis is flipped
  plotRightM <- reactive({
    if(isTRUE(input$flipPlot)){
      if (!isTruthy(input$runAnalysisFinal)) {
        return(50)
      }
      df <- tryCatch(segAdd(), error = function(e) NULL)
      
      if (is.null(df) || nrow(df) == 0) {
        return(50) # Default margin
      } else {
        layers <- length(unique(df$y))+1
        return((20 + layers * 25))
      }
    }else{
      return(50)
    }
  })
  #Reactive value addition for Adding Significance Brackets Button (On/ Off)
  btn_val <- reactiveVal(FALSE)
  
  observeEvent(input$addBrackets,{
    btn_val(!btn_val())
    if(btn_val()==FALSE){
      updateActionButton(session,'addBrackets','Add Brackets to Plot')
    }else {
      updateActionButton(session,'addBrackets','Remove Brackets')
    }
  })
  observeEvent(input$exampleFile,{
    btn_val(TRUE)
  })
  
  segAdd <- reactive({
    req(statBrackets())
    if(isFALSE(btn_val())){
      #Removes all brackets
      return(statBrackets()*0)
    }else{
      return(statBrackets())
    }
  })
  
  
  # Conditional content for Stat panel
  output$stat_main_content <- renderUI({
    if (!isTruthy(input$submitFile) && !isTruthy(input$pasteBtn) && !isTruthy(input$exampleFile)) {
      # No data uploaded or empty data
      div(
        style = "display: flex; justify-content: center; align-items: center;
          height: 60vh; flex-direction: column; text-align: center; color: #856404;
          background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 10px;
          padding: 30px; margin: 20px; z-index:100;",
        icon("exclamation-triangle", style = "font-size: 60px; margin-bottom: 20px; color: #f39c12;"),
        h3(style = "color: #d35400; margin: 0 0 10px 0;", "No data to run statistical analysis"),
        p(style = "font-size: 18px; max-width: 600px;",
          "Please go to the ", strong("File Upload"), " tab,",
          "upload an Excel file, select columns, and click ", strong("Upload Datasheet"), " or Upload Pasted Data to begin.")
      )
    }else if (input$dataGroup == T) {
      col_names <- colnames(data())  
      checkColon <- has_element(str_detect(col_names,':'),FALSE)
      
      if (isTRUE(checkColon)) {
        div(
          style = "display: flex; justify-content: center; align-items: center;
                height: 60vh; flex-direction: column; text-align: center; color: #856404;
                background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 10px;
                padding: 30px; margin: 20px; z-index:100;",
          icon("exclamation-triangle", style = "font-size: 60px; margin-bottom: 20px; color: #f39c12;"),
          h3(style = "color: #d35400; margin: 0 0 10px 0;", "Incompatible Data Format"),
          p(style = "font-size: 18px; max-width: 600px;",
            "For ", strong("Grouped Data"), " mode, column headers must follow the format: ",
            strong("Group:Condition"), "(e.g., G1:A, G1:B, G1:C, G2:A, G2:B, G2:C)."),
          br(),
          p(style = "font-size: 14px; max-width: 600px;",
            "See example data for reference", br(),
            "Current headers: ", strong(paste(col_names, collapse = ", ")))
        )
      }else {
        uiOutput('StatAccordion')
      }
    }  else {
      uiOutput('StatAccordion')
    }
  })
  
  ## Workbook preparation for statistical analysis report for downloading
  
  StatReport <- reactive({
    wb <- openxlsx::createWorkbook()
    #Use sheet name (first 15 characters) with the stat report file name
    sheetName <- paste("Stat Report_", substr(input$sheetlist,1,15), collapse = '')
    openxlsx::addWorksheet(wb, sheetName)
    
    csH1 <- createStyle(textDecoration = "bold",
                        fontName = "Arial", 
                        fontSize = 13,
                        fontColour = '#53135C',
                        fgFill = '#ECE4F2',
                        wrapText = T,
                        valign = 'center')
    csH2 <- createStyle(textDecoration = "bold",
                        fontName = "Arial",
                        wrapText = T)
    
    csB <- createStyle(fontColour = "#555555",
                       fontName = "Arial",
                       halign = 'left',
                       valign = 'center',
                       wrapText = T,
                       numFmt = openxlsx_getOp("numFmt", "NUMBER"))
    csRp <- createStyle(fontName = "Arial",
                        fontColour = '#0C5745',
                        bgFill = '#B9EBDE',
                        fgFill = '#B9EBDE',
                        wrapText = T,
                        halign = 'left',
                        valign = 'center')
    csRf <- createStyle(fontName = "Arial",
                        fontColour = '#5E0E0E',
                        bgFill = '#F7D7D7',
                        fgFill = '#F7D7D7',
                        wrapText = T,
                        halign = 'left',
                        valign = 'center')
    row <- 2
    data1 <- req(testSumm())
    
    writeData(wb, sheet = sheetName, x = data1, startRow = row, startCol = 2)
    addStyle(wb, sheet = sheetName,
             style = csH1, rows = row, cols = 2,
             gridExpand = T)
    mergeCells(wb, sheet = sheetName, rows = row, cols = 1:ncol(descStat())+1)
    setRowHeights(wb, sheet = sheetName, rows = row,
                  heights = 50)
    
    ##Levene's Test Report if parametric test
    
    if (input$paratestType == 'para'){
      row <- row+2
      data2_1 <- "Levene's test report for Equal Variance"
      data2_2 <- data.frame(levTest(), check.names = F)
      writeData(wb, sheet = sheetName, x = data2_1, startRow = row, startCol = 2)
      addStyle(wb, sheet = sheetName,
               style = csH1, rows = row, cols = 2,
               gridExpand = T)
      mergeCells(wb, sheet = sheetName, rows = row, cols = 1:ncol(data2_2)+1)
      
      row <- row+1
      writeData(wb, sheet = sheetName, x = data2_2, startRow = row, startCol = 2,
                colNames = T, headerStyle = csH2)
      addStyle(wb, sheet = sheetName,
               style = csB, rows = row+1, cols = 1:ncol(data2_2)+1,
               gridExpand = T)
    }
    titles <- sigRepTit()
    ##Omnibus report
    if (input$paratestType == 'para'){
      row <- row+nrow(data2_2)+ 3
    } else {
      row <- row+ 3
    }
    if(input$ttestType == 'tSt') {
      df <- tsTest()
      dfT <- ifelse(input$paratestType == 'para', titles[1], titles[3])
    } else {
      df <- ssTest()
      dfT <- ifelse(input$paratestType == 'para', titles[2], titles[4])
    }
    data3_1 <- dfT
    data3_2 <- data.frame(df, check.names = F)
    
    writeData(wb, sheet = sheetName, x = data3_1, startRow = row, startCol = 2)
    addStyle(wb, sheet = sheetName,
             style = csH1, rows = row, cols = 2,
             gridExpand = T)
    mergeCells(wb, sheet = sheetName, rows = row, cols = 1:ncol(data3_2)+1)
    
    row <- row+1
    
    writeData(wb, sheet = sheetName, x = data3_2, startRow = row, startCol = 2,
              colNames = T, headerStyle = csH2)
    addStyle(wb, sheet = sheetName,
             style = csB, rows = (row+1):(row+nrow(data3_2)+1), cols = 1:ncol(data3_2)+1,
             gridExpand = T)
    conditionalFormatting(wb, sheetName,
                          cols = ncol(data3_2):ncol(data3_2)+1,
                          rows = (row+1):(row+nrow(data3_2)), 
                          type = "contains",
                          rule = "ns", style = csRf
    )
    conditionalFormatting(wb, sheetName,
                          cols = ncol(data3_2):ncol(data3_2)+1,
                          rows = (row+1):(row+nrow(data3_2)), 
                          type = "notContains",
                          rule = "ns", style = csRp
    )
    
    #Omnibus report summary
    if (input$ttestType == 'sSt'){
      row <- row+nrow(data3_2)+3
      if (isTRUE(input$dataGroup)){
        style <- if (npResult()[1,3] == 'yes') csRp else csRf
        style1 <- if (npResult()[2,3] == 'yes') csRp else csRf
        style2 <- if (npResult()[3,3] == 'yes') csRp else csRf
        data3_3 <- data.frame(npResult()[,1], check.names = F)
      } else {
        style <- if (npResult()[,3] == 'yes') csRp else csRf
        data3_3 <- npResult()[,1]
      }
      
      if (isTRUE(input$dataGroup)){
        writeData(wb, sheet = sheetName, x = data3_3, startRow = row, startCol = 2,
                  colNames = F)
      } else {
        writeData(wb, sheet = sheetName, x = data3_3, startRow = row, startCol = 2)
      }
      
      mergeCells(wb, sheet = sheetName, rows = row, cols = 1:ncol(data3_2)+1)
      addStyle(wb, sheet = sheetName,
               style = style, rows = row, cols = 2,
               gridExpand = T)
      setRowHeights(wb, sheet = sheetName, rows = row,
                    heights = 50)
      if (isTRUE(input$dataGroup)){
        mergeCells(wb, sheet = sheetName, rows = row+1, cols = 1:ncol(data3_2)+1)
        addStyle(wb, sheet = sheetName,
                 style = style1, rows = row+1, cols = 2,
                 gridExpand = T)
        setRowHeights(wb, sheet = sheetName, rows = row+1,
                      heights = 50)
        mergeCells(wb, sheet = sheetName, rows = row+2, cols = 1:ncol(data3_2)+1)
        addStyle(wb, sheet = sheetName,
                 style = style2, rows = row+2, cols = 2,
                 gridExpand = T)
        setRowHeights(wb, sheet = sheetName, rows = row+2,
                      heights = 50)
      }
    }
    
    #Post Hoc Test report
    if (input$ttestType == 'sSt'){
      row <- row+nrow(data3_2)+2
      data4_1 <- titles[5]
      if (isTRUE(input$dataGroup)){
        data4_2 <- data.frame(do.call(rbind,phTestG()), check.names = F)
      }else {
        data4_2 <- data.frame(phTest(), check.names = F)
      }
      
      
      writeData(wb, sheet = sheetName, x = data4_1, startRow = row, startCol = 2)
      addStyle(wb, sheet = sheetName,
               style = csH1, rows = row, cols = 2,
               gridExpand = T)
      mergeCells(wb, sheet = sheetName, rows = row, cols = 1:ncol(data4_2)+1)
      setRowHeights(wb, sheet = sheetName, rows = row,
                    heights = 40)
      
      row <- row+1
      
      writeData(wb, sheet = sheetName, x = data4_2, startRow = row, startCol = 2,
                colNames = T, headerStyle = csH2)
      addStyle(wb, sheet = sheetName,
               style = csB, rows = (row+1):(row+nrow(data4_2)+1), cols = 1:ncol(data4_2)+1,
               gridExpand = T)
      conditionalFormatting(wb, sheetName,
                            cols = ncol(data4_2):ncol(data4_2)+1,
                            rows = (row+1):(row+nrow(data4_2)), 
                            type = "contains",
                            rule = "ns", style = csRf
      )
      conditionalFormatting(wb, sheetName,
                            cols = ncol(data4_2):ncol(data4_2)+1,
                            rows = (row+1):(row+nrow(data4_2)), 
                            type = "notContains",
                            rule = "ns", style = csRp
      )
      row <- row+nrow(data4_2)+2
    } else {
      row <- row+nrow(data3_2)+2
    }
    
    #Effect Size report
    data5_1 <- "Sample Effect Size report"
    data5_2 <- data.frame(effSize(), check.names = F)
    
    writeData(wb, sheet = sheetName, x = data5_1, startRow = row, startCol = 2)
    addStyle(wb, sheet = sheetName,
             style = csH1, rows = row, cols = 2,
             gridExpand = T)
    mergeCells(wb, sheet = sheetName, rows = row, cols = 1:ncol(data5_2)+1)
    row <- row+1
    writeData(wb, sheet = sheetName, x = data5_2, startRow = row, startCol = 2,
              colNames = T, headerStyle = csH2)
    addStyle(wb, sheet = sheetName,
             style = csB, rows = (row+1):(row+nrow(data5_2)+1), cols = 1:ncol(data5_2)+1,
             gridExpand = T)
    row <- row+nrow(data5_2)+2
    
    #Normality test report
    
    data6_1 <- "Shapiro Wilk's normality test report"
    data6_2 <- data.frame(normTest(), check.names = F)
    
    writeData(wb, sheet = sheetName, x = data6_1, startRow = row, startCol = 2)
    addStyle(wb, sheet = sheetName,
             style = csH1, rows = row, cols = 2,
             gridExpand = T)
    mergeCells(wb, sheet = sheetName, rows = row, cols = 1:ncol(data6_2)+1)
    
    row <- row+1
    writeData(wb, sheet = sheetName, x = data6_2, startRow = row, startCol = 2,
              colNames = T, headerStyle = csH2)
    addStyle(wb, sheet = sheetName,
             style = csB, rows = (row+1):(row+nrow(data6_2)+1), cols = 1:ncol(data6_2)+1,
             gridExpand = T)
    
    #Descriptive Stat report
    row <- row+nrow(data6_2)+2
    data7_1 <- "Descriptive test summary"
    data7_2 <- data.frame(descStat(), check.names = F)
    writeData(wb, sheet = sheetName, x = data7_1, startRow = row, startCol = 2)
    addStyle(wb, sheet = sheetName,
             style = csH1, rows = row, cols = 2,
             gridExpand = T)
    mergeCells(wb, sheet = sheetName, rows = row, cols = 1:ncol(data7_2)+1)
    
    row <- row+1
    writeData(wb, sheet = sheetName, x = data7_2, startRow = row, startCol = 2,
              colNames = T, headerStyle = csH2)
    addStyle(wb, sheet = sheetName,
             style = csB, rows = (row+1):(row+nrow(data7_2)+1), cols = 1:ncol(data7_2)+1,
             gridExpand = T)
    
    #Setting Column Width
    setColWidths(wb, sheet = sheetName, cols = 1:(ncol(descStat())+1),
                 widths = 12.2, ignoreMergedCells = T)
    #Save to a temporary file
    tempFile <- tempfile(fileext = ".xlsx")
    openxlsx::saveWorkbook(wb, file = tempFile, overwrite = TRUE)
    
    return(tempFile)
  })
  
  ## Stat Report Download Handler
  output$statReport <- downloadHandler(
    filename = function() {
      paste("Statistic_Report_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy(StatReport(), file)
    }
  )
  
  
  ### Setting saving options ###
  
  #Flatten Input values
  flattenID <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return("")
    } else if (length(x) > 1) {
      # when more than one input ID with similar names are present flatten them with &
      return(paste(x, collapse = "&"))
    } else {
      return(as.character(x))
    }
  }
  ### A comprehensive table to catch inputs, their names and values to save in a excel file ###
  savesetting_df <- reactive({
    req(!is.null(data()), input$savesetting_proxy)
    settings_df <- data.frame(
      Parameter = c(
        # --- File & Data Settings ---
        # "Select Datasheet",
        "Grouped Data",
        "Switch Groups",
        # "Select Columns",
        "Plot Type (Global)",
        "Plot Type (Grouped)",
        "Plot Type (Grouped II)",
        "Plot Type (Main)",
        
        # --- Graph Settings: Box-Whisker ---
        "Add Datapoints (Box)",
        "Mark Outliers",
        "Box Width",
        "Line Width (Box)",
        "Notches",
        "Point Scatter Width (Box)",
        "Point Size (Box)",
        "Shape Similarity (Box)",
        "Point Shape (Box)",
        "Jitter Pattern (Box)",
        "Distance Between Grouped Shapes (Box)",
        
        # --- Graph Settings: Violin ---
        "Add Box-whisker (Violin)",
        "Line Width (Violin)",
        "Box Width (Violin)",
        "Box Color",
        "Custom Box Color",
        "Add Quantile Lines",
        "Quantile Line Width",
        "Trim Ends",
        "Distance Between Grouped Shapes (Violin)",
        
        # --- Graph Settings: Jitter ---
        "Point Scatter Width (Jitter)",
        "Point Size (Jitter)",
        "Shape Similarity (Jitter)",
        "Point Shape (Jitter)",
        "Jitter Pattern (Jitter)",
        "Summary Statistics (Jitter)",
        "Stat Summ Line Width (Jitter)",
        "Stat Summ Bar Width (Jitter)",
        "Stat Summ Line Color (Jitter)",
        "Stat Summ Line Position (Jitter)",
        
        # --- Graph Settings: Raincloud ---
        "Half-Violin Orientation",
        "Adjust Gap",
        "Line Width (Raincloud)",
        "Trim Ends (Raincloud)",
        "Point Scatter Width (Raincloud)",
        "Point Size (Raincloud)",
        "Shape Similarity (Raincloud)",
        "Point Shape (Raincloud)",
        "Summary Statistics (Raincloud)",
        "Stat Summ Linewidth (Raincloud)",
        "Stat Summ Bar Width (Raincloud)",
        "Stat Summ Line Color (Raincloud)",
        "Stat Summ Line Position (Raincloud)",
        
        # --- Graph Settings: Bar ---
        "Box Width (Bar)",
        "Line Width (Bar)",
        "Bar Display",
        "Summary Statistics (Bar Mean)",
        "Summary Statistics (Bar Median)",
        "Stat Summ Linewidth (Bar)",
        "Stat Summ Bar Width (Bar)",
        "Stat Summ Line Color (Bar)",
        "Errorbar Direction",
        "Add Datapoints (Bar)",
        
        # --- Graph Settings: Connecting Lines ---
        "Add Connecting Line",
        "Line Width (Connect)",
        "Line Color (Connect)",
        "Line Type (Connect)",
        
        # --- Plot Area ---
        "Add Grids",
        "Major Grids",
        "Minor Grids",
        "Choose Axis (Grid)",
        "Grids Color",
        "Add Borders",
        "Add Background Color",
        "Plot Background Color",
        "Legend Title",
        "Legend Position",
        "Legend Text Size",
        "Legend Title Size",
        "Legend Key Size",
        "Legend Border Size",
        "Descriptive Info View",
        "Descriptive Info Type",
        "Descriptive Info Size",
        "Descriptive Info Markdown",
        "Descriptive Info Decimal Point",
        "Descriptive Info Position",
        
        # --- Plot Title ---
        "Plot Title",
        "Add Special Characters (Title)",
        "Select Symbol (Title)",
        "Add Markdown (Title)",
        "Select Markdown (Title)",
        "Text Alignment",
        "Vertical alignment",
        "Title Box Width",
        "Title Box Padding",
        "Title Box Border",
        
        # --- Axes ---
        "Y-Axis Title",
        "Add Special Characters (Y)",
        "Add Markdown (Y)",
        "Select Symbol (Y)",
        "Select Markdown (Y)",
        "X-Axis Title",
        "Add Special Characters (X)",
        "Add Markdown (X)",
        "Select Symbol (X)",
        "Select Markdown (X)",
        "X-Axis Rotation",
        "Reverse X-Axis",
        "Y-Axis Log Scale",
        "Y-Axis Number Display",
        "Y-Axis Min",
        "Y-Axis Max",
        # "Y-Axis Break",
        # "Y-Axis Break Min",
        # "Y-Axis Break Max",
        # "Plot Spread Size (Break)",
        # "Gap Distance (Break)",
        "Axes Line Width",
        "Axes Tick Length",
        "Axes Tick Width",
        "Flip Axes",
        
        # --- Fonts ---
        "Select Font Family",
        "Size: Plot Title",
        "Size: Axis Text (X)",
        "Size: Axis Title (X)",
        "Title Linebreak (X)",
        "Size: Axis Text (Y)",
        "Size: Axis Title (Y)",
        "Title Linebreak (Y)",
        
        # --- Theme ---
        "Select Theme Generator",
        "Preset Themes",
        "Gradient Color 1",
        "Gradient Color 2",
        "Select Theme Generator (Grouped)",
        "Display Contrast",
        "Border Color",
        "Border Color (Grouped)",
        "Border Shade",
        "Border Shade (Grouped)",
        "Shape Opacity",
        "Shape Opacity (Grouped)",
        "Datapoint Outline Color",
        "Datapoint Fill Color",
        "DP Gradient Color 1",
        "DP Gradient Color 2",
        "DP Fill Gradient Color 1",
        "DP Fill Gradient Color 2",
        "Datapoint Outline Color (Grouped)",
        "Datapoint Fill Color (Grouped)",
        
        # --- Statistics ---
        # "Select Test Type (T/S)",
        # "Select Test Type (Para/NonPara)",
        # "Paired Samples",
        # "Perform Multiple Comparisons",
        # "Repeated-Measured Samples",
        # "Select Groups (Control Comp)",
        # "Select Group Comparison Type",
        # "Select Groups (Group Comp)",
        # "Correction Method (General)",
        # "Correction Method (Paired)",
        # "Correction Method (Grouped)",
        # "Select Control",
        # "Correction Method (Control)",
        # "Correction Method (Control Grouped)",
        # "Correction Method (Paired Control)",
        # "Correction Method (Paired Control Grouped)",
        # "Select Groups (Two Sample)",
        # "Select Groups (Annotation)",
        "Bracket Type",
        "Significance Report Type",
        "P Value Style",
        "P Value Text Size",
        "P Value Text Vposition",
        "P Value Horizontal Pos",
        "P Value Vertical Pos",
        "P Value Text Color",
        "P Value Area Margin",
        "Vertical Positioning (Bracket)",
        "Inter-bracket Distance",
        "Tip Length",
        "Gap Distance (Bracket)",
        "Line Width (Bracket)",
        "Line Color (Bracket)",
        
        # --- Export/Size ---
        "Plot Width",
        "Plot Height",
        "Lock Ratio",
        "Save as (File Type)",
        "Select Resolution (DPI)"
      ),
      
      inputID = c(
        # --- File & Data Settings ---
        # "sheetlist",
        "dataGroup",
        "grpSwitch",
        # "selectedCols",
        "askPlotType",
        "askPlotTypeG",
        "askPlotTypeIIG",
        "askPlotTypeII",
        
        # --- Graph Settings: Box-Whisker ---
        "boxtype",
        "outlier",
        "boxwidth",
        "linewidthBox",
        "notch",
        "scatterBox",
        "pointsizeBox",
        "pointDistBox",
        "pointshapeBox",
        "pointMethodBox",
        "innerDistBox",
        
        # --- Graph Settings: Violin ---
        "viotype",
        "linewidthVio",
        "boxWidthVio",
        "boxColVio",
        "boxColCust",
        "askQuantLine",
        "quantLineSize",
        "endTrim",
        "innerDistVio",
        
        # --- Graph Settings: Jitter ---
        "scatter",
        "pointsize",
        "pointDist",
        "pointshape",
        "pointMethod",
        "sum_typeJitter",
        "statLine",
        "statWidth",
        "statColour",
        "askSummPos",
        
        # --- Graph Settings: Raincloud ---
        "slabSide",
        "slabDistance",
        "linewidthRain",
        "endTrimRain",
        "scatterRain",
        "pointsizeRain",
        "pointDistRain",
        "pointshapeRain",
        "sum_typeRain",
        "statLineRain",
        "statWidthRain",
        "statColourRain",
        "askSummPosRain",
        
        # --- Graph Settings: Bar ---
        "barwidth",
        "linewidthBar",
        "barFunc",
        "sum_typeBarMean",
        "sum_typeBarMedian",
        "statLineBar",
        "statWidthBar",
        "statColourBar",
        "askSide",
        "askJitter",
        
        # --- Graph Settings: Connecting Lines ---
        "askConnectLine",
        "connectLineSize",
        "connectLineCol",
        "connectLineType",
        
        # --- Plot Area ---
        "plotThemeGrid",
        "majGrid",
        "minGrid",
        "gridOpt",
        "gridCol",
        "plotThemeBorder",
        "plotThemeBg",
        "plotColor",
        "legTitle",
        "legPos",
        "legTextSize",
        "legTitleSize",
        "legSize",
        "legBorderSize",
        "dpview",
        "dpviewInfo",
        "dpviewSize",
        "dpviewMD",
        "dpviewDecmP",
        "dpviewPos",
        
        # --- Plot Title ---
        "plotTitle",
        "chksymbolTit",
        "symbolsTit",
        "markdownTit",
        "mdTit",
        "titlePos",
        "verAlign",
        "bWidthTitle",
        "padTitle",
        "lineTitle",
        
        # --- Axes ---
        "aytitle",
        "chksymbolY",
        "markdownY",
        "symbolsY",
        "mdy",
        "axtitle",
        "chksymbolX",
        "markdownX",
        "symbolsX",
        "mdx",
        "Xrotate",
        "reverseX",
        "logscale",
        "labelY",
        "minY",
        "maxY",
        # "addYBreak",
        # "minYBreak",
        # "maxYBreak",
        # "breakScale",
        # "breakGap",
        "axisline",
        "ticklength",
        "tickwidth",
        "flipPlot",
        
        # --- Fonts ---
        "font",
        "plotFont",
        "Xfontcol",
        "Xfontsz",
        "Xlinebreak",
        "Yfontcol",
        "Yfontsz",
        "Ylinebreak",
        
        # --- Theme ---
        "choosetheme",
        "boxtheme",
        "grad1",
        "grad2",
        "choosethemeII",
        "grayscale",
        "boxbordercol",
        "boxbordercolG",
        "shadevalue",
        "shadevalueG",
        "shapeAlpha",
        "shapeAlphaG",
        "dpcolor",
        "dpfill",
        "dpgrad1",
        "dpgrad2",
        "dpgradF1",
        "dpgradF2",
        "dpcolorG",
        "dpfillG",
        
        # --- Statistics ---
        # "ttestType",
        # "paratestType",
        # "askPaired",
        # "askComp",
        # "askPairedssT",
        # "statContCols",
        # "compList",
        # "statCols",
        # "askCorrection",
        # "askCorrectionP",
        # "askCorrectionG",
        # "askControl",
        # "askCorrectionC",
        # "askCorrectionCG",
        # "askCorrectionPC",
        # "askCorrectionPCG",
        # "statTwoCol",
        # "grplist",
        "askTipType",
        "askPvalType",
        "askPvalStyle",
        "pvalSize",
        "pvalTVpos",
        "pvalHpos",
        "pvalVpos",
        "pvalCol",
        "topMargin",
        "firstBrack",
        "distWidth",
        "tipLength",
        "gapWidth",
        "bracWidth",
        "bracCol",
        
        # --- Export/Size ---
        "width",
        "height",
        "lockRatio",
        "selectFileType",
        "selectDPI"
      ),
      
      Data = sapply(list(
        # --- File & Data Settings --- #6
        # input$sheetlist,
        input$dataGroup,
        input$grpSwitch,
        # input$selectedCols,
        input$askPlotType,
        input$askPlotTypeG,
        input$askPlotTypeIIG,
        input$askPlotTypeII,
        
        # --- Graph Settings: Box-Whisker --- #11
        input$boxtype,
        input$outlier,
        input$boxwidth,
        input$linewidthBox,
        input$notch,
        input$scatterBox,
        input$pointsizeBox,
        input$pointDistBox,
        input$pointshapeBox,
        input$pointMethodBox,
        input$innerDistBox,
        
        # --- Graph Settings: Violin --- #9
        input$viotype,
        input$linewidthVio,
        input$boxWidthVio,
        input$boxColVio,
        input$boxColCust,
        input$askQuantLine,
        input$quantLineSize,
        input$endTrim,
        input$innerDistVio,
        
        # --- Graph Settings: Jitter --- #10
        input$scatter,
        input$pointsize,
        input$pointDist,
        input$pointshape,
        input$pointMethod,
        input$sum_typeJitter,
        input$statLine,
        input$statWidth,
        input$statColour,
        input$askSummPos,
        
        # --- Graph Settings: Raincloud --- #13
        input$slabSide,
        input$slabDistance,
        input$linewidthRain,
        input$endTrimRain,
        input$scatterRain,
        input$pointsizeRain,
        input$pointDistRain,
        input$pointshapeRain,
        input$sum_typeRain,
        input$statLineRain,
        input$statWidthRain,
        input$statColourRain,
        input$askSummPosRain,
        
        # --- Graph Settings: Bar --- #10
        input$barwidth,
        input$linewidthBar,
        input$barFunc,
        input$sum_typeBarMean,
        input$sum_typeBarMedian,
        input$statLineBar,
        input$statWidthBar,
        input$statColourBar,
        input$askSide,
        input$askJitter,
        
        # --- Graph Settings: Connecting Lines --- #4
        input$askConnectLine,
        input$connectLineSize,
        input$connectLineCol,
        input$connectLineType,
        
        # --- Plot Area --- #20
        input$plotThemeGrid,
        input$majGrid,
        input$minGrid,
        input$gridOpt,
        input$gridCol,
        input$plotThemeBorder,
        input$plotThemeBg,
        input$plotColor,
        input$legTitle,
        input$legPos,
        input$legTextSize,
        input$legTitleSize,
        input$legSize,
        input$legBorderSize,
        input$dpview,
        input$dpviewInfo,
        input$dpviewSize,
        input$dpviewMD,
        input$dpviewDecmP,
        input$dpviewPos,
        
        # --- Plot Title --- #10
        input$plotTitle,
        input$chksymbolTit,
        input$symbolsTit,
        input$markdownTit,
        input$mdTit,
        input$titlePos,
        input$verAlign,
        input$bWidthTitle,
        input$padTitle,
        input$lineTitle,
        
        # --- Axes --- #20
        input$aytitle,
        input$chksymbolY,
        input$markdownY,
        input$symbolsY,
        input$mdy,
        input$axtitle,
        input$chksymbolX,
        input$markdownX,
        input$symbolsX,
        input$mdx,
        input$Xrotate,
        input$reverseX,
        input$logscale,
        input$labelY,
        input$minY,
        input$maxY,
        # input$addYBreak,
        # input$minYBreak,
        # input$maxYBreak,
        # input$breakScale,
        # input$breakGap,
        input$axisline,
        input$ticklength,
        input$tickwidth,
        input$flipPlot,
        
        # --- Fonts --- #8
        input$font,
        input$plotFont,
        input$Xfontcol,
        input$Xfontsz,
        input$Xlinebreak,
        input$Yfontcol,
        input$Yfontsz,
        input$Ylinebreak,
        
        # --- Theme --- #17
        input$choosetheme,
        input$boxtheme,
        input$grad1,
        input$grad2,
        input$choosethemeII,
        input$grayscale,
        input$boxbordercol,
        input$boxbordercolG,
        input$shadevalue,
        input$shadevalueG,
        input$shapeAlpha,
        input$shapeAlphaG,
        input$dpcolor,
        input$dpfill,
        input$dpgrad1,
        input$dpgrad2,
        input$dpgradF1,
        input$dpgradF2,
        input$dpcolorG,
        input$dpfillG,
        
        # --- Statistics --- #33
        # input$ttestType,
        # input$paratestType,
        # input$askPaired,
        # input$askComp,
        # input$askPairedssT,
        # input$statContCols,
        # input$compList,
        # input$statCols,
        # input$askCorrection,
        # input$askCorrectionP,
        # input$askCorrectionG,
        # input$askControl,
        # input$askCorrectionC,
        # input$askCorrectionCG,
        # input$askCorrectionPC,
        # input$askCorrectionPCG,
        # input$statTwoCol,
        # input$grplist,
        input$askTipType,
        input$askPvalType,
        input$askPvalStyle,
        input$pvalSize,
        input$pvalTVpos,
        input$pvalHpos,
        input$pvalVpos,
        input$pvalCol,
        input$topMargin,
        input$firstBrack,
        input$distWidth,
        input$tipLength,
        input$gapWidth,
        input$bracWidth,
        input$bracCol,
        
        # --- Export/Size --- #5
        input$width,
        input$height,
        input$lockRatio,
        input$selectFileType,
        input$selectDPI
      ), flattenID), stringsAsFactors = F
    )
    
    ## For dynamically generated inputs
    # Point shape Box
    # the input ids
    palSID <- paste("pointshapeBox", seq_along(colCount()),sep = '_')
    # flatten the input id's values
    palSVal <- sapply(palSID, function(id){
      val <- input[[id]]
      if (is.null(val)) return(21)
      return(flattenID(val))
    })
    # combing the name, id and value in one data frame
    palShape <- data.frame(
      Parameter = c(rep("Point Shape Box",length(colCount()))),
      inputID = palSID,
      Data = as.character(palSVal) , stringsAsFactors = F
    )
    # adds to the main settings data frame
    settings_df <- rbind(settings_df,palShape)
    
    # Point shape Rain
    palSID <- paste("pointshapeRain", seq_along(colCount()),sep = '_')
    palSVal <- sapply(palSID, function(id){
      val <- input[[id]]
      if (is.null(val)) return(21)
      return(flattenID(val))
    })
    palShape <- data.frame(
      Parameter = c(rep("Point Shape Raincloud",length(colCount()))),
      inputID = palSID,
      Data = as.character(palSVal) , stringsAsFactors = F
    )
    settings_df <- rbind(settings_df,palShape)
    
    # Point shape Jitter
    palSID <- paste("pointshape", seq_along(colCount()),sep = '_')
    palSVal <- sapply(palSID, function(id){
      val <- input[[id]]
      if (is.null(val)) return(21)
      return(flattenID(val))
    })
    palShape <- data.frame(
      Parameter = c(rep("Point Shape Jitter",length(colCount()))),
      inputID = palSID,
      Data = as.character(palSVal) , stringsAsFactors = F
    )
    settings_df <- rbind(settings_df,palShape)
    
    # Palette themes
    palSID <- paste("colors", seq_along(colCount()),sep = '_')
    palSVal <- sapply(palSID, function(id){
      val <- input[[id]]
      if (is.null(val)) return('#CCCCCC')
      return(flattenID(val))
    })
    palShape <- data.frame(
      Parameter = c(rep("Shape Palette",length(colCount()))),
      inputID = palSID,
      Data = as.character(palSVal) , stringsAsFactors = F
    )
    settings_df <- rbind(settings_df,palShape)
    
    # Grouped Palette themes
    palSID <- paste("colorsG", seq_along(colCount()),sep = '_')
    palSVal <- sapply(palSID, function(id){
      val <- input[[id]]
      if (is.null(val)) return('#CCCCCC')
      return(flattenID(val))
    })
    palShape <- data.frame(
      Parameter = c(rep("Shape Palette Grouped",length(colCount()))),
      inputID = palSID,
      Data = as.character(palSVal) , stringsAsFactors = F
    )
    settings_df <- rbind(settings_df,palShape)
    
    # Data point Palette color themes
    palSID <- paste("colorsdp", seq_along(colCount()),sep = '_')
    palSVal <- sapply(palSID, function(id){
      val <- input[[id]]
      if (is.null(val)) return('#CCCCCC')
      return(flattenID(val))
    })
    palShape <- data.frame(
      Parameter = c(rep("DP Shape Color Palette",length(colCount()))),
      inputID = palSID,
      Data = as.character(palSVal) , stringsAsFactors = F
    )
    settings_df <- rbind(settings_df,palShape)
    
    # Data point Palette fill themes
    palSID <- paste("fillsdp", seq_along(colCount()),sep = '_')
    palSVal <- sapply(palSID, function(id){
      val <- input[[id]]
      if (is.null(val)) return('#CCCCCC')
      return(flattenID(val))
    })
    palShape <- data.frame(
      Parameter = c(rep("DP Shape Fill Palette",length(colCount()))),
      inputID = palSID,
      Data = as.character(palSVal) , stringsAsFactors = F
    )
    settings_df <- rbind(settings_df,palShape)
    return(settings_df)
  })
  
  ### Download handler for saving Settings File
  observe({
    #Activate the action buttons in the menu
    req(!is.null(data()))
    updateActionButton(session,'savesetting_proxy', disabled = FALSE)
    updateActionButton(session,'saveBtn', disabled = FALSE)
  })
  observeEvent(input$savesetting_proxy,{
    shinyjs::runjs("document.getElementById('savesetting').click();")
  })
  output$savesetting <- downloadHandler(
    filename = function() { paste0("SENsabled_Settings_", Sys.Date(), ".xlsx") },
    content = function(file) {
      openxlsx::write.xlsx(savesetting_df(), file= file, asTable = T)
    }
  )
  ### Processing for the setting file data to update all the inputs ###
  observeEvent(input$reuseset,{
    req(data(), !isTruthy(input$exampleFile))
    file <- input$usesetting
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "xlsx", "Please upload a xlsx file"))
    uploaded_inputs <- openxlsx::read.xlsx(file$datapath, sheet = 1)
    
    uploaded_inputs <- data.frame(uploaded_inputs, stringsAsFactors = FALSE)
    
    if (isTRUE(identical(uploaded_inputs[,2],savesetting_df()[,2]))){
      # if (sum(grepl("^colors_",uploaded_inputs[,2])) == length(current_colnames()) ||
      #     sum(grepl("^colorsG_",uploaded_inputs[,2])) == length(current_colnames())){
      # Converting logical values
      chngLogical <- function(x) {
        if (is.logical(x)) return(x)
        if (is.numeric(x)) return(as.logical(x))
        x <- trimws(toupper(as.character(x)))
        if (x %in% c("TRUE", "T", "YES", "1")) return(TRUE)
        if (x %in% c("FALSE", "F", "NO", "0")) return(FALSE)
      }
      
      for (i in 1:nrow(uploaded_inputs)) {
        raw_value <- uploaded_inputs[i, 3]
        id <- as.character(uploaded_inputs[i, 2])
        
        # Separate strings with '&' 
        if (!is.na(raw_value) && grepl("&", raw_value, fixed = TRUE)) {
          value <- unlist(strsplit(as.character(raw_value), "&", fixed = TRUE))
          value <- trimws(value)
        } else {
          value <- raw_value
        }
        
        try({ # Updates all the inputs from the setting files
          # Text / Numeric
          updateTextInput(session, id, value = value)
          if (is.numeric(value) || !is.na(safe_as_numeric(value))) {
            updateNumericInputIcon(session, id, value = safe_as_numeric(value))
          }
          # Select / Picker
          updateSelectInput(session, id, selected = value)
          updatePickerInput(session, id, selected = value)
          
          # NoUiSlider (numeric)
          if (!is.na(safe_as_numeric(value))) {
            updateNoUiSliderInput(session, id, value = safe_as_numeric(value))
          }
          
          # Checkboxes / Groups
          updateCheckboxInput(session, id, value = chngLogical(value))
          updateCheckboxGroupInput(session, id, selected = value)
          updateCheckboxGroupButtons(session, id, selected = value)
          
          # Radio Buttons
          if (grepl("pointshape", id, ignore.case = TRUE)) {
            if (grepl("^pointshapeBox_", id)){
              shapeBox$shapes[[id]] <- value
            }
            if (grepl("^pointshapeRain_", id)){
              shapeRain$shapes[[id]] <- value
            }
            if (grepl("^pointshape_", id)){
              shapeJitter$shapes[[id]] <- value
            }
          }
          updateRadioGroupButtons(session, id, selected = value)
          
          # prettySwitch & prettyToggle
          updatePrettySwitch(session, id, value = chngLogical(value))
          updatePrettyToggle(session, id, value = chngLogical(value))
          # 
          # Colourpicker
          if (grepl("col|grad|fill|bg", id, ignore.case = TRUE)) {
            if (grepl("^colors_", id)) {
              palTheme$palette[[id]] <- value
            }
            if (grepl("^colorsG_", id)) {
              palThemeG$palette[[id]] <- value
            }
            if (grepl("^colorsdp_", id)) {
              dpPalTheme$palette[[id]] <- value
            }
            if (grepl("^fillsdp_", id)) {
              dpPalFillTheme$palette[[id]] <- value
            }
            updateColorPickr(session, id, value = value)
          }
          
        }, silent = TRUE)
      }     
      # Keeping all the relevant uiOutput to be always suspended 
      # when reuse button is clicked
      outputOptions(output, c('coltabsOut'), suspendWhenHidden = FALSE)
      outputOptions(output, c('coltabsOutG'), suspendWhenHidden = FALSE)
      outputOptions(output, c('dpcoltabsOut'), suspendWhenHidden = FALSE)
      outputOptions(output, c('dpfilltabsOut'), suspendWhenHidden = FALSE)
      outputOptions(output, c('pointInpUIBox'), suspendWhenHidden = FALSE)
      outputOptions(output, c('pointInpUIBoxG'), suspendWhenHidden = FALSE)
      outputOptions(output, c('pointInpUIRain'), suspendWhenHidden = FALSE)
      outputOptions(output, c('pointInpUI'), suspendWhenHidden = FALSE)
      outputOptions(output, c('submitAnalysis'), suspendWhenHidden = FALSE)
      
      if (!isTruthy(input$exampleFile)){
        shinyWidgets::show_toast(
          title = "Settings Restored!",
          text = "All inputs updated from file.", type = "success")
      } 
      # }
    }else {
      show_alert(
        title = "Invalid Setting File",
        text = "Please upload correct settings file. 
        Make sure there were no changes made in the file and
        the column numbers are equal.", 
        type = "error")
    }
  })
  
  ### Processing for the setting file data to update all the inputs (Demo data) ###
  observeEvent(input$exampleFile,{
    #Load presaved settings file upon demo data loading
    req(demoFile())
    uploaded_inputs <- as.data.frame(demoSettFile())
    uploaded_inputs <- data.frame(uploaded_inputs, stringsAsFactors = FALSE)
    
    # Converting logical values
    chngLogical <- function(x) {
      if (is.logical(x)) return(x)
      if (is.numeric(x)) return(as.logical(x))
      x <- trimws(toupper(as.character(x)))
      if (x %in% c("TRUE", "T", "YES", "1")) return(TRUE)
      if (x %in% c("FALSE", "F", "NO", "0")) return(FALSE)
    }
    
    for (i in 1:nrow(uploaded_inputs)) {
      raw_value <- uploaded_inputs[i, 3]
      id <- as.character(uploaded_inputs[i, 2])
      
      # Separate strings with '&' 
      if (!is.na(raw_value) && grepl("&", raw_value, fixed = TRUE)) {
        value <- unlist(strsplit(as.character(raw_value), "&", fixed = TRUE))
        value <- trimws(value)
      } else {
        value <- raw_value
      }
      
      try({ # Updates all the inputs from the setting files
        # Text / Numeric
        updateTextInput(session, id, value = value)
        if (is.numeric(value) || !is.na(safe_as_numeric(value))) {
          updateNumericInputIcon(session, id, value = safe_as_numeric(value))
        }
        # Select / Picker
        updateSelectInput(session, id, selected = value)
        updatePickerInput(session, id, selected = value)
        
        # NoUiSlider (numeric)
        if (!is.na(safe_as_numeric(value))) {
          updateNoUiSliderInput(session, id, value = safe_as_numeric(value))
        }
        
        # Checkboxes / Groups
        updateCheckboxInput(session, id, value = chngLogical(value))
        updateCheckboxGroupInput(session, id, selected = value)
        updateCheckboxGroupButtons(session, id, selected = value)
        
        # Radio Buttons
        if (grepl("pointshape", id, ignore.case = TRUE)) {
          if (grepl("^pointshapeBox_", id)){
            shapeBox$shapes[[id]] <- value
          }
          if (grepl("^pointshapeRain_", id)){
            shapeRain$shapes[[id]] <- value
          }
          if (grepl("^pointshape_", id)){
            shapeJitter$shapes[[id]] <- value
          }
        }
        updateRadioGroupButtons(session, id, selected = value)
        
        # prettySwitch & prettyToggle
        updatePrettySwitch(session, id, value = chngLogical(value))
        updatePrettyToggle(session, id, value = chngLogical(value))
        # 
        # Colourpicker
        if (grepl("col|grad|fill|bg", id, ignore.case = TRUE)) {
          if (grepl("^colors_", id)) {
            palTheme$palette[[id]] <- value
          }
          if (grepl("^colorsG_", id)) {
            palThemeG$palette[[id]] <- value
          }
          if (grepl("^colorsdp_", id)) {
            dpPalTheme$palette[[id]] <- value
          }
          if (grepl("^fillsdp_", id)) {
            dpPalFillTheme$palette[[id]] <- value
          }
          updateColorPickr(session, id, value = value)
        }
        
      }, silent = TRUE)
    }     
    # Keeping all the relevant uiOutput to be always suspended 
    # when exampleFile button is clicked
    outputOptions(output, c('coltabsOut'), suspendWhenHidden = FALSE)
    outputOptions(output, c('coltabsOutG'), suspendWhenHidden = FALSE)
    outputOptions(output, c('dpcoltabsOut'), suspendWhenHidden = FALSE)
    outputOptions(output, c('dpfilltabsOut'), suspendWhenHidden = FALSE)
    outputOptions(output, c('pointInpUIBox'), suspendWhenHidden = FALSE)
    outputOptions(output, c('pointInpUIBoxG'), suspendWhenHidden = FALSE)
    outputOptions(output, c('pointInpUIRain'), suspendWhenHidden = FALSE)
    outputOptions(output, c('pointInpUI'), suspendWhenHidden = FALSE)
    outputOptions(output, c('submitAnalysis'), suspendWhenHidden = FALSE)
  })
  ##End of Server##
}