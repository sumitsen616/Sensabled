### Source Code for SEN'sabale Plotting App ###
### MIT License - see LICENSE file for details
### Copyright (c) 2026 Sumit Sen

### UI code ###
#Shiny app builder
#' @import shiny
#' @import shinyBS
#' @importFrom shinyjs delay disable enable inlineCSS useShinyjs
#' @import shinycssloaders
#' @import waiter
#' @import shinyWidgets
#Data management
#' @import openxlsx
#' @import tidyr
#' @importFrom DT datatable renderDT DTOutput JS
#' @import stringr
#' @import tidyverse
#' @import dplyr
#Copy to clipboard
#' @import rclipboard
#' @importFrom readr format_tsv
#For plot generation
#' @import ggplot2
#' @import scales
#' @importFrom Hmisc smean.sdl smedian.hilow
#' @import ggbeeswarm
#' @import ggnewscale
#' @import ggdist
#' @import ggtext
#' @import patchwork
#' @importFrom qqplotr stat_qq_band stat_qq_point
#For Font styles
#' @import extrafont
#' @import fontawesome
#Plot and app theme
#' @import colorspace
#' @importFrom bslib accordion accordion_panel accordion_panel_set bs_theme card card_header layout_sidebar nav_panel nav_select navbar_options navset_underline page_navbar popover sidebar toggle_popover
#For Stats
#' @importFrom DescTools ColToGray CombPairs DunnettTest
#' @importFrom lme4 lmer
#' @import emmeans
#' @import PMCMRplus
#' @importFrom broom tidy
#' @importFrom stats anova aov median na.omit relevel runif sd setNames shapiro.test symnum t.test wilcox.test
#' @importFrom car leveneTest
#' @import rstatix
#' @import ARTool
#' @importFrom purrr has_element
#' @importFrom grDevices boxplot.stats colorRampPalette
#' @importFrom utils read.delim
#' @importFrom purrr has_element
# options(shiny.maxRequestSize = 250 * 1024^2) #Max file size to be uploaded is 250mb

app_ui <- function(request){
  page_navbar(
    id = "main_ui",
    title = tags$div(
      tags$img(
        src = 'www/app_logo.png',
        height = '50px',
        style = "margin:auto; position:absolute; left:10px; margin-top:-10px;"
      ),tags$span("SEN'sable Plotting", style = "position:absolute; left:70px"),
      style= "display:inline-flex; flex-direction:row; width:230px;
    height:100%; position:relative; top:-23px"
    ),
    #bslib theme for the app
    theme = bs_theme("zephyr", version= 5),
    header = tagList(
      ## Calling functions required in the app
      shinyjs::useShinyjs(),
      shinyjs::inlineCSS(css),
      fontawesome::fa_html_dependency(),
      waiter::useWaiter(),
      rclipboard::rclipboardSetup(),
      #Enable tooltips dynamically for elements added later to the page
      tags$head(
        tags$script(HTML("
        $(document).ready(function(){
          $('body').tooltip({
            selector: '[data-toggle=\"tooltip\"]'
          });
        });
      "))
      ),
      #JavaScript "Engine" to add markdown at active text input area 
      #Generated using Gemini and Grok AI
      tags$script(HTML("
      var lastFocusedId = null;
  
      // Track which newcolname_ input is focused
      $(document).on('focus', 'input[id^=\"newcolname_\"], #axtitle, #aytitle, #plotTitle', function() {
        lastFocusedId = this.id;
        Shiny.setInputValue('active_input', this.id);
      });

      // NEW: Custom message handler that WRAPS selected text
      Shiny.addCustomMessageHandler('wrapText', function(data) {
        var el = document.getElementById(data.id);
        if (!el) return;

        var start = el.selectionStart;
        var end   = el.selectionEnd;
        var text  = el.value;

        var open  = data.open;
        var close = data.close || '';        // <br> has no closing tag

        var before   = text.substring(0, start);
        var selected = text.substring(start, end);
        var after    = text.substring(end);

        var newValue;
        var newCursorPos;

        if (start === end) {
          // Nothing selected : insert tags and place cursor BETWEEN them
          newValue     = before + open + close + after;
          newCursorPos = start + open.length;
        } else {
          // Text is selected : wrap it
          newValue     = before + open + selected + close + after;
          newCursorPos = end + open.length + close.length;
        }

        el.value = newValue;
        el.focus();
       el.setSelectionRange(newCursorPos, newCursorPos);

        // Tell Shiny the value changed
        $(el).trigger('input');
        $(el).trigger('change');
      });
  
    "))
    ),
    navbar_options = navbar_options(theme = "dark"),
    
    ### Main Panels ###
    nav_panel(title = "File Upload",
              card(
                card_header("Data View"),
                layout_sidebar(
                  sidebar = sidebar(
                    # bg = "#EFEFEF",
                    width = 350,
                    h4("Data Settings"),
                    div(style = "padding:10px; border:2px solid #C4C4C4; border-radius:10px;",
                        ## Data Import Options
                        navset_underline(
                          id='sub_ui',
                          nav_panel(
                            title = "Upload",
                            # 1. Import via uploading excel file
                            div(style = "padding:10px;",
                                fileInput('file', 'Choose XLSX File', accept = c('.xlsx'))
                            )
                          ),
                          nav_panel(
                            title = "Paste",
                            # 2. Import via pasting data table
                            div(style = "padding:10px;",
                                textAreaInput('pasted_Data', label = 'Paste data in the box',rows = 6,
                                              placeholder = "Col1 Col2 Col3 \n12 23 53\n24 45 60"),
                                conditionalPanel(
                                  condition = "input.pasted_Data.length > 0",
                                  div(actionButton('pasteBtn','Upload Pasted Data',
                                                   icon = icon('paste'), width = "100%"),
                                      actionButton('clearBox', 'Clear Box',
                                                   icon = icon('trash-arrow-up'), width = '100%'),
                                      style = "display:flex; flex-direction:column; gap: 7px;
                                margin:auto; align-item:center; justify-content:center; width:100%;")
                                )
                            )
                          ),
                          nav_panel(
                            title = "Demo Data",
                            # 3. Using example dataset
                            div(style = "padding:10px;",
                                div(
                                  radioGroupButtons('demoSheetList',
                                                    label = "Select Demo Datatype",
                                                    choices = c("Ungrouped" = 1, 'Grouped' = 2),
                                                    selected = 1, width = "100%",
                                                    direction = "vertical"),
                                  actionButton('exampleFile', 'Upload Example File', 
                                               icon = icon('file-import'), width = "100%"),
                                  style = "display:flex; flex-direction:column;
                                margin:auto; align-item:center; justify-content:center; width:100%;")
                            )
                          ))
                    ),
                    ## Additional options for data management as UI render
                    #Choosing the sheet from the xlsx file
                    uiOutput('sheetnames'),
                    #Data grouping selection
                    uiOutput('grpBtn'),
                    #Plot Type Options
                    uiOutput('fileupload'),
                    #Submit Button
                    uiOutput('uploadBtnShow'),
                    #Button to open update column header modal
                    uiOutput('colupdateBttn')
                  ), 
                  ### Main Data Table Display ###
                  DTOutput('contents',fill = T),
                  #Renders tips for editing data table
                  uiOutput('DTtipOut')
                )
              )),
    nav_panel(title = "Graph",
              card(
                card_header("Graph View"),
                layout_sidebar(
                  sidebar = sidebar(
                    width=350,
                    h4("Graph Settings"),
                    tagList(
                      #Renders options for adding annotation brackets on graph
                      uiOutput('grpselect'),
                      uiOutput('askAnnotation')
                    ),
                    accordion(multiple = F, class = 'genAcc', id = 'genAcc',
                              accordion_panel(
                                title = 'Customize Shapes',
                                
                                ### Options for Box-Whisker Plot type ###
                                conditionalPanel(
                                  condition = "(input.askPlotTypeII == 'box' && input.dataGroup == false) || 
                                (input.askPlotTypeIIG == 'box' && input.dataGroup == true) ",
                                  #Choosing box plot type
                                  radioGroupButtons(
                                    "boxtype", "Add Datapoints",
                                    choices = c("Add" = 'boxpoint',
                                                "Remove" = 'boxOnly'),
                                    size = 'sm', selected = 'boxpoint'
                                  ),
                                  #Choosing to add outliers
                                  prettySwitch(
                                    "outlier",
                                    label = "Mark Outliers",
                                    status = 'success', value = FALSE,
                                    fill = TRUE
                                  ),
                                  #Choosing box-width
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'boxwidth',
                                        label = 'Box Width',
                                        min = 10, max = 100,
                                        value = 60, tooltips=TRUE,
                                        step=1, height="10px")),
                                  
                                  #Controlling box outline width
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'linewidthBox',
                                        label = 'Line Width',
                                        min = 0, max = 100, value = 50,
                                        tooltips = TRUE, step = 1, height = '10px')),
                                  
                                  #Choosing to add notch or not
                                  switchInput("notch", label = "Notches", value = F,
                                              onLabel = "On", offLabel = "Off",
                                              onStatus = "success", 
                                              offStatus = "danger", size = 'small'),
                                  
                                  #Choosing the Jitter Options
                                  conditionalPanel(
                                    condition = "input.boxtype == 'boxpoint'",
                                    #Point scatter width for box plot
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'scatterBox',
                                          label = 'Point Scatter Width',
                                          min = 1, max = 100, value = 30,
                                          tooltips = TRUE, step = 1, height = '10px')),
                                    #Point size for box plot
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'pointsizeBox',
                                          label = 'Point Size',
                                          min = 1, max = 100, value = 30,
                                          tooltips = TRUE, step = 1, height = '10px')),
                                    #Choosing point shapes the same or different for box plot
                                    switchInput('pointDistBox',
                                                label="Shape Similarity", value=FALSE,
                                                onLabel = "Independent",
                                                offLabel = "Same",
                                                size = 'small'),
                                    #Renders point shapes for individual columns for box plot
                                    conditionalPanel(
                                      condition = "input.pointDistBox && !input.dataGroup",
                                      #For ungrouped data
                                      uiOutput("pointInpUIBox")
                                    ),
                                    conditionalPanel(
                                      condition = "input.pointDistBox && input.dataGroup",
                                      #For grouped data
                                      uiOutput("pointInpUIBoxG")
                                    ),
                                    #Renders point shapes for all columns for box plot
                                    conditionalPanel(
                                      condition = "!input.pointDistBox",
                                      radioGroupButtons(
                                        inputId = "pointshapeBox",
                                        label = "Point Shape", 
                                        choices = iconlist,
                                        justified = T, size = 'sm', selected = 21)
                                    ),
                                    #Point jitter pattern for box-plot
                                    pickerInput(
                                      'pointMethodBox', 'Jitter Pattern',
                                      choices = c('Random' = 'swarm','Square Grid' ='square',
                                                  'Hexgrid' = 'hex', 'Centrally Symmetric' = 'center')),
                                    #Option to set distance between grouped-box shapes
                                    conditionalPanel(
                                      condition = "input.dataGroup == true",
                                      div(id='sliderstyle',
                                          noUiSliderInput(
                                            'innerDistBox',
                                            label = 'Distance Between Grouped Shapes',
                                            min = 50, max = 100, value = 70,
                                            tooltips = TRUE, step = 1, height = '10px'))
                                    )
                                  )
                                ),
                                
                                ### Accordion Options for Violin Plot type ###
                                
                                conditionalPanel(
                                  condition = "(input.askPlotTypeII == 'violin' && input.dataGroup == false ) ||
                                (input.askPlotTypeIIG == 'violin' && input.dataGroup == true)",
                                  tagList(
                                    #Choosing violin shape with or without quantile box
                                    radioGroupButtons(
                                      "viotype", "Add Box-whisker",
                                      choices = c("Add"="Violin-Box", "Remove"="Violin"),
                                      selected = "Violin-Box", size = 'sm'
                                    ),
                                    #Line-width for violin shapes
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'linewidthVio',
                                          label = 'Line Width',
                                          min = 0, max = 100, value = 40,
                                          tooltips = TRUE, step = 1, height = '10px')),
                                    #Options for violin shapes when quantile box is present
                                    conditionalPanel(
                                      condition = "input.viotype=='Violin-Box'",
                                      #Box-width
                                      div(id='sliderstyle',
                                          noUiSliderInput(
                                            'boxWidthVio',
                                            label = 'Box Width',
                                            min = 10, max = 100, value = 30,
                                            tooltips = TRUE, step = 1, height = '10px')),
                                      #Box-color type
                                      radioGroupButtons(
                                        'boxColVio',
                                        'Box Color',
                                        choices = c("Shape","Custom"),
                                        size = 'sm'),
                                      #Custom box-color
                                      conditionalPanel(
                                        condition = "input.boxColVio == 'Custom' ",
                                        colorPickr('boxColCust',
                                                   label = NULL,
                                                   selected = '#FFFFFF',
                                                   pickr_width = '20%')
                                      )
                                    ),
                                    #Options for violin shapes when quantile box is not present
                                    conditionalPanel(
                                      condition = "input.viotype== 'Violin'",
                                      #Choosing whether to add quantile lines
                                      prettySwitch(
                                        'askQuantLine', label = 'Add Quantile Lines',
                                        value = F, status = 'success', fill = T
                                      ),
                                      conditionalPanel(
                                        condition = "input.askQuantLine == true",
                                        #Line-width of quantile lines
                                        div(id='sliderstyle',
                                            noUiSliderInput(
                                              'quantLineSize',
                                              label = 'Quantile Line Width',
                                              min = 1, max = 100, value = 50,
                                              tooltips = TRUE, step = 1, height = '10px'))
                                      )
                                    ),
                                    #Choosing whether to trim ends of violin shapes
                                    prettySwitch(
                                      'endTrim', label = 'Trim Ends', value = TRUE,
                                      status = 'success', fill = T
                                    ),
                                    #Option to set distance between grouped-violin shapes
                                    conditionalPanel(
                                      condition = "input.dataGroup == true",
                                      div(id='sliderstyle',
                                          noUiSliderInput(
                                            'innerDistVio',
                                            label = 'Distance Between Grouped Shapes',
                                            min = 1, max = 100, value = 70,
                                            tooltips = TRUE, step = 1, height = '10px'))
                                    )
                                  )
                                ),
                                
                                ### Accordion Options for Jitter Plot type ###
                                
                                #Jitter scatter-width
                                conditionalPanel(
                                  condition = "input.askPlotTypeII == 'jitter'",
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'scatter',
                                        label = 'Point Scatter Width',
                                        min = 1, max = 100, value = 30,
                                        tooltips = TRUE, step = 1, height = '10px')),
                                  #Jitter point-size 
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'pointsize',
                                        label = 'Point Size',
                                        min = 1, max = 100, value = 30,
                                        tooltips = TRUE, step = 1, height = '10px')),
                                  #Choosing point shapes the same or different 
                                  switchInput('pointDist',
                                              label="Shape Similarity", value=FALSE,
                                              onLabel = "Independent",
                                              offLabel = "Same",
                                              size = 'small'),
                                  #Renders point shapes options for individual data column
                                  conditionalPanel(
                                    condition = "input.pointDist== true",
                                    uiOutput("pointInpUI")
                                  ),
                                  #Point shapes options for all data column
                                  conditionalPanel(
                                    condition = "input.pointDist== false",
                                    radioGroupButtons(
                                      inputId = "pointshape",
                                      label = "Point Shape", 
                                      choices = iconlist,
                                      justified = T, size = 'sm', selected = 21)
                                  ),
                                  #Jitter pattern for jitter plot
                                  pickerInput(
                                    'pointMethod', 'Jitter Pattern',
                                    choices = c('Random' = 'swarm','Square Grid' ='square',
                                                'Hexgrid' = 'hex', 'Centrally Symmetric' = 'center')),
                                  #Choosing type of summary statistics to add on jitter plot
                                  pickerInput(
                                    'sum_typeJitter', 'Summary Statistics',
                                    choices = c(
                                      "Mean" = "mean_only",
                                      "Mean with SD" = "mean_sd",
                                      "Mean with SEM" = "mean_sem",
                                      "Median" = "median_only",
                                      "Median with 95% CI" = "median_ci"
                                    )),
                                  #Line-width for summary stat 
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'statLine',
                                        label = 'Stat Summ Line Width',
                                        min = 0, max = 100, value = 50,
                                        tooltips = TRUE, step = 1, height = '10px')),
                                  #Line-width for error bars of summary stat
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'statWidth',
                                        label = 'Stat Summ Bar Width',
                                        min = 0, max = 100, value = 50,
                                        tooltips = TRUE, step = 1, height = '10px')),
                                  #Choosing colors for summary stat lines
                                  colorPickr(
                                    'statColour',
                                    label = 'Stat Summ Line Color',
                                    selected = '#000000',
                                    pickr_width = "20%"
                                  ),
                                  #Choosing layer position of summary stat over points
                                  radioGroupButtons(
                                    'askSummPos',
                                    label = 'Stat Summ Line Position',
                                    choices = c("Front" = 'top',
                                                "Back" = 'back'),
                                    selected = 'top'
                                  )
                                ),
                                
                                ### Accordion Options for Raincloud Plot type ###
                                
                                conditionalPanel(
                                  condition = "input.askPlotTypeII == 'viopoint'",
                                  tagList(
                                    #Choosing L-R orientation of half-violin
                                    radioGroupButtons(
                                      "slabSide", "Half-Violin Orientation",
                                      choices = c("Left" = 'left', "Right" = 'right'),
                                      selected = "right", size = 'sm'
                                    ),
                                    #Choosing gap distance between half violin and jitter points
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'slabDistance',
                                          label = 'Adjust Gap',
                                          min = 0, max = 100, value = 0,
                                          tooltips = TRUE, step = 10, height = '10px')),
                                    #Line-width of half-violin shape
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'linewidthRain',
                                          label = 'Line Width',
                                          min = 0, max = 100, value = 40,
                                          tooltips = TRUE, step = 1, height = '10px')),
                                    #Choosing whether to trim ends of half violin 
                                    prettySwitch(
                                      'endTrimRain', label = 'Trim Ends', value = TRUE,
                                      fill = T, status = 'success'
                                    ),
                                    ## Options for jitter points for raincloud plot
                                    #Point scatter-width
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'scatterRain',
                                          label = 'Point Scatter Width',
                                          min = 1, max = 100, value = 30,
                                          tooltips = TRUE, step = 1, height = '10px')),
                                    #Point-size
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'pointsizeRain',
                                          label = 'Point Size',
                                          min = 1, max = 100, value = 30,
                                          tooltips = TRUE, step = 1, height = '10px')),
                                    #Choosing point shapes the same or different
                                    switchInput('pointDistRain',
                                                label="Shape Similarity", value=FALSE,
                                                onLabel = "Independent",
                                                offLabel = "Same",
                                                size = 'small'),
                                    #Renders point shapes options for individual data column
                                    conditionalPanel(
                                      condition = "input.pointDistRain== true",
                                      uiOutput("pointInpUIRain")
                                    ),
                                    #Point shapes options for all data column
                                    conditionalPanel(
                                      condition = "input.pointDistRain== false",
                                      radioGroupButtons(
                                        inputId = "pointshapeRain",
                                        label = "Point Shape", 
                                        choices = iconlist,
                                        justified = T, size = 'sm', selected = 21)
                                    ),
                                    #Choosing type of summary statistics to add on raincloud plot
                                    pickerInput(
                                      'sum_typeRain', 'Summary Statistics',
                                      choices = c(
                                        "Mean" = "mean_only",
                                        "Mean with SD" = "mean_sd",
                                        "Mean with SEM" = "mean_sem",
                                        "Median" = "median_only",
                                        "Median with 95% CI" = "median_ci"
                                      )),
                                    #Line-width for summary stat
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'statLineRain',
                                          label = 'Stat Summ Linewidth',
                                          min = 0, max = 100, value = 50,
                                          tooltips = TRUE, step = 1, height = '10px')),
                                    #Line-width for error bars of summary stat
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'statWidthRain',
                                          label = 'Stat Summ Bar Width',
                                          min = 0, max = 100, value = 50,
                                          tooltips = TRUE, step = 1, height = '10px')),
                                    #Choosing colors for summary stat lines
                                    colorPickr(
                                      'statColourRain',
                                      label = 'Stat Summ Line Color',
                                      selected = '#000000',
                                      pickr_width = "20%"
                                    ),
                                    #Choosing layer position of summary stat over points
                                    radioGroupButtons(
                                      'askSummPosRain',
                                      label = 'Stat Summ Line Position',
                                      choices = c("Front" = 'top',
                                                  "Back" = 'back'),
                                      selected = 'top'
                                    )
                                  )
                                ),
                                
                                ### Accordion Options for Bar Plot type ###
                                
                                conditionalPanel(
                                  condition = "input.askPlotTypeII == 'bar'",
                                  tagList(
                                    #Bar-width
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'barwidth',
                                          label = 'Bar Width',
                                          min = 10, max = 100,
                                          value = 60, tooltips=TRUE,
                                          step=1, height="10px")),
                                    #Bar line-width
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'linewidthBar',
                                          label = 'Line Width',
                                          min = 0, max = 100, value = 40,
                                          tooltips = TRUE, step = 1, height = '10px')),
                                    #Choosing mean or median to set bar-height
                                    radioGroupButtons(
                                      'barFunc', 'Bar Display',
                                      choices = c('Mean'= 'mean', 'Median' = 'median'),
                                      selected = 'mean', size = 'sm'
                                    ),
                                    #Choosing options for mean summary stat
                                    conditionalPanel(
                                      condition = "input.barFunc == 'mean'",
                                      pickerInput(
                                        'sum_typeBarMean', 'Summary Statistics',
                                        choices = c(
                                          "Mean" = "mean_only",
                                          "Mean with SD" = "mean_sd",
                                          "Mean with SEM" = "mean_sem"
                                        ), selected = 'mean_only')
                                    ),
                                    #Choosing options for median summary stat
                                    conditionalPanel(
                                      condition = "input.barFunc == 'median'",
                                      pickerInput(
                                        'sum_typeBarMedian', 'Summary Statistics',
                                        choices = c(
                                          "Median" = "median_only",
                                          "Median with 95% CI" = "median_ci"
                                        ), selected = 'median_only')
                                    ),
                                    #Line-width for summary stat
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'statLineBar',
                                          label = 'Stat Summ Linewidth',
                                          min = 0, max = 100, value = 50,
                                          tooltips = TRUE, step = 1, height = '10px')),
                                    #Line-width for error bars of summary stat
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'statWidthBar',
                                          label = 'Stat Summ Bar Width',
                                          min = 0, max = 100, value = 50,
                                          tooltips = TRUE, step = 1, height = '10px')),
                                    #Choosing colors for summary stat lines
                                    colorPickr(
                                      'statColourBar',
                                      label = 'Stat Summ Line Color',
                                      selected = '#000000',
                                      pickr_width = "20%"
                                    ),
                                    #Choosing layer position of summary stat over bars
                                    radioGroupButtons(
                                      'askSide', 'Errorbar Direction',
                                      choices = c('Bothside', 'Outside'),
                                      size = 'sm', selected = 'Bothside'
                                    ),
                                    #Choosing wheter to add jitter points over bars
                                    prettySwitch(
                                      'askJitter', 'Add Datapoints',
                                      status = 'success', value = F, fill = T
                                    ),
                                    #Displays important notice if data points are added
                                    conditionalPanel(
                                      condition = "input.askJitter == true",
                                      p("Note: Datapoints are appened from Jitter Plot.
                                      Customize datapoints at 'Jitter Plot' section.", 
                                        style="color: grey; font-size:13px;")
                                    )
                                    
                                  )
                                ),
                                
                                ### Options shown if paired data type is selected ###
                                
                                conditionalPanel(
                                  condition = "input.askPaired == true || 
                                    input.askPairedssT == true",
                                  #Choosing whether to add connecting lines between shapes
                                  prettySwitch(
                                    'askConnectLine',
                                    'Add Connecting Line',
                                    value = T, fill = T, status = 'success'
                                  ),
                                  #Line-width of connecting line
                                  div(style = "padding:10px; border:1px solid; border-radius:5px;",
                                      div(id='sliderstyle',
                                          noUiSliderInput(
                                            'connectLineSize',
                                            label = 'Line Width',
                                            min = 0, max = 100, value = 50,
                                            tooltips = TRUE, step = 1, height = '10px')),
                                      #Color of connecting line
                                      colorPickr(
                                        'connectLineCol',
                                        label = 'Line Color',
                                        selected = '#000000',
                                        pickr_width = "20%"
                                      ),
                                      #Dash-type for connecting line
                                      pickerInput(
                                        'connectLineType',
                                        label = 'Line Type',
                                        choices = c(
                                          'Solid' = 'solid', 'Dashed' = 'dashed',
                                          'Long Dashed' = 'longdash', 'Dotted' = 'dotted'
                                        ), 
                                        selected = 'solid'
                                      )
                                  )
                                )
                              ),
                              
                              ### Accordion Options for Plot Area ###
                              
                              accordion_panel(
                                title = 'Customize Plot Area',
                                #Chossing to add plot grids
                                prettySwitch(
                                  "plotThemeGrid",
                                  label = "Add Grids",status = 'success', fill = T
                                ),
                                #Grid options
                                conditionalPanel(
                                  condition = "input.plotThemeGrid==true ",
                                  tagList(
                                    checkboxInput('majGrid','Major Grids', value = T),
                                    checkboxInput('minGrid','Minor Grids', value = T),
                                    checkboxGroupButtons('gridOpt', 'Choose Axis', 
                                                         choices = c('X','Y'),
                                                         selected = 'Y',
                                                         justified = T, size = 'sm'),
                                    radioGroupButtons('gridCol', 'Grids Color',
                                                      choices = c('Black'= 'black',
                                                                  'White'= 'white',
                                                                  'Grey' = 'grey',
                                                                  'Light Grey' = '#efefef'), size = 'sm'))),
                                #Choosing to add plot-border
                                prettySwitch(
                                  "plotThemeBorder",
                                  label = "Add Borders",status = 'success', fill = T
                                ),
                                #Choosing to add plot-background
                                prettySwitch(
                                  "plotThemeBg",
                                  label = "Add Background Color",status = 'success', fill = T
                                ),
                                #Plot-background color
                                conditionalPanel(
                                  condition = "input.plotThemeBg== true",
                                  colorPickr('plotColor', label = 'Plot Background',
                                             selected = "#FFFFFF",
                                             pickr_width = '20%')),
                                #Choosing legends position for grouped-data plot 
                                conditionalPanel(
                                  condition = "input.dataGroup == true",
                                  div(style = "border:1px solid; padding:10px; border-radius:10px; margin-bottom:10px;" ,
                                      textInput("legTitle", "Legend Title", value = "Treatments"),
                                      selectInput("legPos", "Legend Position", 
                                                  choices = c("Right"="right",
                                                              "Left"="left",
                                                              "Top"="top",
                                                              "Bottom"="bottom",
                                                              "Inside" = 'inside'),
                                                  selected = "right"),
                                      #XY coordinate when legend position is inside
                                      conditionalPanel(
                                        condition = "input.legPos == 'inside'",
                                        p("Click on the box to position the legend.",
                                          style= "font-weight:500; font-size:12px;"),
                                        plotOutput("legPosPlot", height = "150px",
                                                   click = 'plot_click')
                                      ),
                                      #Legend text-size
                                      div(id='sliderstyle',
                                          noUiSliderInput(
                                            'legTextSize',
                                            label = 'Legend Text Size',
                                            min = 5, max = 30,
                                            value = 12, tooltips=TRUE,
                                            step=1, height="10px")),
                                      #Legend title-size
                                      div(id='sliderstyle',
                                          noUiSliderInput(
                                            'legTitleSize',
                                            label = 'Legend Title Size',
                                            min = 5, max = 35,
                                            value = 14, tooltips=TRUE,
                                            step=1, height="10px")),
                                      #Legend key-size
                                      div(id='sliderstyle',
                                          noUiSliderInput(
                                            'legSize',
                                            label = 'Legend Key Size',
                                            min = 50, max = 100,
                                            value = 50, tooltips=TRUE,
                                            step=1, height="10px")),
                                      #Legend border line-width
                                      div(id='sliderstyle',
                                          noUiSliderInput(
                                            'legBorderSize',
                                            label = 'Legend Border Size',
                                            min = 0, max = 100,
                                            value = 50, tooltips=TRUE,
                                            step=1, height="10px")))
                                ),
                                #Choosing to add descriptive stat info inside the plot
                                prettySwitch(
                                  "dpview",
                                  label = "Add Descriptive Information",
                                  value = FALSE, status = 'success',
                                  fill = TRUE),
                                conditionalPanel(
                                  condition ="input.dpview",
                                  div(style = "border:1px solid; border-radius: 10px;
                                    padding:10px;",
                                      selectInput(
                                        'dpviewInfo',
                                        label =  "Info Type",
                                        choices = c(
                                          "Mean" = 'mean',
                                          "Median" = 'median',
                                          "Sample Size" = 'count',
                                          "Std. Dev." = 'sd',
                                          "Std. Err. of Mean" = 'sem'
                                        ),
                                        selected = 'count'
                                      ),
                                      #Desc stat info text-size
                                      div(id='sliderstyle',
                                          noUiSliderInput(
                                            'dpviewSize',
                                            label = 'Text Size',
                                            min = 3, max = 12,
                                            value = 5, tooltips=TRUE,
                                            step=1, height="10px")),
                                      #Desc stat info text markdown style
                                      checkboxGroupButtons(
                                        'dpviewMD',
                                        label = NULL,
                                        choiceNames = c("<b>B</b>",
                                                        "<i>i</i>"),
                                        choiceValues = c("bold","italics"),
                                        selected = character(0),
                                        individual = T,
                                        width = '100px',
                                        size = 'normal'
                                      ),
                                      #Desc stat info number decimal point limit
                                      div(id='sliderstyle',
                                          noUiSliderInput(
                                            'dpviewDecmP',
                                            label = 'Decimal Point Length',
                                            min = 0, max = 5,
                                            value = 2, tooltips=TRUE,
                                            step=1, height="10px")),
                                      #Desc stat info position
                                      div(id='sliderstyle',
                                          noUiSliderInput(
                                            'dpviewPos',
                                            label = 'Text Vertical Position',
                                            min = 1, max = 100,
                                            value = 38, tooltips=TRUE,
                                            step=1, height="10px")))
                                  
                                )
                              ),
                              
                              ### Accordion Options for Plot Title ###
                              
                              accordion_panel(
                                title = 'Customize Plot Title',
                                #Edit Plot title
                                textInput('plotTitle', 'Plot Title',
                                          placeholder = 'Type plot title',
                                          value = NA),
                                #Choosing to add special character in plot title text
                                checkboxInput('chksymbolTit', label =
                                                "Add Special Characters", value = F),
                                #Renders special character options
                                uiOutput('showsymbolTit'),
                                #Choosing to add markdown style to plot title text
                                checkboxInput('markdownTit', label='Add Markdown', value = F),
                                #Renders markdown options
                                uiOutput('showMarkdownTit'),
                                #Choosing title horizontal position
                                radioGroupButtons(
                                  'titlePos', 'Text Alignment',
                                  choices = starHicon,
                                  selected = 'center',
                                  size = 'sm'
                                ),
                                #Choosing title vertical position
                                div(id='sliderstyle',
                                    noUiSliderInput(
                                      'verAlign',
                                      label = 'Vertical alignment',
                                      min = 1, max = 100,
                                      value = 25, tooltips=TRUE,
                                      step=1, height="10px")),
                                #Box-width of plot title
                                div(id='sliderstyle',
                                    noUiSliderInput(
                                      'bWidthTitle',
                                      label = 'Title Box Width',
                                      min = 50, max = 100,
                                      value = 100, tooltips=TRUE,
                                      step=1, height="10px")),
                                #Box-padding for plot title
                                div(id='sliderstyle',
                                    noUiSliderInput(
                                      'padTitle',
                                      label = 'Title Box Padding',
                                      min = 0, max = 100,
                                      value = 20, tooltips=TRUE,
                                      step=1, height="10px")),
                                #Box line-width for plot title
                                div(id='sliderstyle',
                                    noUiSliderInput(
                                      'lineTitle',
                                      label = 'Title Box Border',
                                      min = 0, max = 100,
                                      value = 0, tooltips=TRUE,
                                      step=1, height="10px")),
                              ),
                              
                              ### Accordion Options for Plot Axes ###
                              
                              accordion_panel(
                                title = 'Customize Axes',
                                #Y-Axis Title
                                textInput(
                                  "aytitle",
                                  label = "Y-Axis Title",
                                  placeholder = "Type axis title",
                                  value = c("Y-Axis")),
                                #Choosing to add special character in Y-axis title
                                checkboxInput('chksymbolY', label =
                                                "Add Special Characters", value = F),
                                #Choosing to add markdown style in Y-axis title
                                checkboxInput('markdownY', label = 
                                                'Add Markdown', value = F),
                                #Renders options for special characters (Y-axis)
                                uiOutput('showsymbolY'),
                                #Renders options for markdown styles (Y-axis)
                                uiOutput('showMarkdownY'),
                                #X-Axis Title
                                textInput(
                                  "axtitle",
                                  label = "X-Axis Title",
                                  placeholder = "Type axis title",
                                  value = NA),
                                #Choosing to add special character in X-axis title
                                checkboxInput('chksymbolX', label =
                                                "Add Special Characters", value = F),
                                #Choosing to add markdown style in X-axis title
                                checkboxInput('markdownX', label = 
                                                'Add Markdown', value = F),
                                #Renders options for special characters (X-axis)
                                uiOutput('showsymbolX'),
                                #Renders options for markdown styles (X-axis)
                                uiOutput('showMarkdownX'),
                                #X-Axis rotation
                                div(id='sliderstyle',
                                    noUiSliderInput(
                                      'Xrotate',
                                      label = 'X-Axis Rotation',
                                      min = 0, max = 90,
                                      value = 0, tooltips=TRUE,
                                      step=5, height="10px")),
                                #Choosing to reverse X-axis data columns
                                prettySwitch(
                                  'reverseX',
                                  'Reverse X-Axis',
                                  value=F, status = 'success',
                                  fill=T
                                ),
                                #Choosing Y-Axis Log transformation
                                radioGroupButtons(
                                  "logscale",
                                  label = "Y-Axis Log Scale",
                                  choices = c(
                                    "Default" = "none",
                                    "Log<sub>10</sub>" = "log10",
                                    "Log&#8322" = "log2"
                                  ), size='sm'),
                                #Choosing Y-axis numerical display type
                                radioGroupButtons(
                                  'labelY',
                                  label = "Y-Axis Number Display",
                                  choices = c('Default',
                                              'Scientific'),
                                  size = 'sm'
                                ),
                                #Choosing the Y-Axis limits
                                p('Y-Axis Limits', style="font-weight:600"),
                                div(
                                  style="display:flex; flex-direction:row;
                                justify-content:space-between; align-items: center;
                                width:100%; gap:1em;",
                                  #Minimum Y axis
                                  numericInputIcon(
                                    'minY',
                                    "Min",
                                    value = NA, min = NULL, max = NULL,
                                  ),
                                  #Maximum Y axis
                                  numericInputIcon(
                                    'maxY',
                                    'Max',
                                    value = NA, min = NULL, max = NULL)),
                                #Options for adding Y-axis break (To be added later)
                                # prettySwitch(
                                #   'addYBreak',
                                #   'Y-Axis Break', value = F,
                                #   fill = T, status = 'success'
                                # ),
                                # conditionalPanel(
                                #   # Y-Axis Break options
                                #   condition = "input.addYBreak",
                                #   div(style='padding:10px; border:1px solid; border-radius:5px;',
                                #     p('Y-Axis Break Limits', style="font-weight:600"),
                                #     div(
                                #       style="display:flex; flex-direction:row;
                                #   justify-content:space-between; align-items: center;
                                #   width:100%; gap:1em;",
                                #       
                                #       numericInputIcon(
                                #         'minYBreak',
                                #         "Min",
                                #         value = NA, min = NULL, max = NULL),
                                #       numericInputIcon(
                                #         'maxYBreak',
                                #         'Max',
                                #         value = NA, min = NULL, max = NULL)),
                                #     radioGroupButtons(
                                #       'breakScale',
                                #       'Plot Spread Size',
                                #       choices = c('Compact' = 'fixed',
                                #                   'Equal' = 'free'),
                                #       
                                #     ),
                                #     div(id='sliderstyle',
                                #         noUiSliderInput(
                                #           'breakGap',
                                #           label = 'Gap Distance',
                                #           min = 0, max = 100,
                                #           value = 50, tooltips=TRUE,
                                #           step=1, height="10px")))
                                # ),
                                
                                #Axes line-width
                                div(id='sliderstyle',
                                    noUiSliderInput(
                                      'axisline',
                                      label = 'Axes Line Width',
                                      min = 0, max = 100,
                                      value = 50, tooltips=TRUE,
                                      step=1, height="10px")),
                                #Major tick length
                                div(id='sliderstyle',
                                    noUiSliderInput(
                                      'ticklength',
                                      label = 'Axes Tick Length',
                                      min = 0, max = 100, value = 30,
                                      step = 1, tooltips = TRUE, height = "10px"
                                    )),
                                #Major tick width
                                div(id='sliderstyle',
                                    noUiSliderInput(
                                      'tickwidth',
                                      label = 'Axes Tick Width',
                                      min = 0, max = 100, value = 50,
                                      step = 1, tooltips = TRUE, height = "10px"
                                    )),
                                #Choosing to reverse Y-axis
                                prettySwitch(
                                  'flipPlot',
                                  'Flip Axes', value = F,
                                  status = "success", fill = T
                                )
                              ),
                              
                              ### Accordion Options for Universal Font Style ###
                              
                              accordion_panel(
                                title = 'Customize Font',
                                #Choosing type face
                                pickerInput(
                                  'font', 'Select Font Family',
                                  choices = c(
                                    'Arial', 'Bookman Old Style', 'Calibri',
                                    'Candara', 'Century Gothic', 'Corbel',
                                    'Garamond', 'Georgia', 'Lucida Bright',
                                    'Lucida Sans', 'Microsoft Sans Serif',
                                    'Segoe UI', 'Tahoma',
                                    'Times New Roman', 'Verdana'
                                  )),
                                #Font-size for plot title
                                div(h4('Plot Title'),
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'plotFont',
                                          label = 'Size: Plot Title',
                                          min = 5, max = 50,
                                          value = 25, tooltips=TRUE,
                                          step=1, height="10px"))
                                ),
                                div(h4('X-Axis'),
                                    #Font-size for X-axis texts
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'Xfontcol',
                                          label = 'Size: Axis Text',
                                          min = 5, max = 50,
                                          value = 18, tooltips=TRUE,
                                          step=1, height="10px")),
                                    #Font-size for X-axis title
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'Xfontsz',
                                          label = 'Size: Axis Title',
                                          min = 5, max = 50,
                                          value = 18, tooltips=TRUE,
                                          step=1, height="10px")),
                                    #Choosing linebreak width for X-axis title
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'Xlinebreak',
                                          label = 'Title Linebreak',
                                          min = 10, max = 100,
                                          value = 75, tooltips=TRUE,
                                          step=1, height="10px"))),
                                div(h4('Y-Axis'),
                                    #Font-size for Y-axis texts
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'Yfontcol',
                                          label = 'Size: Axis Text',
                                          min = 5, max = 50,
                                          value = 18, tooltips=TRUE,
                                          step=1, height="10px")),
                                    #Font-size for Y-axis title
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'Yfontsz',
                                          label = 'Size: Axis Title',
                                          min = 5, max = 50,
                                          value = 18, tooltips=TRUE,
                                          step=1, height="10px")),
                                    #Choosing linebreak width for Y-axis title
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'Ylinebreak',
                                          label = 'Title Linebreak',
                                          min = 10, max = 100,
                                          value = 75, tooltips=TRUE,
                                          step=1, height="10px")))
                              ),
                              
                              ### Accordion Options for Shape Theme ###
                              
                              accordion_panel(
                                title = 'Customize Theme',
                                #Choosing type of theme generation for ungrouped data
                                conditionalPanel(
                                  condition = "input.dataGroup == false &&
                                input.askPlotTypeII != 'jitter'",
                                  #Type of theme options 
                                  pickerInput(
                                    "choosetheme",
                                    label = "Select Theme Generator",
                                    choices = c(
                                      "Default" = "default",
                                      "Preset Shades" =
                                        'preset',
                                      "Make Gradient" =
                                        'gradient',
                                      "Select Individual" =
                                        'palette'
                                    )
                                  ),
                                  #Preset themes
                                  conditionalPanel(
                                    condition = "input.choosetheme == 'preset'",
                                    pickerInput(
                                      "boxtheme",
                                      label = "Preset Themes",
                                      choices = c(
                                        'Purrrrple' = 'purples',
                                        'The Teal Deal' = 'greens',
                                        'Shred of Green' = 'greens2',
                                        'Cherry Blossom' = 'pinks',
                                        'Center of the Earth' = 'oranges',
                                        'Cold-feet' = 'blues',
                                        '22K Gold' = 'golds',
                                        'Desaturated Rainbow' = 'rainbows',
                                        'Seasonal Sky' = 'season',
                                        'Extreme Emotions' = 'heatmap',
                                        'Colorblind Friendly' = 'colorblind',
                                        'London Weather' = 'greys'
                                      ),
                                      choicesOpt = list(style = bgColor)
                                    )
                                  ),
                                  #Gradient theme generation
                                  conditionalPanel(
                                    condition = "input.choosetheme == 'gradient'",
                                    colorPickr(
                                      'grad1', "Color 1", selected = '#EEE1EF',
                                      pickr_width = '20%'),
                                    colorPickr(
                                      'grad2', "Color 2", selected = '#554994',
                                      pickr_width = '20%')
                                  ),
                                  #Renders color inputs for individual selection
                                  conditionalPanel(
                                    condition = "input.choosetheme == 'palette'",
                                    uiOutput('coltabsOut')
                                  ),
                                  #Choosing border shade for the shapes
                                  radioGroupButtons(
                                    "boxbordercol",
                                    label = "Border Color",
                                    choices = c(
                                      "Darker" = 'dark',
                                      "Lighter" = 'light'
                                    ),
                                    selected = 'dark', size = 'sm'
                                  ),
                                  #Value for light or dark shade
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'shadevalue',
                                        label = 'Border Shade',
                                        min = 0, max = 100, value = 100,
                                        tooltips = TRUE, step = 1, height = '10px')),
                                  #Shape transparency
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'shapeAlpha',
                                        label = 'Shape Opacity',
                                        min = 0, max = 100, value = 100,
                                        tooltips = TRUE, step = 1, height = '10px'))
                                ),
                                #Choosing type of theme generation for grouped data
                                conditionalPanel(
                                  condition = "input.dataGroup == true&&
                                input.askPlotTypeIIG != 'jitter'",
                                  #Type of theme options (currently limited)
                                  pickerInput(
                                    "choosethemeII",
                                    label = "Select Theme Generator",
                                    choices = c(
                                      "Default" = "defaultG",
                                      "Select Individual" =
                                        'paletteG'
                                    )
                                  ),
                                  #Choosing border shade for the shapes
                                  radioGroupButtons(
                                    "boxbordercolG",
                                    label = "Border Color",
                                    choices = c(
                                      "Darker" = 'dark',
                                      "Lighter" = 'light'
                                    ),
                                    selected = 'dark', size = 'sm'
                                  ),
                                  #Value for light or dark shade
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'shadevalueG',
                                        label = 'Border Shade',
                                        min = 0, max = 100, value = 100,
                                        tooltips = TRUE, step = 1, height = '10px')),
                                  #Shape transparency
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'shapeAlphaG',
                                        label = 'Shape Opacity',
                                        min = 0, max = 100, value = 100,
                                        tooltips = TRUE, step = 1, height = '10px'))
                                ),
                                #Renders color inputs for individual selection
                                conditionalPanel(
                                  condition = "input.choosethemeII == 'paletteG'",
                                  uiOutput('coltabsOutG')
                                ),
                                #Choosing grayscale pass for contrast check 
                                radioGroupButtons(
                                  "grayscale",
                                  label = "Display Contrast",
                                  choices = c('Original'="No", 'Grayscale'="Yes"),
                                  size = 'sm'),
                                
                                #Choosing data point colors for ungrouped data
                                conditionalPanel(
                                  condition = "input.dataGroup == false",
                                  conditionalPanel(
                                    condition = "['box', 'viopoint', 'jitter'].includes(input.askPlotTypeII)",
                                    #Point color transparency
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'pointAlpha',
                                          label = 'Point Opacity',
                                          min = 0, max = 100, value = 100,
                                          tooltips = TRUE, step = 1, height = '10px'))
                                  ),
                                  #Choosing data point border color theme
                                  conditionalPanel(
                                    condition = "['box', 'viopoint', 'jitter'].includes(input.askPlotTypeII)",
                                    pickerInput(
                                      inputId = 'dpcolor',
                                      label = "Datapoint Border Color",
                                      choices = c(
                                        "Default (Black)" = 'default',
                                        "Shape Color" = 'box',
                                        "Border Color" = 'border',
                                        "Make Gradient" = 'dpgradient',
                                        "Select Individual" = 'dppalette'
                                      )
                                    )
                                  ),
                                  #Choosing data point fill color theme
                                  conditionalPanel(
                                    condition = "['box', 'viopoint', 'jitter'].includes(input.askPlotTypeII)",
                                    pickerInput(
                                      inputId = 'dpfill',
                                      label = "Datapoint Fill Color",
                                      choices = c(
                                        "Default (Black)" = 'default',
                                        "Shape Color" = 'box',
                                        "Border Color" = 'border',
                                        "Make Gradient" = 'dpgradient',
                                        "Select Individual" = 'dppalette'
                                      )
                                    )
                                  ),
                                  conditionalPanel(
                                    condition = "input.dpcolor == 'dpgradient'",
                                    conditionalPanel(
                                      condition = "['box', 'viopoint', 'jitter'].includes(input.askPlotTypeII)",
                                      p("Select Gradient for Datapoint Outline", 
                                        style="font-weight:500;"),
                                      #Gradient theme generation for outline color for 
                                      #all plot types with data points
                                      div(style="display:inline-flex; flex-direction:row; gap:5px;",
                                          colorPickr(
                                            'dpgrad1', "Color 1", selected = '#EEE1EF',
                                            pickr_width = '20%'),
                                          colorPickr(
                                            'dpgrad2', "Color 2", selected = '#554994',
                                            pickr_width = '20%')
                                      )
                                    )
                                  ),
                                  conditionalPanel(
                                    condition = "input.dpfill == 'dpgradient'",
                                    conditionalPanel(
                                      condition = "['box', 'viopoint', 'jitter'].includes(input.askPlotTypeII)",
                                      p("Select Gradient for Datapoints", 
                                        style="font-weight:500;"),
                                      #Gradient theme generation for fill color for 
                                      #all plot types with data points
                                      div(style="display:inline-flex; flex-direction: row;
                                    gap: 5px;",
                                          colorPickr(
                                            'dpgradF1', "Color 1", selected = '#EEE1EF',
                                            pickr_width = '20%'),
                                          colorPickr(
                                            'dpgradF2', "Color 2", selected = '#554994',
                                            pickr_width = '20%')
                                      )
                                    )
                                  ),
                                  conditionalPanel(
                                    condition = "input.dpcolor == 'dppalette'",
                                    conditionalPanel(
                                      condition ="['box', 'viopoint', 'jitter'].includes(input.askPlotTypeII)",
                                      p("Select Colors for Datapoint Outline", 
                                        style="font-weight:500;"),
                                      #Renders color inputs for outline color
                                      #for all plot types with data points (includes grouped data)
                                      uiOutput('dpcoltabsOut')
                                    )
                                  ),
                                  conditionalPanel(
                                    condition = "input.dpfill == 'dppalette'",
                                    conditionalPanel(
                                      condition ="['box', 'viopoint', 'jitter'].includes(input.askPlotTypeII)",
                                      p("Select Colors for Datapoints", 
                                        style="font-weight:500;"),
                                      #Renders color inputs for fill color
                                      #for all plot types with data points (includes grouped data)
                                      uiOutput('dpfilltabsOut')
                                    )
                                  )
                                ),
                                #Choosing data point colors for grouped data
                                conditionalPanel(
                                  condition = "input.askPlotTypeIIG == 'box' &&
                                input.dataGroup == true",
                                  #Choosing theme for data points' outline color
                                  pickerInput(
                                    "dpcolorG",
                                    label = "Datapoint Outline Color",
                                    choices = c(
                                      "Default (Black)" = 'defaultG',
                                      "Border Color" =
                                        'borderG'
                                    ),
                                    selected = "default"
                                  ),
                                  #Choosing theme for data points' fill color
                                  pickerInput(
                                    "dpfillG",
                                    label = "Datapoint Fill Color",
                                    choices = c(
                                      "Default (Black)" = 'defaultG',
                                      "Border Color" =
                                        'borderG'
                                    ),
                                    selected = "default"
                                  )
                                )
                              ),
                              
                              ### Accordion Options for Data Significance Annotation ###
                              
                              accordion_panel(
                                title= "Customize Plot Annotations",
                                value = "annotePanel",
                                tagList(
                                  #Renders list of comparisons to put significance brackets against
                                  uiOutput('statGroups'),
                                  #Button to add or remove brackets
                                  actionButton('addBrackets','Add Brackets to Plot', width='100%',
                                               icon = icon('bars-staggered'), class = 'btn-primary'),
                                  br(),
                                  #Options to customize brackets
                                  div(style = "border:1px solid; border-radius:10px; padding:10px;",
                                      #Bracket tip-length type
                                      radioGroupButtons(
                                        'askTipType',
                                        'Bracket Type',
                                        choices = c('Line'='line',
                                                    'Short Bracket'='short',
                                                    'Long Bracket'='long'),
                                        selected = 'short',
                                        size = 'sm'
                                      ),
                                      #Vertical position of the brackets
                                      div(id='sliderstyle',
                                          noUiSliderInput(
                                            'firstBrack',
                                            label = 'Vertical Positioning',
                                            min = 1, max = 100,
                                            value = 50, tooltips=TRUE,
                                            step=1, height="10px")),
                                      #Inter-bracket distances
                                      div(id='sliderstyle',
                                          noUiSliderInput(
                                            'distWidth',
                                            label = 'Inter-bracket Distance',
                                            min = 0, max = 100,
                                            value = 15, tooltips=TRUE,
                                            step=1, height="10px")),
                                      #Top-margin for brackets
                                      div(id='sliderstyle',
                                          noUiSliderInput(
                                            'topMargin',
                                            label = 'Space Around Brackets',
                                            min = 0, max = 100,
                                            value = 25, tooltips=TRUE,
                                            step=1, height="10px")),
                                      conditionalPanel(
                                        condition = "input.askTipType=='short'",
                                        #Tip-length if short tips selected
                                        div(id='sliderstyle',
                                            noUiSliderInput(
                                              'tipLength',
                                              label = 'Tip Length',
                                              min = 10, max = 100,
                                              value = 40, tooltips=TRUE,
                                              step=1, height="10px"))),
                                      #Gaps between brackets in one layer
                                      div(id='sliderstyle',
                                          noUiSliderInput(
                                            'gapWidth',
                                            label = 'Gap Distance',
                                            min = 10, max = 100,
                                            value = 30, tooltips=TRUE,
                                            step=1, height="10px")),
                                      #Line-width of brackets
                                      div(id='sliderstyle',
                                          noUiSliderInput(
                                            'bracWidth',
                                            label = 'Line Width',
                                            min = 1, max = 100,
                                            value = 65, tooltips=TRUE,
                                            step=1, height="10px")),
                                      #Line-color of brackets
                                      colorPickr(
                                        'bracCol',
                                        label = 'Line Color',
                                        selected = '#000000',
                                        pickr_width = "20%"
                                      )),
                                  br(),
                                  #Options for P value and significance star texts
                                  div(style = "border:1px solid; border-radius:10px; padding:10px;",
                                      #Significance display type
                                      radioGroupButtons(
                                        'askPvalType',
                                        'Significance Report',
                                        choices = c('Raw P Value'='raw',
                                                    'Asterisks'='star',
                                                    'Both' = 'both'),
                                        selected = 'star',
                                        size = 'sm'
                                      ),
                                      #Journal style for significance display
                                      radioGroupButtons(
                                        'askPvalStyle',
                                        'Style',
                                        choices = c('Default'='default',
                                                    'APA'='apa',
                                                    'NEJM'= 'nejm'),
                                        selected = 'default',
                                        size = 'sm'
                                      ),
                                      #Significance text font size
                                      div(id='sliderstyle',
                                          noUiSliderInput(
                                            'pvalSize',
                                            label = 'Text Size',
                                            min = 15, max = 45, value = 25,
                                            tooltips = TRUE, step = 1, height = "10px"
                                          )),
                                      #Significance text vertical position
                                      div(id='sliderstyle',
                                          noUiSliderInput(
                                            'pvalTVpos',
                                            label = 'Text Verical Position',
                                            min = 1, max = 100,
                                            value = 50,
                                            tooltips = TRUE, step = 1, height = "10px"
                                          )),
                                      div(style='display:inline-flex; width:100%; flex-direction:row; align-items:flex-start; justify-content:space-between;',
                                          #Significance text horizontal position
                                          radioGroupButtons(
                                            'pvalHpos',
                                            label='',
                                            choices = starHicon,
                                            selected = 'center',
                                            size = 'sm'
                                          ),br(),
                                          #Significance text top/bottom position to the brackets
                                          radioGroupButtons(
                                            'pvalVpos',
                                            label='',
                                            choices = starVicon,
                                            selected = 'top',
                                            size = 'sm'
                                          )
                                      ),
                                      #Significance text color
                                      colorPickr(
                                        'pvalCol',
                                        label = 'Text Color',
                                        selected = '#000000',
                                        pickr_width = "20%"
                                      )),
                                  br()
                                )
                              )),
                  ),
                  
                  ### Dropdown menu options in the Graph View Tab for general settings###
                  
                  dropdownButton(
                    conditionalPanel(
                      condition = "input.dataGroup == true",
                      #Options to select data plot type for grouped data
                      pickerInput('askPlotTypeIIG',
                                  label = 'Plot Type',
                                  choices = plotG,
                                  choicesOpt = list(content = plotG_img),
                                  selected = 'violin', width = '100%'),
                      #Choosing to switch grouping parameter
                      prettySwitch('grpSwitch', 'Switch Groups', 
                                   status = 'success', value = F, fill = T,
                                   width = "100%")
                    ), conditionalPanel(
                      condition = "input.dataGroup == false",
                      #Options to select data plot type for ungrouped data
                      pickerInput('askPlotTypeII',
                                  label = 'Plot Type',
                                  choices = plotUG,
                                  choicesOpt = list(content = plotUG_img),
                                  selected = 'violin', width = '100%')
                    ),
                    #Canvas color options
                    pickerInput('canvasTheme',
                                label = "Canvas Color",
                                choices = c("Light" = '#FFFFFF00',
                                            "Grey" = '#E6E6E6',
                                            "Dark"= '#535353'),
                                selected = '#FFFFFF00', width = "100%"),
                    div(
                      #Edit Plot-width
                      numericInput('width', label = "Plot Width", 
                                   min = 100, max = 800, width = 150,
                                   value = 500, updateOn = 'blur'),
                      #Button to lock plot aspect ratio (width/height)
                      div(prettyToggle('lockRatio', label_on = NULL, label_off = NULL, 
                                       icon_on = icon('link'), icon_off = icon('link-slash'), 
                                       status_on = 'primary', status_off = 'warning',
                                       fill = F, plain = F, bigger = T, thick = F, 
                                       shape='round', inline = T, width = '0px'),
                          style="width:10px; height:0; margin-right:10px;margin-top:38px;"),
                      #Edit Plot-height
                      numericInput('height', label = "Plot Height", width = 150,
                                   min = 100, max = 800,
                                   value = 400, updateOn = 'blur'),
                      style = "display:inline-flex; flex-direction:row; gap: 5px; position:relative;"),
                    div(
                      #Plot zoom-in button
                      actionButton(
                        inputId = "zoomIn",
                        label = "Zoom In",
                        icon = icon("magnifying-glass-plus"),
                        size = "100%",
                        title = "Increase Plot Size"
                      ),
                      #Plot zoom-out button
                      actionButton(
                        inputId = "zoomOut",
                        label = "Zoom Out",
                        icon = icon("magnifying-glass-minus"),
                        size = "100%",
                        title = "Decrease Plot Size"
                      ),
                      #Plot zoom reset to default button
                      actionButton('resetSize','Reset', icon = icon('rotate-left'), title = "Reset Size to Default"),
                      style = "display:inline-flex; flex-direction:row; gap: 5px; position:relative;"  
                    ),
                    #Button to open modal settings for saving plot 
                    actionButton('saveBtn', 'Save Plot As...', icon = icon("floppy-disk"),
                                 width = '100%', title = "Open Save Settings", disabled = TRUE),
                    #Action button triggering download to save plot settings
                    actionButton("savesetting_proxy", "Export Settings...", 
                                 icon = icon("gear"), title = "Save your plot settings", width = "100%",
                                 disabled = TRUE),
                    conditionalPanel(
                      condition = "input.sub_ui != 'Demo Data'",
                      #Plot theme setting file upload link
                      fileInput("usesetting", label = NULL, buttonLabel = "Import Settings...", accept = c(".xlsx"), width = "100%"),
                      #Button to upload plot theme setting file
                      actionButton("reuseset", "Reuse Settings", icon=icon('file-import'),
                                   title = "Upload Previous Settings", width = "100%"
                      )),
                    inputId = "menuBtn",
                    circle = F, label = "Menu",
                    icon = icon ("bars"), size = 'sm', margin = "10px"
                  ),
                  ## Renders main plot on screen ##
                  uiOutput('graph_main_content'),
                  
                  #Download button to save plot settings
                  downloadButton("savesetting", label=NULL)
                  
                ))),
    nav_panel(title = "Statistics",
              card(
                card_header("Stat Analysis View"),
                layout_sidebar(
                  sidebar = sidebar(
                    width=350,
                    h4("Experimental Settings"),
                    #Renders options for selecting statistical tests
                    uiOutput('testInput'),
                    #Renders button for submitting selected statistical
                    #tests for running analysis
                    uiOutput('submitAnalysis'),
                    #Renders download button to export full statistics report
                    uiOutput('statDnld')
                  ), 
                  
                  ## Renders full statistical report on screen ##
                  uiOutput('stat_main_content')
                  
                )
              )
    ),
    nav_panel(title = "About",
              HTML("<img src = 'www/app_logo_color.png' width='250' height='250'>"),
              div(
                HTML("<h3>About SEN'sable Plotting</h3>"),
                HTML("<b>SEN'sable Plotting</b> is a lightweight, open-source Shiny app for
              visualizing and statistically analyzing discrete or categorical
              data &#x2014; designed as a free, user-friendly alternative to paid softwares
              like GraphPad Prism."),
                p("Built with students, and early-career researchers
              in mind, it offers an intuitive, no-code interface to create", strong("publication-ready
              plots and statistical reports"), "without any knowledge of R coding."),
                h4("Why this app?"),
                HTML("<p>R is a powerful language for statistics and visualization,
              backed by base functions and peer-reviewed packages. However,
              its learning curve can be a barrier. SEN'sable Plotting removes that
              barrier by providing a point-and-click experience while leveraging R's
              robust capabilities under the hood.</p>"),
                h5("Core Packages"),
                HTML(
                  "<ul><li><b>Framework:</b> <code>shiny</code> (with <code>shinyBS</code>,
                <code>shinyjs</code>, <code>shinywidgets</code>, <code>shinycssloaders</code>)</li>
              <li><b>Data handling:</b> <code>openxlsx</code>, <code>DT</code>,
              <code>tidyverse</code> (<code>dplyr</code>, <code>tidyr</code>, <code>stringr</code>, <code>scales</code>),
              <code>broom</code>, <code>rclipboard</code>, <code>readr</code></li>
              <li><b>Plotting:</b> <code>ggplot2</code> + extensions (<code>ggbeeswarm</code>,
              <code>ggdist</code>, <code>ggnewscale</code>, <code>ggtext</code>, <code>qqplotr</code>)</li>
              <li><b>Themes & UI:</b> <code>colorspace</code>, <code>bslib</code>,
              <code>waiter</code>, <code>patchwork</code>, <code>extrafont</code>, <code>fontawesome</code></li>
              <li><b>Statistics:</b> <code>rstatix</code>, <code>DescTools</code>, <code>lme4</code>,
              <code>emmeans</code>, <code>PMCMRplus</code>, <code>car</code>, <code>ARTool</code> (plus base stats)</li></ul>
              All packages are open-source and freely available. Full
              session info and dependencies are in the repo for reproducibility.
              "),
                br(),
                br(),
                h4("Quick Usage Guide"),
                HTML(
                  "<ol><li><b>Upload data</b> (File Upload tab): Import Excel (multi-sheet supported)
              or paste directly; select sheet and upload.</li>
              <li><b>Choose plot type</b> (Plot Type dropdown): Single (Box-Whisker,
              Violin, Raincloud, Jitter, Bar) or grouped (Box-Whisker and Violin) plots.</li>
              <li><b>Customize & Save</b> (Graph tab): Adjust shapes, themes, fonts, colors, labels via collapsible panels.
              Download high-resolution plots (PNG, TIFF, SVG, etc., selectable DPI).</li>
              <li><b>Run statistics</b> (Statistics tab): Auto-detect test type
              (two-sample/multi-sample, parametric/non-parametric) or choose manually.
              Perform post-hoc comparisons and generate report.</li>
              <li><b>Download & annotate:</b> Export stat report (Excel). 
              Add customizable annotations (p-values, brackets, asterisks) directly
              to plots via the Graph tab.</li>
              <li><b>Reusable Settings:</b> Save selected settings for later use or
              import a setting (Excel) file to reuse previous settings to reproduce plots.</li>
              </ol>
              <b>&#9888;Important Disclaimer&#9888;</b><br>
              Statistical results are automated for convenience,
              but users should always verify test assumptions, selections, and 
              outputs using additional tools or expert consultation. This app
              is not a substitute for professional statistical advice.
              "),
                br(),br(),
                h5("Get Involved"),
                HTML("<p>SEN'sable Plotting is licensed under the <b>MIT License</b> (permissive open-source).</p>"),
                HTML('<ul><li><a href="https://github.com/sumitsen616/Sensabled" target="_blank">
            Source code</a></li>
                 <li><a href="https://github.com/sumitsen616/Sensabled/issues" target="_blank">Report bugs, request features, or contribute</a></li>
                 <li><a href="https://github.com/sumitsen616/Sensabled/tags" target="_blank">Release Notes</a></li>
                 </ul>
                 Feedback is very welcome. I actively maintain this tool and
                 appreciate your input to make it better!
                 '),
                br(),
                HTML("<p style='width: 100%; text-align:center; padding:10px;'><b>SEN'sable Plotting</b> v1.2.0 || &copy; Sumit Sen  (<script>document.write(new Date().getFullYear());</script>)</p>")
              ),
              style = "width:75%; padding:50px; margin:0 auto; ")
  )}
