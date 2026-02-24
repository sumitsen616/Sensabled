### Source Code for SEN'sabale Plotting App ###
### Author: Sumit Sen ###
### TIFR, Mumbai ###

##Loading Necessary Packages##
# options(shiny.trace = TRUE)
options(encoding = "UTF-8")
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(waiter)
library(shinyWidgets)
library(rJava)
library(openxlsx)
library(datasets)
library(tidyr)
library(dplyr)
library(DT)
library(stringr)
library(tidyverse)
library(remotes)
library(magrittr)
library(ggplot2)
library(ggbeeswarm)
library(ggnewscale)
library(ggdist)
library(ggtext)
library(ggbreak)
library(patchwork)
library(qqplotr)
library(scales)
library(extrafont)
library(fontawesome)
library(svglite)
library(colorspace)
library(colourpicker)
library(bsplus)
library(bslib)
library(DescTools)
library(rclipboard)
library(lme4)
library(emmeans)
library(PMCMRplus)
library(broom)
library(stats)
library(car)
library(rstatix)
library(ARTool)
library(xlsx)

## Loading screen theme setting
# waiter_set_theme(html = spin_2(), color = '#fcfcfc')

##CSS Custom Styles
css <- "
  body, html { margin:0; padding:0; }
  .noUi-tooltip { padding:1px!important; }
  .noUi-horizontal .noUi-handle { width: 34px!important; height: 12px!important; right: -17px!important; top: -1px!important; }
  .noUi-handle:before, .noUi-handle:after { height: 10px!important; top:-1px!important; }
  #sliderstyle label + div { margin-top:17px !important; padding:0 10px !important; }
  .bslib-sidebar-layout>.sidebar>.sidebar-content {padding-top:10px !important;}
  .genAcc{margin-top:-45px !important;}
  .d-flex{display: grid !important; grid-template-columns: repeat(auto-fill, 20px);}
  .d-flex label{border-radius:0px !important; padding:5px !important; display: flex !important; 
  flex-direction:row, align-content:center; justify-content: center;}
  #dpview{margin:10px !important;}
  .genAcc{transition:0.5s all !important;}
  .colourpicker input{padding:2px;}
  .form-group{margin-bottom:0.5rem !important;}
  #graphFinal{ display: flex; align-items:center; justify-content:flex-end; flex-direction:column; margin:auto;}

"
# datapoint symbol iconlist
iconlist <- c(
  "<i class='fa-solid fa-circle'></i>" = 21,        
  "<i class='fa-regular fa-circle'></i>" = 1,       
  "<i class='fa-solid fa-circle-xmark'></i>" = 13,  
  "<i class='fa-solid fa-square'></i>" = 22,
  "<i class='fa-regular fa-square'></i>" = 0,
  "<i class='fa-solid fa-square-plus'></i>" = 12,
  "<i class='fa-solid fa-caret-up'></i>" = 24,   
  "<i class='fa-solid fa-diamond'></i>" = 23,    
  "<i class='fa-solid fa-plus'></i>" = 3,          
  "<i class='fa-solid fa-xmark'></i>" = 4  
)
suggestIcon <- icon("circle-check", class = "text-warning")
starVicon <- c(
  "<i class='fa-solid fa-arrows-up-to-line'></i>" = 'bottom',
  "<i class='fa-solid fa-arrows-down-to-line'></i>" = 'top'
)
starHicon <- c(
  "<i class='fa-solid fa-align-left'></i>"='left',
  "<i class='fa-solid fa-align-center'></i>"='center',
  "<i class='fa-solid fa-align-right'></i>"='right'
)

#Creating a function to assign asterisk to P value significance intToUtf8(0x2731)
asterisk <- (function(x){
  astr <- data.frame()
  for (i in 1:length(x)){
    if (as.numeric(x[i]) > 0.05) {
      tempastr <- c('ns')
    } else if (as.numeric(x[i]) <= 0.05 && as.numeric(x[i]) >0.01) {
      tempastr <- c('*')
    } else if (as.numeric(x[i]) <= 0.01 && as.numeric(x[i]) >0.001) {
      tempastr <- c(paste0(rep('*', 2), collapse = ""))
    } else if (as.numeric(x[i]) <= 0.001 && as.numeric(x[i]) >0.0001) {
      tempastr <- c(paste0(rep('*', 3), collapse = ""))
    } else if (as.numeric(x[i]) <= 0.0001){
      tempastr <- c(paste0(rep('*', 4), collapse = ""))
    } 
    astr <- rbind(astr,tempastr)
  }
  return(astr)
})

#Creating a function to calculate rank for Mann-Whitney-Wilcox test
mwz <- function(x, y) {
  x <- na.omit(as.numeric(unlist(x)))
  y <- na.omit(as.numeric(unlist(y)))
  
  n_x <- length(x)
  n_y <- length(y)
  
  if (n_x == 0 || n_y == 0) {
    stop("One or both groups have no valid data after removing NA.")
  }
  
  all_vals <- c(x, y)
  ranks <- rank(all_vals, ties.method = "min")
  
  
  rank_x <- ranks[1:n_x]
  rank_y <- ranks[(n_x + 1):(n_x + n_y)]
  
  
  ranksum_x <- sum(rank_x)
  ranksum_y <- sum(rank_y)
  rankmean_x <- mean(rank_x)
  rankmean_y <- mean(rank_y)
  
  U_x <- n_x * n_y + n_x * (n_x + 1) / 2 - ranksum_x
  U_y <- n_x * n_y + n_y * (n_y + 1) / 2 - ranksum_y
  U <- min(U_x, U_y)
  
  mu <- n_x * n_y / 2
  sigma <- sqrt(n_x * n_y * (n_x + n_y + 1) / 12)
  z <- (U - mu) / sigma
  
  data.frame(
    RankSum_X = ranksum_x,
    RankSum_Y = ranksum_y,
    RankMean_X = rankmean_x,
    RankMean_Y = rankmean_y,
    U = U,
    z_Statistics = abs(z)
  )
}

#Creating a function to convert table matrix to list table

TabToVec <- function(df){
  dataP <- na.omit(as.vector(df$p.value))
  dataS <- na.omit(as.vector(df$statistic))
  colnam <- CombPairs(union(colnames(df$p.value),rownames(df$p.value)))
  
  finDF <- data.frame(paste(colnam$X1,'-',colnam$X2),dataS,dataP)
  colnames(finDF) <- c('Comparisons', 'T Statistic', 'Adjusted P Value')
  return(finDF)
}

#######################################
####### The UI Code Starts Here #######
#######################################

options(shiny.maxRequestSize = 250 * 1024^2) #Max file size to be uploaded is 250mb

ui <-page_navbar( 
  title = "SEN'sable Plotting",
  theme = bs_theme("zephyr", version= 5),
  header = tagList(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(css),
    fontawesome::fa_html_dependency()
    # autoWaiter()
    
  ),
  inverse = TRUE,
  nav_panel(title = "File Upload",
            card(
              card_header("Data View"),
              layout_sidebar(
                sidebar = sidebar(
                  # bg = "#EFEFEF",
                  width = 350,
                  h4("Uploaded Data Settings"),
                  #Uploading the file
                  fileInput('file', 'Choose XLSX File', accept = c('.xlsx')),
                  p('--or--', style='width=100%;margin-top:-20px; text-align:center;color:darkgrey'),
                  #Copy-Upload Pasted Data
                  textAreaInput('pasted_Data', label = 'Paste data in the box'),
                  actionButton('pasteBtn','Upload Pasted Data', icon = icon('paste')),
                  # p('--or--', style='width=100%;margin-top:0px; text-align:center;color:darkgrey'),
                  #Example data button
                  # actionButton('exampleFile', 'Upload Example File', icon = icon('file-import')),
                  #Choosing the sheet from the xlsx file
                  uiOutput('sheetnames'),
                  #Plot Type Options
                  uiOutput('fileupload'),
                  #Submit Button
                  uiOutput('uploadBtnShow'),
                  #Button to open update column header modal
                  uiOutput('colupdateBttn')
                ), 
                DTOutput('contents',fill = T)
              )
            )),
  nav_panel(title = "Graph",
            card(
              card_header("Graph View"),
              layout_sidebar(
                sidebar = sidebar(
                  width=350,
                  h4("Graph Settings"),
                  #Adding annotation on plot
                  tagList(
                    uiOutput('grpselect'),
                    uiOutput('askAnnotation')
                  ),
                  accordion(multiple = F, class = 'genAcc',
                            accordion_panel(
                              title = 'Customize Shapes',
                              # uiOutput('shapeType')
                              
                              ### Accordion Options for Box-Jitter Plot type ###
                              
                              conditionalPanel(
                                condition = "(input.askPlotTypeII == 'box' && input.dataGroup == false) || 
                                (input.askPlotTypeIIG == 'box' && input.dataGroup == true) ",
                                #Choosing box plot type
                                radioGroupButtons(
                                  "boxtype", "Change plot subtype",
                                  choices = c("Box-Jitter" = 'boxpoint',
                                              "Box" = 'boxOnly'),
                                  size = 'sm', selected = 'boxpoint'
                                ),
                                #Choosing to add Outliers or not
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
                                  # point scatter width
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'scatterBox',
                                        label = 'Point Scatter Width',
                                        min = 1, max = 100, value = 30,
                                        tooltips = TRUE, step = 1, height = '10px')),
                                  # point size 
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'pointsizeBox',
                                        label = 'Point Size',
                                        min = 1, max = 100, value = 30,
                                        tooltips = TRUE, step = 1, height = '10px')),
                                  
                                  switchInput('pointDistBox',
                                              label="Shape Similarity", value=FALSE,
                                              onLabel = "Independent",
                                              offLabel = "Same",
                                              size = 'small'),
                                  
                                  conditionalPanel(
                                    condition = "input.pointDistBox && !input.dataGroup",
                                    uiOutput("pointInpUIBox")
                                  ),
                                  conditionalPanel(
                                    condition = "input.pointDistBox && input.dataGroup",
                                    uiOutput("pointInpUIBoxG")
                                  ),
                                  conditionalPanel(
                                    condition = "!input.pointDistBox",
                                    radioGroupButtons(
                                      inputId = "pointshapeBox",
                                      label = "Point Shape", 
                                      choices = iconlist,
                                      justified = T, size = 'sm', selected = 21)
                                  ),
                                  pickerInput(
                                    'pointMethodBox', 'Jitter Pattern',
                                    choices = c('Random' = 'swarm','Square Grid' ='square',
                                                'Hexgrid' = 'hex', 'Centrally Symmetric' = 'center')),
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
                                  #Choosing vio plot type
                                  radioGroupButtons(
                                    "viotype", "Change plot subtype",
                                    choices = c("Violin-Box", "Violin"),
                                    selected = "Violin-Box", size = 'sm'
                                  ),
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'linewidthVio',
                                        label = 'Line Width',
                                        min = 0, max = 100, value = 40,
                                        tooltips = TRUE, step = 1, height = '10px')),
                                  conditionalPanel(
                                    condition = "input.viotype=='Violin-Box'",
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'boxWidthVio',
                                          label = 'Box Width',
                                          min = 10, max = 100, value = 30,
                                          tooltips = TRUE, step = 1, height = '10px')),
                                    radioGroupButtons(
                                      'boxColVio',
                                      'Box Color',
                                      choices = c(
                                        "Shape","Custom"
                                      ),
                                      size = 'sm'
                                    ),
                                    conditionalPanel(
                                      condition = "input.boxColVio == 'Custom' ",
                                      colorPickr('boxColCust',
                                                 label = NULL,
                                                 selected = '#FFFFFF',
                                                 pickr_width = '20%')
                                    )
                                  ),
                                  conditionalPanel(
                                    condition = "input.viotype== 'Violin'",
                                    prettySwitch(
                                      'askQuantLine', label = 'Add Quantile Lines',
                                      value = F, status = 'success', fill = T
                                    ),
                                    conditionalPanel(
                                      condition = "input.askQuantLine == true",
                                      div(id='sliderstyle',
                                          noUiSliderInput(
                                            'quantLineSize',
                                            label = 'Quantile Line Width',
                                            min = 1, max = 100, value = 50,
                                            tooltips = TRUE, step = 1, height = '10px'))
                                    )
                                  ),
                                  prettySwitch(
                                    'endTrim', label = 'Trim Ends', value = TRUE,
                                    status = 'success', fill = T
                                  ),
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
                              
                              conditionalPanel(
                                condition = "input.askPlotTypeII == 'jitter'",
                                div(id='sliderstyle',
                                    noUiSliderInput(
                                      'scatter',
                                      label = 'Point Scatter Width',
                                      min = 1, max = 100, value = 30,
                                      tooltips = TRUE, step = 1, height = '10px')),
                                # point size 
                                div(id='sliderstyle',
                                    noUiSliderInput(
                                      'pointsize',
                                      label = 'Point Size',
                                      min = 1, max = 100, value = 30,
                                      tooltips = TRUE, step = 1, height = '10px')),
                                
                                switchInput('pointDist',
                                            label="Shape Similarity", value=FALSE,
                                            onLabel = "Independent",
                                            offLabel = "Same",
                                            size = 'small'),
                                
                                conditionalPanel(
                                  condition = "input.pointDist== true",
                                  uiOutput("pointInpUI")
                                ),
                                conditionalPanel(
                                  condition = "input.pointDist== false",
                                  radioGroupButtons(
                                    inputId = "pointshape",
                                    label = "Point Shape", 
                                    choices = iconlist,
                                    justified = T, size = 'sm', selected = 21)
                                ),
                                pickerInput(
                                  'pointMethod', 'Jitter Pattern',
                                  choices = c('Random' = 'swarm','Square Grid' ='square',
                                              'Hexgrid' = 'hex', 'Centrally Symmetric' = 'center')),
                                pickerInput(
                                  'sum_typeJitter', 'Summary Statistics',
                                  choices = c(
                                    "Mean" = "mean_only",
                                    "Mean with SD" = "mean_sd",
                                    "Mean with SEM" = "mean_sem",
                                    "Median" = "median_only",
                                    "Median with 95% CI" = "median_ci"
                                  )),
                                div(id='sliderstyle',
                                    noUiSliderInput(
                                      'statLine',
                                      label = 'Stat Summ Line Width',
                                      min = 0, max = 100, value = 50,
                                      tooltips = TRUE, step = 1, height = '10px')),
                                div(id='sliderstyle',
                                    noUiSliderInput(
                                      'statWidth',
                                      label = 'Stat Summ Bar Width',
                                      min = 0, max = 100, value = 50,
                                      tooltips = TRUE, step = 1, height = '10px'))
                              ),
                              
                              ### Accordion Options for Raincloud Plot type ###
                              
                              conditionalPanel(
                                condition = "input.askPlotTypeII == 'viopoint'",
                                tagList(
                                  #Choosing box plot type
                                  radioGroupButtons(
                                    "slabSide", "Half-Violin Orientation",
                                    choices = c("Left" = 'left', "Right" = 'right'),
                                    selected = "right", size = 'sm'
                                  ),
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'slabDistance',
                                        label = 'Adjust Gap',
                                        min = 0, max = 100, value = 0,
                                        tooltips = TRUE, step = 10, height = '10px')),
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'linewidthRain',
                                        label = 'Line Width',
                                        min = 0, max = 100, value = 40,
                                        tooltips = TRUE, step = 1, height = '10px')),
                                  prettySwitch(
                                    'endTrimRain', label = 'Trim Ends', value = TRUE,
                                    fill = T, status = 'success'
                                  ),
                                  # Scatter Plot controls for Raincloud Plot
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'scatterRain',
                                        label = 'Point Scatter Width',
                                        min = 1, max = 100, value = 30,
                                        tooltips = TRUE, step = 1, height = '10px')),
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'pointsizeRain',
                                        label = 'Point Size',
                                        min = 1, max = 100, value = 30,
                                        tooltips = TRUE, step = 1, height = '10px')),
                                  switchInput('pointDistRain',
                                              label="Shape Similarity", value=FALSE,
                                              onLabel = "Independent",
                                              offLabel = "Same",
                                              size = 'small'),
                                  conditionalPanel(
                                    condition = "input.pointDistRain== true",
                                    uiOutput("pointInpUIRain")
                                  ),
                                  conditionalPanel(
                                    condition = "input.pointDistRain== false",
                                    radioGroupButtons(
                                      inputId = "pointshapeRain",
                                      label = "Point Shape", 
                                      choices = iconlist,
                                      justified = T, size = 'sm', selected = 21)
                                  ),
                                  pickerInput(
                                    'sum_typeRain', 'Summary Statistics',
                                    choices = c(
                                      "Mean" = "mean_only",
                                      "Mean with SD" = "mean_sd",
                                      "Mean with SEM" = "mean_sem",
                                      "Median" = "median_only",
                                      "Median with 95% CI" = "median_ci"
                                    )),
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'statLineRain',
                                        label = 'Stat Summ Linewidth',
                                        min = 0, max = 100, value = 50,
                                        tooltips = TRUE, step = 1, height = '10px')),
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'statWidthRain',
                                        label = 'Stat Summ Bar Width',
                                        min = 0, max = 100, value = 50,
                                        tooltips = TRUE, step = 1, height = '10px'))
                                )
                              ),
                              conditionalPanel(
                                condition = "input.askPlotTypeII == 'bar'",
                                tagList(
                                  #Choosing bar-width
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'barwidth',
                                        label = 'Box Width',
                                        min = 10, max = 100,
                                        value = 60, tooltips=TRUE,
                                        step=1, height="10px")),
                                  #Choosing bar linewidth
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'linewidthBar',
                                        label = 'Line Width',
                                        min = 0, max = 100, value = 40,
                                        tooltips = TRUE, step = 1, height = '10px')),
                                  radioGroupButtons(
                                    'barFunc', 'Bar Display',
                                    choices = c('Mean'= 'mean', 'Median' = 'median'),
                                    selected = 'mean', size = 'sm'
                                  ),
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
                                  conditionalPanel(
                                    condition = "input.barFunc == 'median'",
                                    pickerInput(
                                      'sum_typeBarMedian', 'Summary Statistics',
                                      choices = c(
                                        "Median" = "median_only",
                                        "Median with 95% CI" = "median_ci"
                                      ), selected = 'median_only')
                                  ),
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'statLineBar',
                                        label = 'Stat Summ Linewidth',
                                        min = 0, max = 100, value = 50,
                                        tooltips = TRUE, step = 1, height = '10px')),
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'statWidthBar',
                                        label = 'Stat Summ Bar Width',
                                        min = 0, max = 100, value = 50,
                                        tooltips = TRUE, step = 1, height = '10px')),
                                  radioGroupButtons(
                                    'askSide', 'Errorbar Direction',
                                    choices = c('Bothside', 'Outside'),
                                    size = 'sm', selected = 'Bothside'
                                  ),
                                  prettySwitch(
                                    'askJitter', 'Add Datapoints',
                                    status = 'success', value = F, fill = T
                                  ),
                                  conditionalPanel(
                                    condition = "input.askJitter == true",
                                    p("Note: Datapoints are appened from Jitter Plot.
                                      Customize datapoints at 'Jitter Plot' section.", 
                                      style="color: grey; font-size:13px;")
                                  )
                                  
                                )
                              ),
                              conditionalPanel(
                                condition = "input.askPaired == true || 
                                    input.askPairedssT == true",
                                prettySwitch(
                                  'askConnectLine',
                                  'Add Connecting Line',
                                  value = T, fill = T, status = 'success'
                                ),
                                div(style = "padding:10px; border:1px solid; border-radius:5px;",
                                    div(id='sliderstyle',
                                        noUiSliderInput(
                                          'connectLineSize',
                                          label = 'Line Width',
                                          min = 0, max = 100, value = 50,
                                          tooltips = TRUE, step = 1, height = '10px')),
                                    colorPickr(
                                      'connectLineCol',
                                      label = 'Line Colour',
                                      selected = '#000000',
                                      useAsButton = T,
                                      pickr_width = "20%"
                                    ),
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
                            uiOutput('statGroups'),
                            accordion_panel(
                              title = 'Customize Plot Area',
                              prettySwitch(
                                "plotThemeGrid",
                                label = "Add Grids",status = 'success', fill = T
                              ),
                              conditionalPanel(
                                condition = "input.plotThemeGrid==true ",
                                tagList(
                                  checkboxInput('majGrid','Major Grids', value = T),
                                  checkboxInput('minGrid','Minor Grids', value = T),
                                  checkboxGroupButtons('gridOpt', 'Choose Axis', 
                                                       choices = c('X','Y'),
                                                       selected = 'Y',
                                                       justified = T, size = 'sm'),
                                  radioGroupButtons('gridCol', 'Grids Colour',
                                                    choices = c('Black'= 'black',
                                                                'White'= 'white',
                                                                'Grey' = 'grey',
                                                                'Light Grey' = '#efefef'), size = 'sm'))),
                              prettySwitch(
                                "plotThemeBorder",
                                label = "Add Borders",status = 'success', fill = T
                              ),
                              prettySwitch(
                                "plotThemeBg",
                                label = "Add Background Colour",status = 'success', fill = T
                              ),
                              conditionalPanel(
                                condition = "input.plotThemeBg== true",
                                colorPickr('plotColor', label = 'Plot Background',
                                           selected = "#FFFFFF",
                                           pickr_width = '20%')),
                              conditionalPanel(
                                condition = "input.dataGroup == true",
                                textInput("legTitle", "Legend Title", value = "Treatments"),
                                selectInput("legPos", "Legend Position", 
                                            choices = c("Right"="right",
                                                        "Left"="left",
                                                        "Top"="top",
                                                        "Bottom"="bottom",
                                                        "Inside" = 'inside'),
                                            selected = "right"),
                                conditionalPanel(
                                  condition = "input.legPos == 'inside'",
                                  p("Click on the box to position the legend.",
                                    style= "font-weight:500; font-size:12px;"),
                                  plotOutput("legPosPlot", height = "150px",
                                             click = 'plot_click')
                                ),
                                div(id='sliderstyle',
                                    noUiSliderInput(
                                      'legTextSize',
                                      label = 'Legend Text Size',
                                      min = 5, max = 30,
                                      value = 12, tooltips=TRUE,
                                      step=1, height="10px")),
                                div(id='sliderstyle',
                                    noUiSliderInput(
                                      'legTitleSize',
                                      label = 'Legend Title Size',
                                      min = 5, max = 35,
                                      value = 14, tooltips=TRUE,
                                      step=1, height="10px")),
                                div(id='sliderstyle',
                                    noUiSliderInput(
                                      'legSize',
                                      label = 'Legend Key Size',
                                      min = 50, max = 100,
                                      value = 50, tooltips=TRUE,
                                      step=1, height="10px")),
                                div(id='sliderstyle',
                                    noUiSliderInput(
                                      'legBorderSize',
                                      label = 'Legend Border Size',
                                      min = 0, max = 100,
                                      value = 50, tooltips=TRUE,
                                      step=1, height="10px"))
                              ),
                              div(style = "margin:10px;",switchInput(
                                "dpview",
                                "Datapoints Count",
                                # choices = c('Remove' = 0, 'Add' =5),
                                onLabel = 'Add', offLabel = 'Remove',
                                onStatus = 'success', offStatus = 'danger', size = 'small')
                              )
                              
                            ),
                            accordion_panel(
                              title = 'Customize Plot Title',
                              textInput('plotTitle', 'Plot Title',
                                        placeholder = 'Type plot title',
                                        value = NA),
                              checkboxInput('chksymbolTit', label =
                                              "Add Special Characters", value = F),
                              uiOutput('showsymbolTit'),
                              checkboxInput('markdownTit', label='Add Markdown', value = F),
                              uiOutput('showMarkdownTit'),
                              radioGroupButtons(
                                'titlePos', 'Text Alignment',
                                choices = starHicon,
                                selected = 'center',
                                size = 'sm'
                              ),
                              div(id='sliderstyle',
                                  noUiSliderInput(
                                    'verAlign',
                                    label = 'Vertical alignment',
                                    min = 1, max = 100,
                                    value = 25, tooltips=TRUE,
                                    step=1, height="10px")),
                              div(id='sliderstyle',
                                  noUiSliderInput(
                                    'bWidthTitle',
                                    label = 'Title Box Width',
                                    min = 50, max = 100,
                                    value = 100, tooltips=TRUE,
                                    step=1, height="10px")),
                              div(id='sliderstyle',
                                  noUiSliderInput(
                                    'padTitle',
                                    label = 'Title Box Padding',
                                    min = 0, max = 100,
                                    value = 20, tooltips=TRUE,
                                    step=1, height="10px")),
                              div(id='sliderstyle',
                                  noUiSliderInput(
                                    'lineTitle',
                                    label = 'Title Box Border',
                                    min = 0, max = 100,
                                    value = 0, tooltips=TRUE,
                                    step=1, height="10px")),
                            ),
                            accordion_panel(
                              title = 'Customize Axes',
                              #Y-Axis Title
                              textInput(
                                "aytitle",
                                label = "Y-Axis Title",
                                placeholder = "Type axis title",
                                value = c("Y-Axis")),
                              checkboxInput('chksymbolY', label =
                                              "Add Special Characters", value = F),
                              checkboxInput('markdownY', label='Add Markdown', value = F),
                              uiOutput('showsymbolY'),
                              uiOutput('showMarkdownY'),
                              #X-Axis Title
                              textInput(
                                "axtitle",
                                label = "X-Axis Title",
                                placeholder = "Type axis title",
                                value = NA),
                              checkboxInput('chksymbolX', label =
                                              "Add Special Characters", value = F),
                              checkboxInput('markdownX', label='Add Markdown', value = F),
                              uiOutput('showsymbolX'),
                              uiOutput('showMarkdownX'),
                              
                              #X-Axis rotation
                              div(id='sliderstyle',
                                  noUiSliderInput(
                                    'Xrotate',
                                    label = 'X-Axis Rotation',
                                    min = 0, max = 90,
                                    value = 0, tooltips=TRUE,
                                    step=5, height="10px")),
                              prettySwitch(
                                'reverseX',
                                'Reverse X-Axis',
                                value=F, status = 'success',
                                fill=T
                              ),
                              #Choosing Y-Axis transformation
                              radioGroupButtons(
                                "logscale",
                                label = "Y-Axis Log Scale",
                                choices = c(
                                  "Default" = "none",
                                  "Log<sub>10</sub>" = "log10",
                                  "Log&#8322" = "log2"
                                ), size='sm'),
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
                                
                                numericInputIcon(
                                  'minY',
                                  "Min",
                                  value = NA, min = NULL, max = NULL),
                                numericInputIcon(
                                  'maxY',
                                  'Max',
                                  value = NA, min = NULL, max = NULL)),
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
                              div(id='sliderstyle',
                                  noUiSliderInput(
                                    'axisline',
                                    label = 'Axes Line Width',
                                    min = 0, max = 100,
                                    value = 50, tooltips=TRUE,
                                    step=1, height="10px")),
                              div(id='sliderstyle',
                                  noUiSliderInput(
                                    'ticklength',
                                    label = 'Axes Tick Length',
                                    min = 0, max = 100, value = 30,
                                    step = 1, tooltips = TRUE, height = "10px"
                                  )),
                              div(id='sliderstyle',
                                  noUiSliderInput(
                                    'tickwidth',
                                    label = 'Axes Tick Width',
                                    min = 0, max = 100, value = 50,
                                    step = 1, tooltips = TRUE, height = "10px"
                                  )),
                              prettySwitch(
                                'flipPlot',
                                'Flip Axes', value = F,
                                status = "success", fill = T
                              )
                            ),
                            accordion_panel(
                              title = 'Customize Font',
                              #Select type face
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
                              #Selecting font  size
                              div(h4('Plot Title'),
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'plotFont',
                                        label = 'Size: Plot Title',
                                        min = 5, max = 50,
                                        value = 18, tooltips=TRUE,
                                        step=1, height="10px"))
                              ),
                              div(h4('X-Axis'),
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'Xfontcol',
                                        label = 'Size: Axis Text',
                                        min = 5, max = 50,
                                        value = 18, tooltips=TRUE,
                                        step=1, height="10px")),
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'Xfontsz',
                                        label = 'Size: Axis Title',
                                        min = 5, max = 50,
                                        value = 18, tooltips=TRUE,
                                        step=1, height="10px")),
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'Xlinebreak',
                                        label = 'Title Linebreak',
                                        min = 10, max = 100,
                                        value = 75, tooltips=TRUE,
                                        step=1, height="10px"))),
                              div(h4('Y-Axis'),
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'Yfontcol',
                                        label = 'Size: Axis Text',
                                        min = 5, max = 50,
                                        value = 18, tooltips=TRUE,
                                        step=1, height="10px")),
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'Yfontsz',
                                        label = 'Size: Axis Title',
                                        min = 5, max = 50,
                                        value = 18, tooltips=TRUE,
                                        step=1, height="10px")),
                                  div(id='sliderstyle',
                                      noUiSliderInput(
                                        'Ylinebreak',
                                        label = 'Title Linebreak',
                                        min = 10, max = 100,
                                        value = 75, tooltips=TRUE,
                                        step=1, height="10px")))
                            ),
                            accordion_panel(
                              title = 'Customize Theme',
                              #Choosing theme generation
                              conditionalPanel(
                                condition = "input.dataGroup == false &&
                                input.askPlotTypeII != 'jitter'",
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
                                ),conditionalPanel(
                                  condition = "input.choosetheme == 'preset'",
                                  pickerInput(
                                    "boxtheme",
                                    label = "Preset Themes",
                                    choices = c(
                                      'Purrrrple' = 'purples',
                                      'Shred of Green' = 'greens',
                                      'Cherry Blossom' = 'pinks',
                                      'Center Of the Earth' = 'oranges',
                                      'Cold-feet' = 'blues',
                                      '22K Gold' = 'golds',
                                      'Desaturated Rainbow' = 'rainbows',
                                      'Seasonal Sky' = 'season',
                                      'Colourblind Friendly' = 'colorblind',
                                      'London Weather' = 'greys'
                                    )
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.choosetheme == 'gradient'",
                                  colorPickr(
                                    'grad1', "Colour 1", selected = '#EEE1EF',
                                    pickr_width = '20%'),
                                  colorPickr(
                                    'grad2', "Colour 2", selected = '#554994',
                                    pickr_width = '20%')
                                ),
                                conditionalPanel(
                                  condition = "input.choosetheme == 'palette'",
                                  uiOutput('coltabsOut')
                                )
                              ), conditionalPanel(
                                condition = "input.dataGroup == true&&
                                input.askPlotTypeIIG != 'jitter'",
                                pickerInput(
                                  "choosethemeII",
                                  label = "Select Theme Generator",
                                  choices = c(
                                    "Default" = "defaultG",
                                    "Select Individual" =
                                      'paletteG'
                                  )
                                )
                              ),
                              conditionalPanel(
                                condition = "input.choosethemeII == 'paletteG'",
                                uiOutput('coltabsOutG')
                              ),
                              
                              radioGroupButtons(
                                "grayscale",
                                label = "Display Contrast",
                                choices = c('Original'="No", 'Grayscale'="Yes"),
                                size = 'sm'),
                              radioGroupButtons(
                                "boxbordercol",
                                label = "Border Colour",
                                choices = c(
                                  "Darker" = 'dark',
                                  "Lighter" = 'light'
                                ),
                                selected = 'dark', size = 'sm'
                              ),
                              div(id='sliderstyle',
                                  noUiSliderInput(
                                    'shadevalue',
                                    label = 'Border Shade',
                                    min = 0, max = 100, value = 100,
                                    tooltips = TRUE, step = 1, height = '10px')),
                              div(id='sliderstyle',
                                  noUiSliderInput(
                                    'shapeAlpha',
                                    label = 'Shape Opacity',
                                    min = 0, max = 100, value = 100,
                                    tooltips = TRUE, step = 1, height = '10px')),
                              
                              #Choosing datapoint colors
                              conditionalPanel(
                                condition = "input.dataGroup == false",
                                conditionalPanel(
                                  condition = "input.askPlotTypeII == 'box' ||
                                input.askPlotTypeII == 'viopoint'",
                                  pickerInput(
                                    "dpcolor",
                                    label = "Datapoint Outline Colour",
                                    choices = c(
                                      "Default (Black)" = 'default',
                                      "Shape Colour" =
                                        'box',
                                      "Border Colour" =
                                        'border',
                                      "Make Gradient" =
                                        'dpgradient',
                                      "Select Individual" = 'dppalette'
                                    ),
                                    selected = "default"
                                  ),
                                  pickerInput(
                                    "dpfill",
                                    label = "Datapoint Fill Colour",
                                    choices = c(
                                      "Default (Black)" = 'default',
                                      "Shape Colour" =
                                        'box',
                                      "Border Colour" =
                                        'border',
                                      "Make Gradient" =
                                        'dpgradient',
                                      "Select Individual" = 'dppalette'
                                    ),
                                    selected = "default"
                                  )),
                                conditionalPanel(
                                  condition = "input.askPlotTypeII == 'jitter'",
                                  pickerInput(
                                    "dpcolor",
                                    label = "Datapoint Outline Colour",
                                    choices = c(
                                      "Default (Black)" = 'default',
                                      "Make Gradient" =
                                        'dpgradient',
                                      "Select Individual" = 'dppalette'
                                    ),
                                    selected = "default"
                                  ),
                                  pickerInput(
                                    "dpfill",
                                    label = "Datapoint Fill Colour",
                                    choices = c(
                                      "Default (Black)" = 'default',
                                      "Make Gradient" =
                                        'dpgradient',
                                      "Select Individual" = 'dppalette'
                                    ),
                                    selected = "default"
                                  )),
                                conditionalPanel(
                                  condition = "input.dpcolor == 'dpgradient'",
                                  conditionalPanel(
                                    condition = "input.askPlotTypeII == 'jitter' || 
                                    input.askPlotTypeII == 'viopoint' || input.askPlotTypeII == 'box'",
                                    p("Select Gradient for Datapoint Outline", 
                                      style="font-weight:500;"),
                                    div(style="display:inline-flex; flex-direction:row; gap:5px;",
                                        colorPickr(
                                          'dpgrad1', "Colour 1", selected = '#EEE1EF',
                                          pickr_width = '20%'),
                                        colorPickr(
                                          'dpgrad2', "Colour 2", selected = '#554994',
                                          pickr_width = '20%')
                                    )
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.dpfill == 'dpgradient'",
                                  conditionalPanel(
                                    condition = "input.askPlotTypeII == 'jitter' || 
                                    input.askPlotTypeII == 'viopoint' || input.askPlotTypeII == 'box'",
                                    p("Select Gradient for Datapoints", 
                                      style="font-weight:500;"),
                                    div(style="display:inline-flex; flex-direction: row;
                                    gap: 5px;",
                                        colorPickr(
                                          'dpgradF1', "Colour 1", selected = '#EEE1EF',
                                          pickr_width = '20%'),
                                        colorPickr(
                                          'dpgradF2', "Colour 2", selected = '#554994',
                                          pickr_width = '20%')
                                    )
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.dpcolor == 'dppalette'",
                                  conditionalPanel(
                                    condition ="input.askPlotTypeII == 'jitter' || 
                                    input.askPlotTypeII == 'viopoint' || input.askPlotTypeII == 'box'",
                                    p("Select Colors for Datapoint Outline", 
                                      style="font-weight:500;"),
                                    uiOutput('dpcoltabsOut')
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.dpfill == 'dppalette'",
                                  conditionalPanel(
                                    condition ="input.askPlotTypeII == 'jitter' || 
                                    input.askPlotTypeII == 'viopoint' || input.askPlotTypeII == 'box'",
                                    p("Select Colors for Datapoints", 
                                      style="font-weight:500;"),
                                    uiOutput('dpfilltabsOut')
                                  )
                                )
                              ),
                              conditionalPanel(
                                condition = "input.askPlotTypeIIG == 'box' &&
                                input.dataGroup == true",
                                pickerInput(
                                  "dpcolorG",
                                  label = "Datapoint Outline Colour",
                                  choices = c(
                                    "Default (Black)" = 'defaultG',
                                    "Border Colour" =
                                      'borderG'
                                  ),
                                  selected = "default"
                                ),
                                pickerInput(
                                  "dpfillG",
                                  label = "Datapoint Fill Colour",
                                  choices = c(
                                    "Default (Black)" = 'defaultG',
                                    "Border Colour" =
                                      'borderG'
                                  ),
                                  selected = "default"
                                )
                              )
                            )),
                  tableOutput('test')
                ),
                div(conditionalPanel(
                  condition = "input.dataGroup == true",
                  pickerInput('askPlotTypeIIG',
                              label = 'Plot Type',
                              choices = c("Violin Plot" = 'violin',
                                          "Box-Jitter Plot" = 'box' ),
                              selected = 'violin', width = '150px'),
                  prettySwitch('grpSwitch', 'Switch Groups', 
                               status = 'success', value = F, fill = T,
                               width = "50%")
                ), conditionalPanel(
                  condition = "input.dataGroup == false",
                  pickerInput('askPlotTypeII',
                              label = 'Plot Type',
                              choices = c("Violin Plot" = 'violin',
                                          "Raincloud Plot" = 'viopoint',
                                          "Box-Jitter Plot" = 'box',
                                          "Jitter Plot"='jitter',
                                          "Bar Plot" = 'bar' ),
                              selected = 'violin', width = '150px')
                ),style="position:absolute !important;
                right:18px !important; z-index:10;background:#fff;
                padding:8px; border-radius:10px;"),
                uiOutput('graph_main_content')
                
              ))),
  nav_panel(title = "Statistics",
            card(
              card_header("Stat Analysis View"),
              layout_sidebar(
                sidebar = sidebar(
                  width=350,
                  h4("Experimental Settings"),
                  uiOutput('testInput'),
                  uiOutput('submitAnalysis'),
                  uiOutput('statDnld')
                ), 
                uiOutput('stat_main_content')
                
              )
            )
  ),
  nav_panel(title = "About", p("To Be Updated Soon."))
)

###########################
##Starting of Server Code##
###########################

server <- shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize = 250 * 1024^2) # Max File size is 250 MB
  
  current_colnames <- reactiveVal(NULL)
  obsBtn <- reactiveValues(submitVal=NULL, pasteVal=NULL)
  
  file_Path <- reactive({
    # if (isTruthy(input$exampleFile)){
    #   list(datapath = 'Example_data.xlsx')
    # } else {
    input$file
    # }
  })
  
  #Disable Paste Data Button if XLSX file is uploaded
  observe({
    req(file_Path())
    updateActionButton(session = session, 'pasteBtn',
                       'Upload Pasted Data', disabled = T)
  })
  observe({
    req(input$exampleFile)
    updateActionButton(session = session, 'pasteBtn',
                       'Upload Pasted Data', disabled = T)
  })
  observe({
    req(input$pasteBtn)
    updateActionButton(session = session, 'exampleFile', 'Upload Example File',
                       disabled = T)
  })
  observe({
    req(input$file)
    updateActionButton(session = session, 'exampleFile', 'Upload Example File',
                       disabled = T)
  })
  
  
  # Sheet names
  sheetName <- reactive({
    req(file_Path())
    openxlsx::getSheetNames(file_Path()$datapath)
  })
  
  output$fileupload <- renderUI({
    req(data())
    div(style="display:inline-flex;width:100%; flex-direction:column;",
        tagList(
          div(prettySwitch('dataGroup', 'Grouped Data',
                           status = 'success', inline = T, value = F, fill = T),
              style="width:cover; text-align:center; display:inline-flex;
              "),
          conditionalPanel(
            condition = "input.dataGroup == true",
            pickerInput('askPlotTypeG',
                        label = 'Plot Type',
                        choices = c("Violin Plot" = 'violin',
                                    "Box-Jitter Plot" = 'box'),
                        selected = 'violin')
          ), conditionalPanel(
            condition = "input.dataGroup == false",
            pickerInput('askPlotType',
                        label = 'Plot Type',
                        choices = c("Violin Plot" = 'violin',
                                    "Raincloud Plot" = 'viopoint',
                                    "Box-Jitter Plot" = 'box',
                                    "Jitter Plot"='jitter',
                                    "Bar Plot" = 'bar' ),
                        selected = 'violin')
          )))
  })
  
  output$uploadBtnShow <- renderUI({
    req(file_Path())
    actionButton('submitFile', 'Upload Datasheet', icon = icon('file-arrow-up'))
  })
  
  observe({
    req(sheetName())
    updateSelectInput(session, "sheetlist", "Select Datasheet", 
                      choices = sheetName(), selected = sheetName()[1])
  })
  
  
  output$sheetnames <- renderUI({
    req(file_Path())
    tagList(
      selectInput("sheetlist", "Select Datasheet",
                  choices = sheetName(), selected = sheetName()[1])
    )
  })
  
  pasteDf <- reactiveValues(df = NULL)
  
  observeEvent(input$pasteBtn,{ 
    req(input$pasted_Data)
    pastedData <- read.delim(text = input$pasted_Data, 
                             header = TRUE,
                             sep = "\t",
                             check.names = FALSE,
                             stringsAsFactors = FALSE)
    pastedData <- as.data.frame(pastedData)
    
    if (!is.null(pastedData) && nrow(pastedData)>0 && isTRUE(is.data.frame(pastedData))){
      showNotification('Data Pasted Successfully', type = 'message')
      pasteDf$df <- pastedData
    } else {
      showNotification('Invalid Data. Paste Only Datatable', type = 'error')
    }
  })
  
  colNames <- reactive({
    df <- NULL
    if (!is.null(pasteDf$df)){
      df <- pasteDf$df
    } 
    if (!is.null(file_Path())){
      req(file_Path(), input$sheetlist)
      df <- NULL
      df <- openxlsx::read.xlsx(file_Path()$datapath, sheet = input$sheetlist, colNames = TRUE)
      
    }
    colnames(df)
  })
  
  #Processing Two Upload Buttons
  observeEvent(input$submitFile,{
    obsBtn$submitVal <- input$submitFile
  })
  observeEvent(input$pasteBtn,{
    obsBtn$pasteVal <- input$pasteBtn
  })
  
  observeEvent(c(obsBtn$submitVal, obsBtn$pasteVal), {
    current_colnames(colNames())
    updatePickerInput(session, "selectedCols", "Select Columns",
                      choices = current_colnames(), selected = current_colnames())
  }, ignoreNULL = TRUE)
  observeEvent(c(obsBtn$submitVal, obsBtn$pasteVal),{
    validate(
      need(isTRUE(is.numeric(data()[,1])), "Please paste valid data.")
    )
    output$colupdateBttn <- renderUI({
      tagList(
        #Selecting columns to plot
        uiOutput("colnames"),
        actionButton('chngColBtn', 'Update Column Header', icon = icon('heading'), 
                     class = "btn-primary")
      )
    })
  })
  
  # If user changes sheet after submit: reset names and selection
  observeEvent(input$sheetlist, {
    current_colnames(colNames())
    updatePickerInput(session, "selectedCols", "Select Columns",
                      choices = current_colnames(), selected = current_colnames())
  })
  
  # Render column picker UI (after submit or sheet change)
  output$colnames <- renderUI({
    req(current_colnames())
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
  
  # Main data reactive: read full sheet, apply current names, then subset
  
  data <- reactive({
    #Normal file upload path
    df_full <- NULL
    #Read from uploaded XLSX file
    if (!is.null(file_Path()) && !is.null(input$sheetlist) && isTruthy(input$submitFile)) {
      
      df_full <- tryCatch(
        openxlsx::read.xlsx(
          file_Path()$datapath,
          sheet = input$sheetlist,
          colNames = TRUE,
          skipEmptyRows = TRUE
        ),
        error = function(e) {
          showNotification("Failed to read uploaded file.", type = "error")
          NULL
        }
      )
    }
    
    #If pasteDf$df or pasted data exists
    if (is.null(df_full) && !is.null(pasteDf$df) && is.data.frame(pasteDf$df)) {
      df_full <- pasteDf$df
    }
    #Check whether correct datatable loaded
    if (is.null(df_full) || !is.data.frame(df_full) || nrow(df_full) == 0 ||
        isTRUE(has_element(sapply(df_full,is.numeric),FALSE))) {
      req(input$submitFile>0)
      showNotification("No valid data loaded.", type = "warning")
      return(NULL)
    }
    
    # Use current_colnames() to rename
    if (ncol(df_full) == length(current_colnames())) {
      colnames(df_full) <- current_colnames()
    } else {
      req(input$submitFile>0)
      showNotification("Column count mismatch after loading — using original names.", type = "warning")
    }
    
    # Subset only if selectedCols exist and are valid
    selected <- input$selectedCols %||% current_colnames()
    selected <- intersect(selected, colnames(df_full))
    
    if (length(selected) == 0) {
      showNotification("No valid columns selected.", type = "warning")
      return(NULL)
    }
    
    df_full[, selected, drop = FALSE]
  })
  
  # Data table
  output$contents <- renderDT({
    req(data())
    
    DT::datatable(as.data.frame(data()), 
                  editable = TRUE,
                  rownames = F,
                  options = list(
                    columnDefs = list(list(className = 'dt-left', targets = '_all'))
                  ))
  })
  
  #Column Rename Modal
  observeEvent(input$chngColBtn, {
    showModal(modalDialog(
      title = "Update Column Headers",
      uiOutput('newColOut'),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("saveColNames", "Save Changes", class = "btn-primary")
      )
    ))
  })
  
  # Modal inputs – default to CURRENT names
  output$newColOut <- renderUI({
    req(data())
    lapply(seq_along(data()), function(i) {
      textInput(
        paste0("newcolname_", i),
        label = paste("Column", i, "- current:", colnames(data())[i]),
        value = current_colnames()[i]
      )
    })
  })
  
  # Save renamed columns
  observeEvent(input$saveColNames, {
    req(current_colnames())
    old_names <- current_colnames()
    new_names <- sapply(seq_along(old_names), function(i) {
      val <- input[[paste0("newcolname_", i)]]
      if (is.null(val) || trimws(val) == "") old_names[i] else trimws(val)
    })
    
    current_colnames(new_names)
    
    # Preserve selection by matching old names
    selected_new <- new_names[old_names %in% (input$selectedCols %||% old_names)]
    updatePickerInput(session, "selectedCols", choices = new_names, selected = selected_new)
    
    removeModal()
  })
  
  
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
  
  
  # Arranging Input Raw Data for plotting
  orderdata <- reactive({
    if (input$dataGroup == T){
      if(isTRUE(input$grpSwitch)){
        colsOrder <- c("variable", "groups")
      } else { 
        colsOrder <- c("groups","variable")
      }
      order_data <-
        data() |> pivot_longer(cols = everything(),
                               names_to = colsOrder,
                               names_sep = ':',
                               values_to = "value") |>
        arrange(variable)
    } else {
      order_data <-
        data() |> pivot_longer(cols = everything(),
                               names_to = "variable",
                               values_to = "value") |>
        arrange(variable)}
  })
  
  ### Graph Axis Processing ###
  axistitleY <- reactive({
    aytitle <- as.character(req(input$aytitle))
  })
  axistitleX <- reactive({
    axtitle <- (input$axtitle)
  })
  #Processing special characters
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
  observeEvent(input$addsymbolTit, {
    updateTextInput(session,
                    "plotTitle",
                    value = paste(input$plotTitle, input$symbolsTit, sep = ''))
  })
  observeEvent(input$addsymbolY, {
    updateTextInput(session,
                    "aytitle",
                    value = paste(input$aytitle, input$symbolsY, sep = ''))
  })
  observeEvent(input$addsymbolX, {
    updateTextInput(session, "axtitle", value = paste(input$axtitle, input$symbolsX))
  })
  
  #Processing markdown
  output$showMarkdownTit <- renderUI({
    if (isTRUE(input$markdownTit)) {
      choice_labels <- c(
        "<b>B</b>", 
        "<i>i</i>", 
        "x&#178", 
        "x&#8322"
      )
      choice_values <- c(
        "**replace this**", 
        "*replace this*", 
        "<sup>replace this</sup>", 
        "<sub>replace this</sub>"
      )
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
    updateTextInput(session,
                    "plotTitle",
                    value = paste(input$plotTitle, input$mdTit, sep = ''))
  })
  output$showMarkdownY <- renderUI({
    if (isTRUE(input$markdownY)) {
      choice_labels <- c(
        "<b>B</b>", 
        "<i>i</i>", 
        "x&#178", 
        "x&#8322"
      )
      choice_values <- c(
        "**replace this**", 
        "*replace this*", 
        "<sup>replace this</sup>", 
        "<sub>replace this</sub>"
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
    updateTextInput(session,
                    "aytitle",
                    value = paste(input$aytitle, input$mdy, sep = ''))
  })
  output$showMarkdownX <- renderUI({
    if (isTRUE(input$markdownX)) {
      choice_labels <- c(
        "<b>B</b>", 
        "<i>i</i>", 
        "x&#178", 
        "x&#8322"
      )
      choice_values <- c(
        "**replace this**", 
        "*replace this*", 
        "<sup>replace this</sup>", 
        "<sub>replace this</sub>"
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
    updateTextInput(session,
                    "axtitle",
                    value = paste(input$axtitle, input$mdx, sep = ''))
  })
  #X-Axis rotation processing
  xcolPos <- reactive({
    if(input$Xrotate == 0){
      return(0.5)
    }else {
      return(1)
    }
  })
  #Y axis log transformation
  logaxis <- reactive({
    logscale <- switch(input$logscale,
                       none = 'identity', log10 = 'log10', log2 = 'log2')
  })
  #Plot Title Text alignment 
  titlePlot <- reactive({
    plotT <- switch(input$titlePos, left = 0, center = 0.5, right = 1)
  })
  #Y axis scientific transformation
  Ycontax <- reactive({
    if (input$labelY=='Scientific'){
      scale_y_continuous (labels=scales::scientific, trans = logaxis(),
                          expand = c(0,0.1))
    }else{
      scale_y_continuous (trans = logaxis(), expand = c(0,0.1))
    }
  })
  #Y axis maximum and minimum range
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
  
  ## Plot Customization ##
  shapeBox <- reactiveValues(shapes = list())
  shapeRain <- reactiveValues(shapes = list())
  shapeJitter <- reactiveValues(shapes = list())
  
  # Datapoint shapes For Box-Jitter Plot
  dpshapeBox <- reactive({
    if (isFALSE(input$pointDistBox)) {
      dpshapes <- rep(c(input$pointshapeBox), length(colorCount()))
    } else if (isTRUE(input$pointDistBox)) {
      dpshapes <- lapply(seq_along(colorCount()), function(i) {
        input[[paste("pointshapeBox", i, sep = '_')]]
      })
    }
    return(c(dpshapes))
  })
  pointShapeBoxOut <- reactive({
    req(data(), current_colnames())
    if (input$dataGroup == T){
      cols <- length(colorCount())
      coln <- colorCount()
    } else {
      cols <- ncol(data())
      coln <- colnames(data())
    }
    # Generate the list of buttons
    
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
    })
  })
  output$pointInpUIBox <- renderUI({
    pointShapeBoxOut()
  })
  output$pointInpUIBoxG <- renderUI({
    pointShapeBoxOut()
  })
  observe({
    ## To observe any changes in new point shape box inputs and update accordingly
    req(isTRUE(input$pointDistBox))
    ids <- paste("pointshapeBox", seq_along(colorCount()), sep = '_')
    
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
      dpshapes <- rep(c(input$pointshape), ncol(data()))
    } else if (isTRUE(input$pointDist)) {
      dpshapes <- lapply(seq_along(colorCount()), function(i) {
        input[[paste("pointshape", i, sep = '_')]]
      })
    }
    return(c(dpshapes))
  })
  output$pointInpUI <- renderUI({
    req(data()) 
    cols <- ncol(data())
    coln <- colnames(data())
    # Generate the list of buttons
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
    })
  })
  observe({
    ## To observe any changes in new point shape jitter inputs and update accordingly
    req(isTRUE(input$pointDist))
    ids <- paste("pointshape", seq_along(colorCount()), sep = '_')
    
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
      dpshapes <- rep(c(input$pointshapeRain), ncol(data()))
    } else if (isTRUE(input$pointDistRain)) {
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
    # Generate the list of buttons
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
    })
  })
  observe({
    ## To observe any changes in new point shape raincloud inputs and update accordingly
    req(isTRUE(input$pointDistRain))
    ids <- paste("pointshapeRain", seq_along(colorCount()), sep = '_')
    
    for (id in ids) {
      curr_input <- input[[id]]
      if (isTruthy(input$reuseset)){
        shapeRain$shapes[[id]] <- shapeRain$shapes[[id]]
      } else {
        shapeRain$shapes[[id]] <- curr_input
      }
    }
  })
  
  addScatterBox <- reactive({
    if (input$boxtype == "boxpoint") {
      scatter <- (input$scatterBox)/25
    }
  })
  
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
  
  #Datapoint count processing
  addDataPointLabel <- reactive({
    tempdata <- data()
    newdf <- data.frame()
    for (i in 1:ncol(tempdata)) {
      df <- data.frame(length(na.omit(tempdata[, i])))
      newdf <- rbind(newdf, df)
    }
    text <- newdf
    minval <- min(na.omit(tempdata))
    if (is.na(input$minY)) {
      Ycord <- as.data.frame(rep(minval - (minval / 1.5), ncol(tempdata)))
    } else {
      Ycord <- input$minY
    }
    
    Xcord <- data.frame(1:ncol(tempdata))
    finaldf <- cbind(text, Xcord, Ycord)
    colnames(finaldf) <- c('text', 'x', 'y')
    as.data.frame(finaldf)
  })
  
  ### Plot Theme processing ###
  
  addTheme <- reactive({
    if (input$dataGroup == T && input$choosethemeII == 'paletteG'){
      usertheme <- lapply(seq_along(colorCount()), function(i) {
        input[[paste("colors", i, sep = '_')]]
      })
      usertheme <- unlist(usertheme)
      palThemeG(usertheme)
    } else if (input$dataGroup == T && input$choosethemeII == 'defaultG') {
      usertheme <- colorRampPalette(c("#EEE1EF", "#554994"))(length(colorCount()))
    } else {
      if (input$choosetheme == 'default') {
        usertheme <- colorRampPalette(c("#EEE1EF", "#554994"))(ncol(data()))
      } else if (input$choosetheme == 'preset') {
        # Making Gradients to generate Preselected Color Shades
        usertheme <- switch(
          input$boxtheme,
          purples = colorRampPalette(c("#EEE1EF", "#554994"))(ncol(data())),
          greens = colorRampPalette(c("#97C4B8", "#064420"))(ncol(data())),
          pinks = colorRampPalette(c('#FDE4DE', '#F56093'))(ncol(data())),
          oranges = colorRampPalette(c('#FFAE01', '#C70E00'))(ncol(data())),
          blues = colorRampPalette(c('#1BFFFF', '#2E3192'))(ncol(data())),
          golds = colorRampPalette(c('#FDD700', '#8C3617'))(ncol(data())),
          rainbows = colorRampPalette(c('#bae1ff', '#ffb3ba'))(ncol(data())),
          season = colorRampPalette(c('#F4E867', '#DA4B82', '#387494'))(ncol(data())),
          colorblind = colorRampPalette(c('#F5EADA','#768267', '#304659'))(ncol(data())),
          greys = colorRampPalette(c('#C0C0C2', '#373737'))(ncol(data()))
        )
      }
      else if (input$choosetheme == 'gradient') {
        usertheme <- colorRampPalette(c(input$grad1, input$grad2))(ncol(data()))
      }
      else if (input$choosetheme == 'palette') {
        usertheme <- lapply(seq_along(colorCount()), function(i) {
          input[[paste("colors", i, sep = '_')]]
        })
      }
    }
    if (input$grayscale == "Yes") {
      theme <- ColToGray(usertheme)
    } else{
      theme <- usertheme
    }
    return(theme)
  })
  
  #Theme generator processing
  colorCount <- reactive({
    if (input$dataGroup == T) {
      unique(orderdata()$groups)
    } else {
      data()
    }
  })
  palTheme <- reactiveValues(palette = list())
  dpPalTheme <- reactiveValues(palette = list())
  dpPalFillTheme <- reactiveValues(palette = list())
  
  colLevelG <- reactive({
    temp <- str_split_fixed(colnames(data()), ':', 2)
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
  
  coltabs <- reactive({
    req(colorCount())
    
    if(isTRUE(input$dataGroup)){
      coltabname <- unique(factor(orderdata()$groups, levels = colLevelG()))
    } else {
      coltabname <- colnames(data())
    }
    
    div(lapply(seq_along(colorCount()), function(i) {
      id <- paste("colors", i, sep = '_')
      initial_val <- isolate(palTheme$palette[[id]])
      if (is.null(initial_val)) initial_val <- "#CCCCCC"
      
      div(colorPickr(
        id,
        label = as.character(coltabname[i]),
        selected = initial_val,
        pickr_width = '20%'
      ), style = "width:100px")
    }), style = "display:inline-flex; flex-wrap:wrap !important; gap:8px;")
  })
  
  output$coltabsOut <- renderUI({
    coltabs()
  })
  output$coltabsOutG <- renderUI({
    coltabs()
  })
  paletteTheme <- reactive({
    req(colorCount())
    
    ids <- paste("colors", seq_along(colorCount()), sep = '_')
    
    if (input$choosetheme == 'palette') {
      cols <- lapply(ids, function(x) {
        if (!is.null(palTheme$palette[[x]])) {
          return(palTheme$palette[[x]])
        } 
        else if (!is.null(input[[x]])) {
          return(input[[x]])
        } 
        else {
          return('#CCCCCC')
        }
      })
      return(as.character(unlist(cols)))
    } else {
      return(as.character(addTheme()))
    }
  })
  observe({
    ## To observe any changes in new palette color inputs and update accordingly
    req(input$choosetheme == 'palette')
    ids <- paste("colors", seq_along(colorCount()), sep = '_')
    
    for (id in ids) {
      curr_input <- input[[id]]
      if (isTruthy(input$reuseset)){
        palTheme$palette[[id]] <- palTheme$palette[[id]]
      } else {
        palTheme$palette[[id]] <- curr_input
      }
    }
  })
  ## For jitter point theme
  dpcoltabs <- reactive({
    req(colorCount())
    if(isTRUE(input$dataGroup)){
      coltabname <- unique(factor(orderdata()$groups,levels = colLevelG()))
    } else{
      coltabname <- colnames(data())
    }
    
    div(lapply(seq_along(colorCount()), function(i) {
      id <- paste("colorsdp",i, sep = '_')
      initial_val <- isolate(dpPalTheme$palette[[id]])
      if (is.null(initial_val)) initial_val <- '#CCCCCC'
      
      div(colorPickr(
        inputId = id,
        label = as.character(coltabname[i]),
        selected = initial_val,
        pickr_width = '20%'
      ), style = "width:100px;")
    }),style="display:inline-flex; flex-wrap:wrap !important; gap:10px;")
  })
  dpPaletteTheme <- reactive({
    req(colorCount())
    
    ids <- paste("colorsdp", seq_along(colorCount()), sep = '_')
    
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
      return(as.character(unlist(cols)))
    } else {
      return(as.character(dpcolors()))
    }
  })
  observe({
    ## To observe any changes in new palette color inputs and update accordingly
    req(input$dpcolor == 'dppalette')
    ids <- paste("colorsdp", seq_along(colorCount()), sep = '_')
    
    for (id in ids) {
      curr_input <- input[[id]]
      if (isTruthy(input$reuseset)){
        dpPalTheme$palette[[id]] <- dpPalTheme$palette[[id]]
      } else {
        dpPalTheme$palette[[id]] <- curr_input
      }
    }
  })
  output$dpcoltabsOut <- renderUI({
    dpcoltabs()
  })
  ## For jitter point fill themes
  dpfilltabs <- reactive({
    req(colorCount())
    if(isTRUE(input$dataGroup)){
      coltabname <- unique(factor(orderdata()$groups,levels = colLevelG()))
    } else{
      coltabname <- colnames(data())
    }
    div(lapply(seq_along(colorCount()), function(i) {
      id <- paste("fillsdp",i, sep = '_')
      initial_val <- isolate(dpPalFillTheme$palette[[id]])
      if (is.null(initial_val)) initial_val <- '#CCCCCC'
      
      div(colorPickr(
        inputId = id,
        label = as.character(coltabname[i]),
        selected = initial_val,
        pickr_width = '20%'
      ), style = "width:100px;")
    }),style="display:inline-flex; flex-wrap:wrap !important; gap:10px;")
  })
  
  dpPaletteFillTheme <- reactive({
    req(colorCount())
    
    ids <- paste("fillsdp", seq_along(colorCount()), sep = '_')
    
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
      return(as.character(unlist(cols)))
    } else {
      return(as.character(dpfillVal()))
    }
  })
  observe({
    ## To observe any changes in new palette color inputs and update accordingly
    req(input$dpfill == 'dppalette')
    ids <- paste("fillsdp", seq_along(colorCount()), sep = '_')
    
    for (id in ids) {
      curr_input <- input[[id]]
      if (isTruthy(input$reuseset)){
        dpPalFillTheme$palette[[id]] <- dpPalFillTheme$palette[[id]]
      } else {
        dpPalFillTheme$palette[[id]] <- curr_input
      }
    }
  })
  output$dpfilltabsOut <- renderUI({
    dpfilltabs()
  })
  
  
  
  
  boxVioTheme <-reactive({
    if (input$boxColVio == 'Shape'){
      paletteTheme()
    }else{
      if (input$dataGroup == T){
        rep(input$boxColCust,length(colorCount()))
      }else {
        rep(input$boxColCust,ncol(data()))
      }
    }
  })
  
  bordercolor <- reactive({
    if (input$boxbordercol == 'light') {
      boxborder <- lighten(paletteTheme(), (input$shadevalue/100))
    } else if (input$boxbordercol == 'dark') {
      boxborder <- darken(paletteTheme(), (input$shadevalue/100))
    }
    return(boxborder)
  })
  
  
  ##Data point color process
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
        dpcolors <- rep(c('#000000'), length(colorCount()))
      } else if (input$dpcolor == 'box') {
        dpcolors <- paletteTheme()
      } else if (input$dpcolor == 'border') {
        dpcolors <-  bordercolor()
      } else if (input$dpcolor == 'dpgradient') {
        dpcolors <- colorRampPalette(c(input$dpgrad1, input$dpgrad2))(length(colorCount()))
      } else if (input$dpcolor == 'dppalette') {
        dpcolors <- lapply(seq_along(colorCount()), function(i) {
          input[[paste("colorsdp", i, sep = '_')]]}
        )
      }
    }
  })
  
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
        dpcolors <- rep(c('#000000'), length(colorCount()))
      } else if (input$dpfill == 'box') {
        dpcolors <- paletteTheme()
      } else if (input$dpfill == 'border') {
        dpcolors <-  bordercolor()
      } else if (input$dpfill == 'dpgradient') {
        dpcolors <- colorRampPalette(c(input$dpgradF1, input$dpgradF2))(length(colorCount()))
      } else if (input$dpfill == 'dppalette') {
        dpcolors <- lapply(seq_along(colorCount()), function(i) {
          input[[paste("fillsdp", i, sep = '_')]]}
        )
      }
    }
  })
  
  #Font-family
  fontfamily <- reactive({
    # Load the fonts
    # font_import()
    loadfonts(device = 'win')
    input$font
  })
  
  #plot title position
  PlotTitPos <- reactive({
    if (!isTruthy(input$addBrackets)) {
      y <- yaxisMax()*(input$verAlign/500+1)
    } else{
      y <- max(statBrackets()$y)*(input$verAlign/500+1)
    }
    xPos <- length(unique(orderdata()$variable))
    return(data.frame(x=xPos/2+0.5,y=y,text=input$plotTitle))
  })
  
  # Processing the gap for raincloud plot
  
  gap <- reactive({
    if (input$slabSide == 'right'){
      return(0-(input$slabDistance/300))
    }else{
      return(1+(input$slabDistance/300))
    }
  })
  #Summary Layer
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
    
    summary_layers <- switch(
      current_sum_val,
      "mean_only" = stat_summary(
        fun = mean, fun.min = mean, fun.max = mean,
        geom = "crossbar", width = statWidth/100, color = "black", linewidth = (statLine/80)),
      "mean_sd" = list(
        stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
                     geom = "errorbar", color = "black", linewidth = (statLine/50), width = statWidth/150),
        stat_summary(fun = mean, fun.min = mean, fun.max = mean,
                     geom = "crossbar", width = statWidth/100, color = "black", linewidth = (statLine/80))),
      
      "mean_sem" = list(
        stat_summary(fun.data = mean_se, geom = "errorbar",
                     color = "black", linewidth = (statLine/50), width = statWidth/150),
        stat_summary(fun = mean, fun.min = mean, fun.max = mean,
                     geom = "crossbar", width = statWidth/100, color = "black", linewidth = (statLine/80))),
      "median_only" = stat_summary(
        fun = median, fun.min = median, fun.max = median,
        geom = "crossbar", width = statWidth/100, color = "black", linewidth = (statLine/80)),
      "median_ci" = list(
        stat_summary(fun.data = median_hilow, fun.args = list(conf.int = 0.95),
                     geom = "errorbar", color = "black", linewidth = (statLine/50), width = statWidth/150),
        stat_summary(fun = median, fun.min = median, fun.max = median,
                     geom = "crossbar", width = statWidth/100, color = "black", linewidth = (statLine/80)))                                       
    )})
  #Summary Layer for Bar plots
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
    
    summary_layers <- switch(
      current_sum_val,
      "mean_only" = stat_summary(
        fun = mean, fun.min = mean, fun.max = mean,
        geom = "crossbar", width = 0, color = "black", linewidth = (statLine/80)),
      "mean_sd" = list(
        stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
                     geom = "errorbar", color = "black", linewidth = (statLine/50), width = statWidth/150)),
      
      "mean_sem" = list(
        stat_summary(fun.data = mean_se, geom = "errorbar",
                     color = "black", linewidth = (statLine/50), width = statWidth/150)),
      "median_only" = stat_summary(
        fun = median, fun.min = median, fun.max = median,
        geom = "crossbar", width = 0, color = "black", linewidth = (statLine/80)),
      "median_ci" = list(
        stat_summary(fun.data = median_hilow, fun.args = list(conf.int = 0.95),
                     geom = "errorbar", color = "black", linewidth = (statLine/50), width = statWidth/150))                                       
    )})
  
  
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
      fill = guide_legend(override.aes = list(alpha = 1, color=NA), title = input$legTitle),
      shape = guide_legend(override.aes = list(alpha = 1),title = input$legTitle),
      color = guide_legend(override.aes = list(alpha = 1, color =  NA),title = input$legTitle)
    )
    return(leg)
  })
  ## Processing for inside legend position selector
  leg_pos <- reactiveVal(c(0.87,0.85))
  observeEvent(input$plot_click, {
    leg_pos(c(input$plot_click$x, input$plot_click$y))
  })
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
    if (isTRUE(input$dataGroup)){
      temp <- str_split_fixed(colnames(data()), ':', 2)
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
      df$groups <- factor(df$groups, level = lev)
      
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
  
  plotinput <- reactive({
    x <- orderdata()
    
    if (input$dataGroup == T) {
      temp <- str_split_fixed(colnames(data()), ':', 2)
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
            trim = input$endTrim, alpha=(input$shapeAlpha/100),
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
              trim = input$endTrim, alpha=(input$shapeAlpha/100),
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
              trim = input$endTrim, alpha=(input$shapeAlpha/100),
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
      ### For Box-Jitter Plot ###
      
      if (input$boxtype == "boxpoint"){
        p <-  g+
          geom_boxplot(
            mapping = aes(fill = fillPara, color= fillPara),
            position = position_dodge(width = input$innerDistBox/100),
            width = input$boxwidth/100, alpha=(input$shapeAlpha/100),
            lwd = input$linewidthBox/40, fatten = 2,
            show.legend = F,
            notch = input$notch,
            outliers = input$outlier,
            outlier.shape = as.numeric(8),
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
            method = input$pointMethodBox,
            corral = 'wrap'
            # shape = as.numeric(input$pointshape)
          )+
          scale_color_manual(values = unlist(dpPaletteTheme()))+
          scale_fill_manual(values = unlist(dpPaletteFillTheme()))+
          scale_shape_manual(values=as.numeric(dpshapeBox()))+
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
            width = input$boxwidth/100, alpha=(input$shapeAlpha/100),
            lwd = input$linewidthBox/40, fatten = 2,
            show.legend = NA,
            notch = input$notch,
            outliers = input$outlier,
            outlier.shape = as.numeric(8),
            outlier.colour = 'red',
            outlier.size=3,
            staplewidth = 0.3)+
          scale_color_manual(values = bordercolor())+
          scale_fill_manual(values = paletteTheme())+
          grpLegend()
      }
    } else if (plotType() =='jitter'){
      ### For Jitter Plot ###
      
      p <-  g+
        summary_layers()+
        geom_beeswarm(
          mapping = aes(shape = fillPara,
                        fill= fillPara,
                        color = fillPara
          ),
          # dodge.width = input$innerDistBox/100,
          size = input$pointsize/30,
          cex = input$scatter/25,
          alpha = input$shapeAlpha/100,
          method = input$pointMethod,
          corral = 'wrap'
        )+
        scale_color_manual(values = unlist(dpPaletteTheme()))+
        scale_fill_manual(values = unlist(dpPaletteFillTheme()))+
        scale_shape_manual(values=as.numeric(dpshape()))
    } else if (plotType() == 'viopoint'){
      ### For Raincloud Plot ###
      
      p <-  g+
        stat_halfeye(
          mapping = aes(fill = fillPara, slab_color = fillPara),
          side=input$slabSide,
          point_colour = NA,
          alpha = (input$shapeAlpha/100),
          justification = gap(),
          .width = 0,
          width = 0.6,
          trim = input$endTrimRain,
          slab_linewidth = input$linewidthRain/40,
          linetype= 'solid'
        )+
        scale_fill_manual(values = paletteTheme())+
        scale_color_manual(values = bordercolor(),
                           aesthetics = 'slab_color')+
        summary_layers()+
        new_scale_fill()+
        new_scale_color()+
        geom_beeswarm(
          aes(shape=fillPara, fill = fillPara, color=fillPara),
          size = (input$pointsizeRain/30),
          cex = (input$scatterRain/50),
          alpha = (input$shapeAlpha/100),
          method = input$pointMethod,
          corral = 'wrap')+
        scale_shape_manual(values=as.numeric(dpshapeRain()))+
        scale_fill_manual(values = unlist(dpPaletteFillTheme()))+
        scale_color_manual(values=unlist(dpPaletteTheme()))
    } else if (plotType() == 'bar'){
      ### For Bar Plot ###
      
      if (input$askSide == 'Bothside'){
        q <-  g+
          stat_summary(
            fun = input$barFunc,
            geom = "col",
            mapping = aes(fill = fillPara),
            color = bordercolor(),
            width = input$barwidth / 100, # Reusing your box width slider
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
            fill = 'white',
            color = bordercolor(),
            width = input$barwidth / 100, # Reusing your box width slider
            # alpha = (input$shapeAlpha / 100),
            linewidth = input$linewidthBar / 40
          )+
          stat_summary(
            fun = input$barFunc,
            geom = "col",
            mapping = aes(fill = fillPara),
            color = bordercolor(),
            width = input$barwidth / 100, # Reusing your box width slider
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
            # shape = as.numeric(input$pointshape),
          )+
          scale_color_manual(values = unlist(dpPaletteTheme()))+
          scale_fill_manual(values = unlist(dpPaletteFillTheme()))+
          scale_shape_manual(values=as.numeric(dpshape()))
      } else {p <- q}
    }
    
    #Other common ggproto objects
    pN <- p +
      # scale_color_manual(values = bordercolor()) +
      theme_classic() +
      labs(
        y = input$aytitle,
        x = input$axtitle) +
      
      geom_textbox(data = PlotTitPos(),
                   aes(x=x,y=y,label=(text), family = fontfamily()), size = input$plotFont/2.5,
                   width = unit(input$bWidthTitle/100, "npc"), box.colour = 'black', box.size = unit(input$lineTitle/83.33, 'pt'), 
                   box.padding = unit(input$padTitle/20,'pt'),position = 'identity',
                   box.margin = unit(0,'pt'), halign=(as.numeric(titlePlot())))+
      Ycontax() + #scale_y_continuous logic
      coord_cartesian(ylim = c(yaxisMin(), yaxisMax()), clip = 'off')  +
      theme(
        # plot.title = element_textbox_simple(size = input$plotFont, color = 'black',
        #                                     halign = as.numeric(input$titlePos)),
        axis.text.x = element_markdown(size = input$Xfontcol, color = "black",
                                       angle = as.numeric(input$Xrotate),
                                       hjust = xcolPos()),
        axis.title.x = element_textbox(size = input$Xfontsz, color = "black",
                                       width = unit(input$Xlinebreak*5,"pt"),
                                       hjust = 0.5,
                                       halign = 0.5,
                                       padding = margin(5,0,0,0)),
        axis.text.y = element_text(size = input$Yfontcol, color = "black"),
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
        plot.margin = margin(t = plotTopM(), r = plotRightM(), l = 10, b = 10, unit = "pt"),
        panel.border = plotBorder())
    
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
                     position = position_dodge(width = 0.2),
                     linewidth = 1,
                     linejoin = 'mitre')+ 
        
        geom_segment(segAdd(),
                     mapping =aes(x = xend, xend = xend, y = yend, yend = yendR),
                     position = position_dodge(width = 0.2),
                     linewidth = 1,
                     linejoin = 'mitre') +
        
        geom_richtext(segAdd(),mapping=aes(x=as.numeric(xT), y=as.numeric(yT),
                                           label=text, family = fontfamily(),
                                           hjust =  pvalHalign(),
                                           angle = ifelse(isTRUE(input$flipPlot), 270, 0)),
                      label.padding = unit(c(1),"pt"),
                      fill = ifelse(isTRUE(input$flipPlot),input$plotColor, 'white'),
                      label.colour= NA, size = segAdd()$size, vjust = segAdd()$vjust)+
        
        geom_segment(segAdd(),
                     mapping=aes(x = x, xend = xend,y = y, yend = yend),
                     position = position_dodge(width = 0.2),
                     linewidth = 1,
                     linejoin = 'mitre')
    }
    ## For Y-axis break
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
    if (isTRUE(input$flipPlot)){
      pR <- pF + coord_flip()
    } else {
      pR <- pF
    }
    if (isTRUE(input$reverseX)){
      pFinal <- pR + scale_x_discrete(limits = rev)
    } else {
      pFinal <- pR
    }
    return(pFinal)
    
  })
  
  
  ### Graph Output Processing ###
  output$graphFinal <- renderPlot({
    plotinput()
  })
  
  
  
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
        dpi = as.numeric(input$selectDPI)
      )
    }
  )
  
  plotContent <- reactive({
    tagList(
      div(
        style = "margin-bottom: 20px;",
        uiOutput("FinalPlot")
      ),
      div(
        style = "display: flex; flex-direction: row; justify-content: center; align-items: center; gap: 20px; margin-bottom: 20px;",
        div(
          style = "display: flex; flex-direction: column; gap: 10px; width: 32%;",
          div(style="display: inline-flex; flex-direction:row !important; gap:10px;
              justify-content:space-between; align-items:center;",
              numericInputIcon('width', label = "Plot Width", 
                               min = 100, max = 800, value = 500, width = "100%"),
              div(prettyToggle('lockRatio', label_on = NULL, label_off = NULL, 
                               icon_on = icon('link'), icon_off = icon('link-slash'), 
                               status_on = 'primary', status_off = 'warning',
                               fill = F, plain = F, bigger = T, thick = F, 
                               shape='round', inline = T, width = '0px'),
                  style="width:10px; height:0; margin-right:10px;"),
              numericInputIcon('height', label = "Plot Height", 
                               min = 100, max = 800, value = 400, width = "100%")
          ),
          actionButton('resetSize','Reset Size', icon = icon('rotate-left'))
        ),
        div(
          style = "display: flex; flex-direction: column; gap: 10px; width: 32%;",
          div(style="display:inline-flex; flex-direction: row !important; gap: 10px;",
              selectInput("selectFileType", "Save as", choices = c('PNG' = 'png', 'JPEG' = 'jpeg', 'TIFF' = 'tiff', 
                                                                   'SVG' = 'svg', 'EPS' = 'eps'), width = "100%"),
              radioGroupButtons("selectDPI", 'Select Resolution (DPI)',
                                choices = c(72, 96, 150, 300, 400, 600), size = 'sm', selected = 150)
          ),
          downloadButton("downloadBPlot", "Download Plot", icon = icon("download"), title = "Click to download your plot")
        )
      ),
      div(
        style = "display: flex; flex-direction: row; gap: 15px; margin-bottom: 20px; height:50px;
            align-items:start;justify-content:center;",
        downloadButton("savesetting", "Export Settings", icon = icon("gear"), class = "btn-primary", title = "Save your plot settings"),
        fileInput("usesetting", label = NULL, buttonLabel = "Import Settings File", accept = c(".xlsx")),
        actionButton("reuseset", "Upload", icon=icon('file-import'),
                     title = "Click to load your saved xlsx file to reuse previous setting")
      )
    )
  })
  
  # Conditional content for Graph panel
  output$graph_main_content <- renderUI({
    if (!isTruthy(input$submitFile) && !isTruthy(input$pasteBtn)) {
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
      
      if (!any(grepl(":", col_names, fixed = TRUE))) {
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
      plotContent()
    }
  })
  
  plotwidth <- reactive({
    width <- 200 #fallback
    if (is.na(input$width)){
      showNotification("Plot width cannot be blank", type='error')
    } else if(input$width>800){
      width <- 800
      showNotification("Plot width cannot be more than 800", type = 'error')
    }else if(input$width<200){
      width <- 200
      showNotification("Plot width cannot be lesser than 200", type = 'error')
    }else{
      width <- input$width
    }
    
    return(width)
  })
  plotheight <- reactive({
    height <- 200 
    if (is.na(input$height)){
      showNotification("Plot height cannot be blank", type='error')
    } else if(input$height>600){
      height <- 600
      showNotification("Plot height cannot be more than 600", type = 'error')
    }else if(input$height<200){
      height <- 200
      showNotification("Plot height cannot be lesser than 200", type = 'error')
    }else{
      height <- input$height
    }
    
    return(height)
  })
  
  observeEvent(input$resetSize,{
    tagList(
      updateNumericInput(session,'width', label = "Plot Width",
                         min = 100, max = 800, value = 500),
      updateNumericInput(session,'height', label = "Plot Height",
                         min = 100, max = 800, value = 400)
    )
  })
  
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
  output$FinalPlot <- renderUI({
    plotOutput(
      "graphFinal",
      width = plotwidth(),
      height = plotheight()
    )
    
  })
  observe({
    if (isFALSE(input$endTrim)){
      showNotification("Extended ends could be deceptive!", type = 'warning')
    }
  }) 
  
  ##################
  ### Statistics ###
  ##################
  
  # Choice names for sample test type
  sampChoice <- reactive({
    icon_html <- as.character(tags$span(id = "suggest", suggestIcon))
    if (ncol(data()) == 2) {
      c(
        paste0("Two-sample test ", icon_html),
        "Several samples test"
      )
    } else {
      c(
        "Two-sample test",
        paste0("Several samples test ", icon_html)
      )
    }
  })
  # Choice values for sample test types
  selcChoice <- reactive({
    if (ncol(data()) == 2) "tSt" else "sSt"
  })
  
  # Choice names for parametric test types
  sampChoicePara <- reactive({
    icon_html <- as.character(tags$span(id = "suggest", suggestIcon))
    normP <- as.numeric(normTest()$`P Value`)
    if (isFALSE(any(normP<0.05))) {
      c(
        paste0("Parametric ", icon_html),
        "Nonparametric"
      )
    } else {
      c(
        "Parametric",
        paste0("Nonparametric ", icon_html)
      )
    }
  })
  #Choice values for parametric test types
  selcChoicePara <- reactive({
    normP <- as.numeric(normTest()$`P Value`)
    if (isFALSE(any(normP<0.05))) "para" else "nonpara"
  })
  
  output$testInput <- renderUI({
    req(data())
    tagList(
      
      radioGroupButtons(
        "ttestType",
        "Select Test Type",
        choiceNames =  sampChoice(),
        choiceValues = c('tSt','sSt'),
        selected = selcChoice(),
        direction = "vertical",
        justified = TRUE,
        width = '100%'
      ), radioGroupButtons(
        'paratestType',
        "Select Test Type",
        choiceNames = sampChoicePara(),
        choiceValues = c('para','nonpara'),
        selected = selcChoicePara(),
        direction = 'vertical',
        justified = TRUE,
        width = '100%'
      ), p("Note: The checkmarked tests are suggested based on your data.",
           style = "color: grey; font-size:13px;"
      ), conditionalPanel(
        condition = "input.ttestType == 'tSt'",
        prettySwitch(
          'askPaired', 'Paired Samples',
          value = F, fill = T, status = 'success'
        )),
      conditionalPanel( 
        condition = "input.ttestType == 'sSt'",
        prettySwitch(
          'askComp', 'Perform Multiple Comparisons',
          fill = T, status = 'success', value = T
        ),prettySwitch(
          'askPairedssT', 'Repeated-Measured Samples',
          value = F, fill = T, status = 'success'
        )
      )
    )
  })
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
  
  ## PostHoc Selection Modal ##
  observeEvent(input$runAnalysis, {
    showModal(modalDialog(
      title = "Post Hoc Analysis",
      tagList(
        conditionalPanel(
          condition = "input.askComp == true",
          
          # Only show comparison options for several sample test type
          conditionalPanel(
            condition = "input.ttestType == 'sSt'",
            
            # Comparison type selector: only visible when data is NOT grouped
            # (When dataGroup == true, this input is hidden but its value persists)
            conditionalPanel(
              condition = "!input.dataGroup",
              pickerInput(
                inputId = 'compList',
                label = 'Select Group Comparison Type',
                choices = c(
                  'Compare against the control' = 'controlC',
                  'Compare against each other' = 'groupC'
                ),
                selected = 'groupC',
                width = '100%'
              )
            ),
            
            # Branch 1: Pairwise / all-vs-all comparisons (groupC)
            # This panel shows when compList == 'groupC'
            # It works even when dataGroup == true (compList hidden, but value remains 'groupC' by default)
            conditionalPanel(
              condition = "input.compList == 'groupC'",
              
              # Group selection (multiple columns/groups)
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
              
              # Correction method: non-parametric, unpaired, non-grouped data
              conditionalPanel(
                condition = "input.paratestType == 'nonpara' && !input.askPairedssT && !input.dataGroup",
                pickerInput(
                  inputId = 'askCorrection',
                  label = "P Value Correction Method",
                  choices = c("None" = 'none', 'Bonferroni' = 'bonferroni', 'Holm-Sidak' = 'holm'),
                  selected = 'bonferroni',
                  width = '100%'
                )
              ),
              
              # Correction method: parametric, paired, non-grouped data
              conditionalPanel(
                condition = "input.paratestType == 'para' && input.askPairedssT && !input.dataGroup",
                pickerInput(
                  inputId = 'askCorrectionP',
                  label = "P Value Correction Method",
                  choices = c("None" = 'none', 'Bonferroni' = 'bonferroni', 'Holm-Sidak' = 'holm'),
                  selected = 'bonferroni',
                  width = '100%'
                )
              ),
              
              # Correction method: non-parametric, grouped data (includes Tukey for post-hoc)
              conditionalPanel(
                condition = "input.paratestType == 'nonpara' && input.dataGroup",
                pickerInput(
                  inputId = 'askCorrectionG',
                  label = "P Value Correction Method",
                  choices = c("None" = 'none', 'Tukey' = 'tukey', 'Bonferroni' = 'bonferroni', 'Holm-Sidak' = 'holm'),
                  selected = 'bonferroni',
                  width = '100%'
                )
              )
            ),
            
            # Branch 2: Compare against a control group (controlC)
            conditionalPanel(
              condition = "input.compList == 'controlC'",
              
              # Select which group is the control
              pickerInput(
                inputId = 'askControl',
                label = 'Select Control',
                choices = current_colnames(),
                selected = current_colnames()[1],
                width = '100%'
              ),
              
              # Dynamic UI for specific comparisons (defined elsewhere in server)
              uiOutput('testCompInp'),
              
              # Correction method: non-parametric, unpaired (control comparison)
              conditionalPanel(
                condition = "input.paratestType == 'nonpara' && !input.askPairedssT",
                pickerInput(
                  inputId = 'askCorrectionC',
                  label = "P Value Correction Method",
                  choices = c("None" = 'none', 'Bonferroni' = 'bonferroni', 'Holm-Sidak' = 'holm'),
                  selected = 'bonferroni',
                  width = '100%'
                )
              ),
              
              # Correction method: parametric, paired (control comparison)
              conditionalPanel(
                condition = "input.paratestType == 'para' && input.askPairedssT",
                pickerInput(
                  inputId = 'askCorrectionCG',
                  label = "P Value Correction Method",
                  choices = c("None" = 'none', 'Bonferroni' = 'bonferroni', 'Holm-Sidak' = 'holm'),
                  selected = 'bonferroni',
                  width = '100%'
                )
              )
            ),
            
            # Additional correction options (not tied to a specific compList branch)
            
            # Non-parametric, paired, non-grouped (not inside controlC or groupC branch)
            conditionalPanel(
              condition = "input.paratestType == 'nonpara' && input.askPairedssT && !input.dataGroup",
              pickerInput(
                inputId = 'askCorrectionPC',
                label = "P Value Correction Method",
                choices = c("None" = 'none', 'Bonferroni' = 'bonferroni', 'Holm-Sidak' = 'holm'),
                selected = 'bonferroni',
                width = '100%'
              )
            ),
            
            # Parametric, grouped data (not inside controlC or groupC branch)
            conditionalPanel(
              condition = "input.paratestType == 'para' && input.dataGroup",
              pickerInput(
                inputId = 'askCorrectionPCG',
                label = "P Value Correction Method",
                choices = c("None" = 'none', 'Bonferroni' = 'bonferroni', 'Holm-Sidak' = 'holm'),
                selected = 'bonferroni',
                width = '100%'
              )
            )
          )
        ), conditionalPanel(
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
        ), conditionalPanel(
          condition = "input.askComp==false && input.ttestType=='sSt'",
          p('Turn on "Multiple Comparison" to compare between groups or run only omnibus test.', 
            style = 'font-weight:bolder;')
        )
      ),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("runAnalysisFinal", "Run Analysis", class = "btn-primary")
      )
    ))
    
  })
  
  #Submit Analysis Button processing
  vars <-  reactiveValues(count = 0)
  
  output$submitAnalysis <- renderUI({
    req(data())
    actionButton('runAnalysis', submitLabel()[[1]], icon = icon(submitLabel()[[2]]))
  })
  
  observe({
    if (!is.null(input$runAnalysis)){
      input$runAnalysis
      isolate({
        vars$count <- vars$count + 1
      })
    }
  })
  submitLabel <- reactive({
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
  
  ###### All the Statistics test processing #####
  
  
  
  ###Normality test###
  
  normTest <- reactive({
    #generation of Normality test report
    normdf <- data.frame()
    
    dataN <- na.omit(data()) 
    
    if (nrow(dataN) > 3000 || nrow(dataN) < 3) {
      data.frame(
        Test_Report = c(
          'Can not perform Shapiro test for sample number less than 3 or more than 3000.'
        )
      )
    } else{
      # Shapiro-Wilk test for normal/ skewed distribution check
      for (i in 1:ncol(dataN)) {
        tempdf <- tidy(shapiro.test(dataN[, i]))
        normdf <- rbind(normdf, tempdf)
      }
      testrep <- data.frame(normdf[, 1:3])
      newcol <- data.frame("condition" = as.character(colnames(dataN)))
      testrep <- cbind(testrep, newcol)
      testrep <- testrep
      df <- data.frame()
      for (i in 1:nrow(testrep)) {
        if (as.numeric(testrep[i, 2]) < 0.05) {
          tempdf2 <- c('No')
          df <- rbind(df, tempdf2)
        } else if (as.numeric(testrep[i, 2]) > 0.05) {
          tempdf2 <- c("Yes")
          df <- rbind(df, tempdf2)
        }
      }
      pval <- testrep[,2]
      astrM <- asterisk(pval)
      testrep <- cbind(testrep, df, astrM)
      testrep <- data.frame(testrep[, 4], testrep[1], testrep[2], testrep[6], testrep[5])
      colnames(testrep) <- c( 'Condition', 'Shapiro-Wilk Statistics',
                              'P Value', 'Significance', 'Passed normality test (P<0.05)?')
      
      
      return(testrep)
    }
  })
  
  ###Descriptive Statistics###
  
  descStat <- reactive({
    descTab <- data.frame()
    for (i in 1:ncol(data())) {
      descStat <- data.frame(as.matrix(summary(na.omit(data()[[i]]))))
      descStat <- rbind(length(na.omit(data()[[i]])), descStat)
      descSD <- apply(data()[i], 2, sd, na.rm = T)
      descErr <- descSD / sqrt(length(na.omit(data()[[i]])))
      descStat <- rbind(descStat, descSD, descErr)
      colnames(descStat) <- c(colnames(data()[i]))
      descTab <- append(descTab, descStat)
    }
    descTab <- data.frame(descTab, check.names = F)
    
    cols <- c(
      'Condition', 'N', 'Minimum', "1st Quartile", 'Median',
      'Mean', '3rd Quartile', 'Maximum', 'Std. Dev.', 'Std. Err.'
    )
    descTab <- data.frame(t(descTab))
    descTab <- cbind(colnames(data()),descTab)
    colnames(descTab) <- cols
    return(descTab)
  })
  
  #Levene's test for equal variance for full stat report
  levTest <- reactive({
    levdf <- data.frame(tidy(car::leveneTest(value ~ variable, na.omit(orderdata()))))
    if (levdf$p.value < 0.05)
      (temp <- c(
        'The groups do not have equal variances on the dependent variable'
      ))
    else{
      temp <- c('The groups have approximately equal variances on the dependent variable')
    }
    astrM <- asterisk(levdf$p.value)
    levdf <- data.frame(formatC(levdf[, 1], format = 'g', digits = 3), levdf[, 3],
                        formatC(levdf[, 2], format = 'g', digits = 3),
                        astrM, temp)
    colnames(levdf) <- c('F Statistics', 'DF', 'P Value', 'Significance', 'Remarks')
    return(levdf)
  })
  
  ### Two Sample test processing ###
  
  tsTest <- reactive({
    testdata <- data()
    colname <- (input$statTwoCol) # Selected columns for t test
    
    x <- match(colname[1],current_colnames())
    y <- match(colname[2],current_colnames())
    
    if (input$paratestType == "para") {
      if(as.numeric(levTest()$`P Value`)>0.05){
        varEql <- TRUE
      } else { varEql <- FALSE}
      
      testrow <- t.test(testdata[x], testdata[y], alternative = c('two.sided'), 
                        paired = input$askPaired,
                        var.equal = varEql)
      testrow <- tidy(testrow)
      astrN <- asterisk(testrow$p.value)
      colHead <- c('Comparisons', 'Difference in Means', 'Confidence Interval', 't Statistics', 'P Value', 'Significance')
      df <- data.frame(paste(colname[1],' vs ', colname[2], sep = ''),
                       as.numeric(formatC(abs(testrow$estimate),digits = 3, format = 'f')),
                       paste(as.numeric(formatC(abs(testrow$conf.low),digits = 3, format = 'f')),
                             ', ', as.numeric(formatC(abs(testrow$conf.high),digits = 3, format = 'f')), sep=''),
                       abs(testrow$statistic),
                       formatC(testrow$p.value, format='g', digits = 3),
                       astrN)
      colnames(df) <- colHead
    } else if (input$paratestType == "nonpara"){
      testrow <- tidy(stats::wilcox.test(as.numeric(unlist(testdata[x])),as.numeric(unlist(testdata[y])), alternative = c('two.sided'),
                                         paired = input$askPaired, correct = T))
      testRank <- mwz(testdata[x],testdata[y])
      
      astrN <- asterisk(testrow$p.value)
      colHead <- c('Comparisons','Mann Whitney U Statistics', 'z Statistics', 
                   paste0('Rank Sum ', colname[1],sep=''),
                   paste0('Rank Sum ', colname[2], sep=''),
                   'Difference in Rank Sum',
                   'P Value', 'Significance')
      df <- data.frame(paste(colname[1],' vs ', colname[2], sep = ''),
                       formatC(as.numeric(testrow$statistic),format='f', digits = 3),
                       testRank$z_Statistics, testRank$RankSum_X, testRank$RankSum_Y,
                       abs(testRank$RankSum_X-testRank$RankSum_Y),
                       formatC(as.numeric(testrow$p.value), format = 'g',digits = 3),
                       astrN
      )
      colnames(df) <- colHead
    }
    return(df)
  })|> bindEvent(input$runAnalysisFinal)
  
  ### Several Sample test processing ###
  
  ssTest <- reactive({
    req(data())
    if (isTRUE(input$askComp) && input$ttestType == 'sSt'){
      if(input$compList == 'controlC'){
        colname <- c(input$askControl,input$statContCols)
      } else if (input$compList == 'groupC') {
        colname <- (input$statCols)
      }
    } else if (isFALSE(input$askComp) && input$ttestType == 'tSt'){
      colname <- (input$statTwoCol)
    } else{
      colname <- colnames(data())
    }
    
    widedata <- data() |> select(all_of(colname))
    
    ## ANOVA Test for parametric data
    if(input$paratestType == 'para'){
      ## For Parametric Test
      if(isTRUE(input$askPairedssT)){
        # For Paired data
        widedata$ID <- row.names(widedata)
        if(input$dataGroup == T){
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
        if (input$dataGroup == T){
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
        # Heteroscedasticity Correction if Levene test gives significant p value
        het_sce <- ifelse (as.numeric(levTest()$`P Value`)>0.05, FALSE, TRUE)
        
        if (input$dataGroup == T){
          # For Groupued data
          aovTest <- rstatix::anova_test(longdata, val ~ para*groups, type= 3,
                                         white.adjust = het_sce)
        } else {
          # For Ungrouped data
          aovTest <- rstatix::anova_test(longdata, val ~ para, type= 3,
                                         white.adjust = het_sce)
        }   
      }
      if(input$dataGroup == T){
        aovTest <- data.frame(aovTest$Effect,aovTest$F, aovTest$DFn, aovTest$DFd, aovTest$p)
      } else {
        aovTest <- data.frame(aovTest$F, aovTest$DFn, aovTest$DFd, aovTest$p)
      }
      asterN <- asterisk(aovTest[,ncol(aovTest)])
      aovTest <- cbind(aovTest,asterN)
      if (input$dataGroup == T){
        colnames(aovTest) <- c('Effect','F Statistics', 'DFn', 'DFd', 'P Value', 'Significance')
        aovTest$Effect <- c(input$legTitle,'Groups',paste0(input$legTitle,' to Groups'))
      }else{
        colnames(aovTest) <- c('F Statistics', 'DFn', 'DFd', 'P Value', 'Significance')
      }
      
      aovTest$`P Value` <- formatC(aovTest$`P Value`, format = 'g', digits = 3)
      return(aovTest)
    } else {
      ## For Nonparametric Test
      
      if(isTRUE(input$askPairedssT)){
        widedata$ID <- row.names(widedata)
        if (input$dataGroup == T){
          longdata <- na.omit(widedata) |> pivot_longer(cols = -ID, 
                                                        names_to = c('para','groups'),
                                                        names_sep = ':',
                                                        values_to = 'val')
          model <- ARTool::art(data = na.omit(longdata), 
                               formula = val ~ factor(para)*factor(groups) + (1|ID))
          nparaTest <- anova(model)
        } else {
          longdata <- widedata |> pivot_longer(cols = -ID, 
                                               names_to = 'para', values_to = 'val')
          nparaTest <- rstatix::friedman_test(longdata, val ~ para | ID)
        }
        
      } else {
        if (input$dataGroup == T){
          longdata <- widedata |> pivot_longer(cols = everything(), 
                                               names_to = c('para','groups'),
                                               names_sep = ':',
                                               values_to = 'val')
          model <- ARTool::art(data = na.omit(longdata), 
                               formula = val ~ factor(para)*factor(groups))
          nparaTest <- anova(model)
        } else{
          longdata <- widedata |> pivot_longer(cols = everything(), 
                                               names_to = 'para', values_to = 'val')
          nparaTest <- rstatix::kruskal_test(longdata, val ~ para)
        }
        
      }
      if (input$dataGroup == T){
        nparaTest <- data.frame(nparaTest$Term, nparaTest$`F`, nparaTest$Df,
                                nparaTest$Df.res, nparaTest$`Pr(>F)`)
        
      }else {
        nparaTest <- data.frame(nparaTest$statistic, nparaTest$df, nparaTest$p)
      }
      
      asterN <- asterisk(nparaTest[,ncol(nparaTest)])
      nparaTest <- cbind(nparaTest,asterN)
      
      if (input$dataGroup == T){
        colnames(nparaTest) <- c('Effect','F Statistics', 'DFn', 'DFd', 'P Value', 'Significance' )
        nparaTest$Effect <- c(input$legTitle,'Groups',paste0(input$legTitle,' to Groups'))
      } else{
        colnames(nparaTest) <- c('H Statistics', 'DF', 'P Value', 'Significance' )
      }
      
      nparaTest$`P Value` <- formatC(nparaTest$`P Value`, format = 'g', digits = 3) 
      return(nparaTest)
    }
    
  }) |> bindEvent(input$runAnalysisFinal)
  
  ### Post Hoc Pairwise Test Processing ###
  phTest <- reactive({
    req(data())
    if (isTRUE(input$askComp) && input$ttestType == 'sSt'){
      if(input$compList == 'controlC'){
        colname <- c(input$askControl,input$statContCols)
      } else if (input$compList == 'groupC') {
        colname <- (input$statCols)
      }else{
        colname <- colnames(data())
      }
      widedata <- data() |>  select(all_of(colname))
      longdata <- widedata |>  pivot_longer(cols=everything(), 
                                            names_to = 'para',
                                            values_to = 'val') 
      longdata$para <- factor(longdata$para, levels = colname)   
      
      req(longdata)
      phDf <- NULL
      if (isFALSE(input$askPairedssT)){ 
        ## For independent samples (not repeated samples)
        
        # Parametric Control vs Groups
        if(input$compList == 'controlC' && input$paratestType == "para"){
          if (levTest()$`P Value`<0.05){
            # Unequal Variance - Dunnett T3 Test
            T3 <- PMCMRplus::tamhaneDunnettTest(x = longdata$val, 
                                                g = relevel(factor(longdata$para), 
                                                            ref = input$askControl))
            phDf <- data.frame(paste(colnames(T3$p.value), '-', 
                                     rownames(T3$p.value)), T3$statistic,
                               T3$p.value, stringsAsFactors = F)
            astrN <- asterisk(phDf[,3])
            phDf[,3] <- ifelse( phDf[,3]== 0 | phDf[,3] < 2.2e-16,
                                "< 2.2e-16",
                                formatC(phDf[,3], format = "g", digits = 4))
            phDf <- cbind(phDf,astrN)
            colnames(phDf) <- c('Comparisons', 'T Statistics', 'P Value', 'Significance')
          } else {
            # Equal Variance - Dunnett's Test
            dT <- DescTools::DunnettTest(x = longdata$val, 
                                         g = relevel(factor(longdata$para),
                                                     ref = input$askControl))
            phDf <- as.data.frame(dT[[1]])
            phDf <- data.frame(rownames(phDf),phDf[,1], paste0(formatC(phDf[,2], format = 'f', digits = 3)
                                                               ,' to ',formatC(phDf[,3], format = 'f', digits = 3), sep=''),
                               phDf[,4], asterisk(phDf[,4]))
            colnames(phDf) <- c('Comparisons','Difference Between Means',
                                '95% CI of Mean Diff', 'P Value', 'Significance')
            phDf[,4] <- ifelse( phDf[,4]== 0 | phDf[,4] < 2.2e-16,
                                "< 2.2e-16",
                                formatC(phDf[,4], format = "g", digits = 4))
          }
          # Parametric all pairwise groups 
        } else if (input$compList == 'groupC' && input$paratestType == "para" &&
                   levTest()$`P Value`<0.05) {
          # For unequal variance - Games-Howell
          phDf <- rstatix::games_howell_test(longdata, val ~ para, detailed = T)
          astrN <- asterisk(phDf$p.adj)[[1]]
          phDf <- data.frame(paste0(phDf$group1,'-',phDf$group2, sep=''), -(phDf$estimate),
                             paste0(formatC(-(phDf$conf.low), format = 'f', digits = 3)
                                    ,' to ',formatC(-(phDf$conf.high), format = 'f', digits = 3)),
                             phDf$df, phDf$p.adj, astrN)
          colnames(phDf) <- c('Comparisons', 'Difference Between Means', 
                              '95% CI of Mean Diff','DFd', 'P Value Adjusted', 'Significance')
          phDf$`P Value Adjusted` <- formatC(phDf$`P Value Adjusted`, format = 'g', digits = 3)
          
        } else if (input$compList == 'groupC' && input$paratestType == "para" &&
                   levTest()$`P Value`>0.05) {
          # For equal variance - Tukey's HSD
          phDf <- rstatix::tukey_hsd(longdata, val ~ para, detailed = T)
          astrN <- asterisk(phDf$p.adj)[[1]]
          phDf <- data.frame(paste0(phDf$group1,'-',phDf$group2, sep=''), -(phDf$estimate),
                             paste0(formatC(-(phDf$conf.low), format = 'f', digits = 3)
                                    ,' to ',formatC(-(phDf$conf.high), format = 'f', digits = 3)),
                             phDf$p.adj, astrN)
          colnames(phDf) <- c('Comparisons', 'Difference Between Means', 
                              '95% CI of Mean Diff', 'P Value Adjusted', 'Significance')
          phDf$`P Value Adjusted` <- formatC(phDf$`P Value Adjusted`, format = 'g', digits = 3)
          
        } else if (input$compList == 'controlC' && input$paratestType == "nonpara"){
          # For nonparametric Control vs Groups - Dunn's test
          phDf <- longdata |> rstatix::dunn_test(val ~ para, p.adjust.method = input$askCorrectionC, 
                                                 detailed = T) |> data.frame()
          astrN <- asterisk(phDf$p.adj)
          phDf <- data.frame(paste0(phDf$group1,'-',phDf$group2, sep=''), -(phDf$estimate), -(phDf$statistic),
                             phDf$p, phDf$p.adj, astrN)
          colnames(phDf) <- c('Comparisons', 'Mean Rank Difference', 'Z Statistic',
                              'Raw P Value', 'Adjusted P Value', 'Significance')
          phDf <- phDf |> filter(grepl(input$askControl, Comparisons))
          phDf$`Raw P Value` <- formatC(phDf$`Raw P Value`, format = 'g', 
                                        digits = 3)
          phDf$`Adjusted P Value` <- formatC(phDf$`Adjusted P Value`, format = 'g', 
                                             digits = 3)
        } else if (input$compList == 'groupC' && input$paratestType == "nonpara"){
          # For nonparametric all pairwise groups - Dunn's test
          phDf <- longdata |> rstatix::dunn_test(val ~ para, p.adjust.method = input$askCorrection, 
                                                 detailed = T) |> data.frame()
          astrN <- asterisk(phDf$p.adj)
          phDf <- data.frame(paste0(phDf$group1,'-',phDf$group2, sep=''), -(phDf$estimate), -(phDf$statistic),
                             phDf$p, phDf$p.adj, astrN)
          colnames(phDf) <- c('Comparisons', 'Mean Rank Difference', 'Z Statistic',
                              'Raw P Value', 'Adjusted P Value', 'Significance')
          phDf$`Raw P Value` <- formatC(phDf$`Raw P Value`, format = 'g', 
                                        digits = 3)
          phDf$`Adjusted P Value` <- formatC(phDf$`Adjusted P Value`, format = 'g', 
                                             digits = 3)
        }
      } else {
        ## For repeated measurement
        if (input$paratestType == 'nonpara'){
          #nonparametric - Conover's test
          cT <- PMCMRplus::frdAllPairsConoverTest(as.matrix(widedata),
                                                  p.adjust=input$askCorrectionPC)
          cT <- summary(cT)
          phDf <- TabToVec(cT)
          astrN <- asterisk(phDf[,3])
          phDf <- data.frame(phDf,  astrN)
          phDf[,3] <- ifelse( phDf[,3]== 0 | phDf[,3] < 2.2e-16,
                              "< 2.2e-16",
                              formatC(phDf[,3], format = "g", digits = 3))
          colnames(phDf) <- c('Comparisons', 'T Statistics', 
                              'Adjusted P Value', 'Significance')
          if (input$compList=='controlC'){
            phDf <- phDf |> filter(grepl(input$askControl,Comparisons))
            return(phDf)
          }else{
            return(phDf)
          }
        } else {
          #Parametric repeated Anova - pairwise-t-test
          if (input$compList=='controlC'){
            ptT <- rstatix::pairwise_t_test(longdata, val ~ para, p.adjust.method = input$askCorrectionCG,
                                            paired = T, ref.group = input$askControl)
          } else {
            ptT <- rstatix::pairwise_t_test(longdata, val ~ para, p.adjust.method = input$askCorrectionP,
                                            paired = T)
          }
          phDf <- data.frame(paste0(ptT$group1,'-',ptT$group2), ptT$statistic,
                             ptT$df, ptT$p,ptT$p.adj,asterisk(ptT$p.adj))
          colnames(phDf) <- c('Comparisons', 'T Statistics', 'Dfd', 
                              'Raw P Value', 'Adjusted P Value', 'Significance')
          phDf[,5] <- ifelse(phDf[,5]==0 | phDf[,5]<2.2e-16, "2.2e-16", 
                             formatC(phDf[,5], format = 'g', digits = 3))
          phDf[,4] <- ifelse(phDf[,4]==0 | phDf[,4]<2.2e-16, "2.2e-16", 
                             formatC(phDf[,4], format = 'g', digits = 3))
        }
      }
      return(phDf)
    } else {
      return(NULL)
    }
  }) |> bindEvent(input$runAnalysisFinal)
  
  
  ### Grouped data posthoc test ###
  
  phTestG <- reactive({
    req(data())
    widedata <- data() |>  select(all_of(input$statCols))
    
    if(isTRUE(input$askPairedssT)){
      widedata$ID <- row.names(widedata)
      longdata <- widedata |>  pivot_longer(cols=-ID, 
                                            names_to = c('para','groups'),
                                            names_sep = ':',
                                            values_to = 'val')
    }else{
      longdata <- widedata |>  pivot_longer(cols=everything(), 
                                            names_to = c('para','groups'),
                                            names_sep = ':',
                                            values_to = 'val')
    }
    longdata$para <- factor(longdata$para)
    longdata$groups <- factor(longdata$groups)
    
    if(input$paratestType == 'nonpara'){
      if(isTRUE(input$askPairedssT)){
        model <- art(data=na.omit(longdata), 
                     formula= val ~ para*groups + (1|ID))
      } else {
        model <- art(data=na.omit(longdata), 
                     formula= val ~ para*groups)
      }
      phDfA <- art.con(model, ~para,
                       adjust = input$askCorrectionG) |> summary()
      phDfB <- art.con(model, ~groups,
                       adjust = input$askCorrectionG) |> summary()
      phDfAB <- art.con(model, ~para*groups,
                        adjust = input$askCorrectionG) |> summary()
      
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
      
      phDfA <- phDf_format(phDfA)
      phDfB <- phDf_format(phDfB)
      phDfAB <- phDf_format(phDfAB)
      
      phList <- list(phDfA, phDfB, phDfAB)
      names(phList) <- c("Groups", input$legTitle,
                         paste("Groups:", input$legTitle, collapse = '') )
      
    } else {
      if(isTRUE(input$askPairedssT)){
        model <- lme4::lmer(data=na.omit(longdata), 
                            formula = val ~ para*groups + (1|ID))
      } else {
        model <- stats::aov(data=na.omit(longdata), 
                            formula = val ~ para*groups)
      }
      phDfA <- emmeans(model, pairwise~para,
                       adjust = input$askCorrectionPCG)$contrasts |> summary()
      phDfB <- emmeans(model, pairwise~groups,
                       adjust = input$askCorrectionPCG)$contrasts |> summary()
      phDfAB <- emmeans(model, pairwise~para*groups,
                        adjust = input$askCorrectionPCG)$contrasts |> summary()
      
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
      
      
      phDfA <- phDf_format(phDfA)
      phDfB <- phDf_format(phDfB)
      phDfAB <- phDf_format(phDfAB)
      
      phList <- list(phDfA, phDfB, phDfAB)
    }
    return(phList)
  }) |> bindEvent(input$runAnalysisFinal)
  
  ### Test Summary Description Logic ###
  
  testSumm <- reactive({
    if (input$ttestType == 'tSt') {
      # Two-sample tests (unchanged — already correct)
      if (input$paratestType == 'nonpara') {
        if (isTRUE(input$askPaired)) {
          "Experimental Summary: Wilcoxon signed-rank test (paired samples)"
        } else {
          "Experimental Summary: Mann-Whitney U test (independent samples)"
        }
      } else {  # parametric
        if (isTRUE(input$askPaired)) {
          "Experimental Summary: Paired Student's t-test"
        } else {
          if (as.numeric(levTest()$`P Value`) < 0.05) {
            "Experimental Summary: Welch's t-test (unequal variances assumed)"
          } else {
            "Experimental Summary: Student's t-test (equal variances assumed)"
          }
        }
      }
      
    } else {  # sSt — several samples
      base_test <- ""
      posthoc_text <- ""
      
      if (input$paratestType == 'nonpara') {
        if(isTRUE(input$dataGroup)){
          base_test <- "Aligned Rank Transform (ART) test"
          posthoc_text <- " with ART-C post-hoc test"
          
        } else {
          if (isTRUE(input$askPairedssT)) {
            base_test <- "Friedman test (nonparametric repeated measures)"
            if (isTRUE(input$askComp)) {
              posthoc_text <- " with Conover's post-hoc test"
            }
          } else {
            base_test <- "Kruskal-Wallis test (nonparametric independent samples)"
            if (isTRUE(input$askComp)) {
              posthoc_text <- " with Dunn's post-hoc test"
            }
          }
        }
        
      } else {  # parametric
        if (isTRUE(input$askPairedssT)) {
          base_test <- "Repeated-measures ANOVA (with Greenhouse-Geisser correction for sphericity)"
          if (isTRUE(input$askComp)) {
            posthoc_text <- " with pairwise paired t-tests (p-value adjusted)"
          }
        } else {  # independent samples
          if (as.numeric(levTest()$`P Value`) < 0.05) {
            if (isTRUE(input$dataGroup)){
              base_test <- "Two-way ANOVA (unequal variances)"
            } else {
              base_test <- "Welch's one-way ANOVA (unequal variances)"
            }
          } else {
            if (isTRUE(input$dataGroup)){
              base_test <- "Two-way ANOVA (equal variances assumed)"
            } else {
              base_test <- "One-way ANOVA (equal variances assumed)"
            }
          }
          
          if (isTRUE(input$askComp)) {
            if (input$compList == 'controlC') {
              if (as.numeric(levTest()$`P Value`) < 0.05) {
                posthoc_text <- " with Dunnett's T3 post-hoc test (comparisons against control)"
              } else {
                posthoc_text <- " with Dunnett's post-hoc test (comparisons against control)"
              }
            } else {  # groupC — all pairwise
              if (as.numeric(levTest()$`P Value`) < 0.05) {
                posthoc_text <- " with Games-Howell post-hoc test (all pairwise comparisons)"
              } else {
                posthoc_text <- " with Tukey's HSD post-hoc test (all pairwise comparisons)"
              }
            }
            if (isTRUE(input$dataGroup)){
              posthoc_text <- " with pairwise t-tests (p-value adjusted)"
            }
          }
        }
      }
      
      paste0("Experimental Summary: ", base_test, posthoc_text)
    }
  })
  
  
  #### Significance Test report display ####
  
  observeEvent(input$runAnalysisFinal,{ #Upon Clicking Final Run Analysis button
    # Two-sample test: always need exactly 2 groups
    if (input$ttestType == "tSt") {
      if (length(input$statTwoCol) != 2) {
        show_alert(
          title = "Warning",
          text = "For two-sample test, select exactly 2 groups.",
          type = "warning"
        )
        return()  # Stop here, keep modal open
      }
    }
    
    # Several-sample test
    if (input$ttestType == "sSt") {
      if (isTRUE(input$askComp)) {
        if (input$compList == "controlC") {
          # Need control + at least 2 comparisons → total >=3
          if (is.null(input$statContCols) || length(input$statContCols) < 2) {
            show_alert(
              title = "Warning",
              text = "For control comparison, select at least 2 groups to compare against control.",
              type = "warning"
            )
            return()
          }
        } else if (input$compList == "groupC") {
          if (is.null(input$statCols) || length(input$statCols) < 3) {
            show_alert(
              title = "Warning",
              text = "For pairwise comparisons, select at least 3 groups.",
              type = "warning"
            )
            return()
          }
        }
      } else {
        # No multiple comparisons → need at least 3 columns in data
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
    Sys.sleep(3)
    hidePageSpinner()
    removeModal()
    
    showNotification("Analysis complete!", type = "message")
    
  },ignoreNULL = T)
  
  output$normStatTab <- renderTable({
    
    tableNorm <- normTest()
    tableNorm$`P Value` <- formatC(tableNorm$`P Value`, format = 'g', digits = 3)
    tableNorm$`Shapiro-Wilk Statistics` <- formatC(tableNorm$`Shapiro-Wilk Statistics`,
                                                   format = 'g', digits = 3)
    if (input$ttestType == 'tSt'){
      tableNorm <- tableNorm |> filter(Condition %in% input$statTwoCol)
    }else {
      tableNorm <- tableNorm
    }
    return(tableNorm)
  }, striped = T, width = '100%', align = 'l')|> bindEvent(input$runAnalysisFinal)
  
  output$normQQPlot <- renderPlot({
    req(data())
    if (input$ttestType == 'tSt'){
      df <- data() |> select(input$statTwoCol)
    } else {
      df <- data()
    }
    
    plot_list <- lapply(1:ncol(df), function(i) {
      force(i) 
      ggplot(df, aes(sample = .data[[colnames(df)[i]]])) +
        stat_qq_band(bandType = "pointwise", fill = "lavender", alpha = 0.5) +
        stat_qq_point(shape = 1) + 
        stat_qq_line() +
        ggtitle(colnames(df)[i]) +
        theme_classic() +
        labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
        theme(
          panel.border = element_rect(fill = NA, linewidth = 0.8),
          axis.line = element_line(linewidth = 0.8)
        )
    })
    patchwork::wrap_plots(plot_list)
  })
  
  observeEvent(input$showQQ,{
    showModal(modalDialog(
      title = "QQ Plots for Probability Distribution",
      plotOutput('normQQPlot', height = 600),
      size = 'xl',
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel")
      )
    ))
  })
  sigRepTit <- reactive({
    
    ## For parametric Two sample t-test or Anova-test titles
    if(as.numeric(levTest()$`P Value`)<0.05){
      tTitle <- "Welch's t-test report"
      sTitle <- "Welch's One-way ANOVA test report"
    } else {
      tTitle <- "Student's t-test report"
      sTitle <- "One-way ANOVA test report"
    }
    if (input$dataGroup == T){
      sTitle <- "Two-way ANOVA test report"
    }
    
    ## For nonparametric Two sample or several samples test titles
    if (isTRUE(input$askPaired)){
      mwTitle <- "Wilcoxon signed-rank test report"
    } else {
      mwTitle <-  "Mann-Whitney U test report"
    }
    if (isTRUE(input$askPairedssT)){
      npTitle <- "Friedman test report"
      
    } else {
      npTitle <-  "Kruskal Wallis test report"
    }
    if (input$dataGroup == T){
      npTitle <- "Aligned Rank Transform test report"
    }
    
    if (isTRUE(input$askComp)) {
      
      # Default correction text handling 
      get_corr_text <- function(corr) {
        if (corr == 'none') 'no' 
        else if (corr == 'holm') 'holm-sidak' 
        else if (corr == 'tukey') 'Tukey'  # only possible in some nonpara grouped cases
        else corr
      }
      
      if (isTRUE(input$askPairedssT)) {  # PAIRED TESTS
        
        if (input$paratestType == 'nonpara') {  # Non-parametric paired
          
          if (isTRUE(input$dataGroup)) {  # Grouped data > ART for factorial/repeated
            corr <- input$askCorrectionG
            corr_text <- get_corr_text(corr)
            posthoc_text <- paste("Post-hoc multifactor contrasts following Aligned Rank Transform (ART-C) with", 
                                  corr_text, "correction")
            
          } else {  # Non-grouped > Friedman + Conover
            corr <- input$askCorrectionPC
            corr_text <- get_corr_text(corr)
            posthoc_text <- paste("Conover's post-hoc test following Friedman test with", 
                                  corr_text, "correction")
          }
          
        } else {  # Parametric paired
          
          # Determine which correction input to use
          corr <- if (input$compList == 'controlC') input$askCorrectionCG else input$askCorrectionP
          corr_text <- get_corr_text(corr)
          
          if (input$compList == 'controlC') {
            posthoc_text <- paste("Paired t-tests against the control group with", corr_text, "correction")
          } else {
            posthoc_text <- paste("Pairwise paired t-tests with", corr_text, "correction")
          }
          
        }
        
      } else {  # UNPAIRED TESTS
        
        if (input$paratestType == 'nonpara') {  # Non-parametric unpaired
          
          if (isTRUE(input$dataGroup)) {  # Grouped data > ART for factorial/independent
            corr <- input$askCorrectionG
            corr_text <- get_corr_text(corr)
            posthoc_text <- paste("Post-hoc multifactor contrasts following Aligned Rank Transform (ART-C) with", 
                                  corr_text, "correction")
            
          } else {  # Non-grouped > Kruskal-Wallis + Dunn
            corr <- if (input$compList == 'controlC') input$askCorrectionC else input$askCorrection
            corr_text <- get_corr_text(corr)
            
            if (input$compList == 'controlC') {
              posthoc_text <- paste("Dunn's post-hoc tests for comparisons against the control with", 
                                    corr_text, "correction")
            } else {
              posthoc_text <- paste("Dunn's post-hoc test following Kruskal-Wallis test with", 
                                    corr_text, "correction")
            }
          }
          
        } else {  # Parametric unpaired
          
          if (input$compList == 'controlC') {
            # Dunnett's with built-in adjustment
            posthoc_text <- "Dunnett's post-hoc test following one-way ANOVA"
            
          } else {  # groupC - all pairwise
            
            if (isTRUE(input$dataGroup)) {
              # When data is grouped
              corr <- input$askCorrectionPCG
              corr_text <- get_corr_text(corr)
              posthoc_text <- paste("Pairwise independent samples t-tests with", corr_text, "correction")
              
            } else {
              # Classic one-way ANOVA post-hoc (equal/unequal variance)
              if (as.numeric(levTest()$`P Value`) < 0.05) {
                posthoc_text <- "Games-Howell post-hoc test following one-way ANOVA"
              } else {
                posthoc_text <- "Tukey's HSD post-hoc test following one-way ANOVA"
              }
            }
          }
        }
      }
    }
    
    finalTitles <- list(tTitle, sTitle, mwTitle, npTitle, posthoc_text)
  })
  npResult <- reactive({
    if (input$dataGroup == T){
      statM <- ifelse(input$paratestType=='nonpara', 'medians', 'means')
      npRes <- data.frame()
      for (i in 1:nrow(ssTest())){
        pState <- ifelse(as.numeric(ssTest()[i,5])<=0.05,'a','no')
        pState2 <- ifelse(as.numeric(ssTest()[i,5])<=0.05,' of at least two conditions within the "',' of conditions within the "')
        pStyle <- ifelse(as.numeric(ssTest()[i,5])<=0.05,'#B9EBDE','#F7D7D7')
        temp1 <- paste0("There is ",pState," significant difference between ",statM, 
                        pState2,ssTest()[i,1],'" Effect.')
        temp2 <- paste0("padding:5px; background-color:",pStyle,
                        "; border: 1px solid; border-radius:5px;")
        temp3 <- ifelse(as.numeric(ssTest()[i,5])<=0.05,'yes','no')
        temp <- cbind(temp1,temp2, temp3)
        npRes <- rbind(npRes,temp)
      }
      
    } else {
      statM <- ifelse(input$paratestType=='nonpara', 'medians', 'means')
      pState <- ifelse(as.numeric(ssTest()$`P Value`)<=0.05,'a','no')
      pState2 <- ifelse(as.numeric(ssTest()$`P Value`)<=0.05,' of at least two conditions within the',' of conditions within the')
      pStyle <- ifelse(as.numeric(ssTest()$`P Value`)<=0.05,'#B9EBDE','#F7D7D7')
      temp1 <- paste0("There is ",pState," significant difference between ",statM, 
                      pState2," groups.")
      temp2 <- paste0("padding:5px; background-color:",pStyle,
                      "; border: 1px solid; border-radius:5px;")
      temp3 <- ifelse(as.numeric(ssTest()$`P Value`)<=0.05,'yes','no')
      npRes <- data.frame(temp1,temp2, temp3)
    }
    
    return(npRes)
  })
  sigRepContent <- reactive({
    titles <- sigRepTit()
    npResult <- lapply(1:nrow(npResult()),function(i){
      p(npResult()[i,1],style = npResult()[i,2])
    })
    if (input$paratestType == 'para'){
      tagList(
        p("Levene's test report for equal variance", style = "font-weight:bolder;"),
        tableOutput('leveVarTab'),
        
        if (input$ttestType == 'tSt'){
          tagList(
            p(titles[1], style = "font-weight:bolder;"),
            tableOutput('tsTestTab')
          )
        } 
        else if (input$ttestType == 'sSt'){
          
          tagList(
            p(titles[2], style = "font-weight:bolder;"),
            tableOutput('ssTestTab'),
            npResult,
            br(),
            p(titles[5],style='font-weight:bolder'),
            tableOutput('phTestTab')
          )
        }
      )
    } else if (input$paratestType == 'nonpara'){
      
      
      if (input$ttestType == 'tSt'){
        # Two-sampled t Test report display
        tagList(
          p(titles[3], style = "font-weight:bolder;"),
          tableOutput('tsTestTab')
        )
      } else {
        #Several sampled Test report display
        if (isTRUE(input$askComp)){
          tagList(
            p(titles[4], style = "font-weight:bolder;"),
            tableOutput('ssTestTab'),
            npResult,
            br(),
            p(titles[5], style = "font-weight:bolder;"),
            tableOutput('phTestTab')
          )
        } else{
          tagList(
            p(titles[4], style = "font-weight:bolder;"),
            tableOutput('ssTestTab'),
            npResult
          )
        }
      }
    }
  })
  
  ## Significance Table Outputs
  output$descStatTab <- renderTable({
    df <- descStat()
    if (input$ttestType == 'tSt'){
      df <- df |> filter(Condition %in% input$statTwoCol)
    }else {
      df <- df
    }
    return(df)
  }, striped = T, width = '100%', align = 'l')|> bindEvent(input$runAnalysisFinal)
  
  output$leveVarTab <- renderTable({
    return(levTest())
  }, striped = T, width = '100%', align = 'l') |> bindEvent(input$runAnalysisFinal)
  
  output$ssTestTab <- renderTable({
    return(ssTest())
  }, striped = T, width = '100%', align = 'l')|> bindEvent(input$runAnalysisFinal)
  
  output$tsTestTab <- renderTable({
    return(tsTest())
  }, striped = T, width = '100%', align = 'l') |> bindEvent(input$runAnalysisFinal)
  
  output$phTestTab <- renderTable({
    if(isTRUE(input$dataGroup)){
      return(do.call(rbind,phTestG()))
    } else{
      return(phTest())
    }
    
  }, striped = T, width = '100%', align = 'l') |> bindEvent(input$runAnalysisFinal)
  
  output$StatAccordion <- renderUI({
    
    accordion( multiple = T, class = 'statAcc', open = T,
               p(testSumm(), style = "font-weight:bolder; padding: 10px;
          border-radius:5px; border:2px solid; font-size:15px;"),
               accordion_panel(
                 title = "Statistical Significance Test",
                 uiOutput('compSigReport')
               ),
               accordion_panel(
                 title = "Normality Test",
                 p("Shapiro-Wilk normality test report", style = 'font-weight:bolder;'),
                 tableOutput('normStatTab'),
                 actionButton('showQQ', 'Show QQ Plot')
               ),
               accordion_panel(
                 title = "Descriptive Statistics",
                 tableOutput('descStatTab')
               )
    )
  })|> bindEvent(input$runAnalysisFinal)
  
  output$compSigReport <- renderUI({
    sigRepContent()
  })|> bindEvent(input$runAnalysisFinal)
  
  observeEvent(input$runAnalysisFinal,{
    if (input$ttestType=='sSt'){
      if(isTRUE(input$dataGroup)){
        grps <- do.call(rbind,phTestG())
      }else{
        grps <- phTest()
      }
    }else{
      grps <- tsTest()
    }
    output$statGroups <- renderUI({
      if(isFALSE(input$dataGroup)){ # Temporary condition; will fix when group annotation is coded
        accordion_panel(
          title= "Customize Plot Annotations",
          
          tagList(
            pickerInput('grplist',
                        'Select Groups',
                        choices = grps[,1],
                        multiple = T,
                        selected = grps[1,1]),
            actionButton('addBrackets','Add Brackets to Plot', width='100%',
                         icon = icon('bars-staggered'), class = 'btn-primary'),
            radioGroupButtons(
              'askTipType',
              'Bracket Type',
              choices = c('Line'='line',
                          'Short Bracket'='short',
                          'Long Bracket'='long'),
              selected = 'short',
              size = 'sm'
            ),
            radioGroupButtons(
              'askPvalType',
              'Significance Report',
              choices = c('Raw P Value'='raw',
                          'Asterisks'='star'),
              selected = 'star',
              size = 'sm'
            ),
            radioGroupButtons(
              'askPvalStyle',
              'P Value Style',
              choices = c('Default'='default',
                          'APA'='apa',
                          'NEJM'= 'nejm'),
              selected = 'default',
              size = 'sm'
            ),
            div(id='sliderstyle',
                noUiSliderInput(
                  'pvalSize',
                  label = 'P Value Text Size',
                  min = 15, max = 45, value = 25,
                  tooltips = TRUE, step = 1, height = "10px"
                )),
            div(style='display:inline-flex; width:100%; flex-direction:row; align-items:flex-start; justify-content:space-between;',
                radioGroupButtons(
                  'pvalHpos',
                  label='',
                  choices = starHicon,
                  selected = 'center',
                  size = 'sm'
                ),br(),
                radioGroupButtons(
                  'pvalVpos',
                  label='',
                  choices = starVicon,
                  selected = 'top',
                  size = 'sm'
                )
            ),
            div(id='sliderstyle',
                noUiSliderInput(
                  'firstBrack',
                  label = 'Vertical Positioning',
                  min = 20, max = 100,
                  value = 80, tooltips=TRUE,
                  step=1, height="10px")),
            div(id='sliderstyle',
                noUiSliderInput(
                  'distWidth',
                  label = 'Inter-bracket Distance',
                  min = 0, max = 100,
                  value = 70, tooltips=TRUE,
                  step=1, height="10px")),
            
            div(id='sliderstyle',
                noUiSliderInput(
                  'topMargin',
                  label = 'Space Around Brackets',
                  min = 0, max = 100,
                  value = 25, tooltips=TRUE,
                  step=1, height="10px")),
            conditionalPanel(
              condition = "input.askTipType=='short'",
              div(id='sliderstyle',
                  noUiSliderInput(
                    'tipLength',
                    label = 'Tip Length',
                    min = 10, max = 100,
                    value = 40, tooltips=TRUE,
                    step=1, height="10px"))),
            div(id='sliderstyle',
                noUiSliderInput(
                  'gapWidth',
                  label = 'Gap Distance',
                  min = 10, max = 100,
                  value = 30, tooltips=TRUE,
                  step=1, height="10px")),
            br()
          )
        )
      }  
      
    })
    output$statDnld <- renderUI({
      tagList(
        downloadButton(
          'statReport',
          'Download Stat Report',
          icon= icon('file-arrow-down'),
          class = "btn-primary"
        ),
        p("Add significance annotations to the plot from 'Graph' tab.",
          style="color:#999;")
      )
      
    })
  })
  
  ### Significance brackets processing ###
  pvalHalign <- reactive({
    if(input$pvalHpos=='left'){
      h <- 0
    }else if (input$pvalHpos=='right'){
      h <- 1
    } else{
      h <- 0.5
    }
  })
  
  ## Offset: dodgeWidth*((var-in-grp-0.5)/totGrp - 0.5)
  statBrackets <- reactive({
    sepV <- ifelse(isTRUE(input$dataGroup),' - ', '-')
    groups <- as.data.frame(str_split(input$grplist,sepV))
    groups <- as.data.frame(t(groups))
    rownames(groups) <- NULL
    colnames(groups) <- c('left', 'right')
    
    if (isTRUE(input$reverseX)){
      lv <- rev(current_colnames())
    } else {
      lv <- current_colnames()
    }
    left <- groups$left |> factor(levels = lv ) 
    right <- groups$right |> factor(levels = lv) 
    
    first <- input$firstBrack/1000
    dist <- 1.0+input$distWidth/1000
    tipL <- 0.02
    gap <- input$gapWidth/1000
    
    if (isFALSE(input$askJitter)){
      if(input$askPlotTypeII=="bar"){
        sem <- function(x){
          x <- na.omit(x)
          sd(x) / sqrt(length(x))
        }
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
            outl <- boxplot.stats(y)$out
            temp <- y[!unlist(y) %in% outl]
            nMax <- rbind(nMax,max(temp))
          }
          ci <- nMax |> t() |> as.data.frame()
          # out
          # iqr <- lapply(x,IQR) |> as.data.frame()
          # qnt75 <- sapply(x, quantile)[4,] |> as.data.frame() |> t() |> as.data.frame()
          # ci <- qnt75+(1.5*iqr)
          rownames(ci) <- NULL
          colnames(ci) <- colnames(data())
          return(ci)
        }
        if (input$barFunc == 'mean'){
          if (input$sum_typeBarMean=='mean_only'){
            mx <- apply(data(), MARGIN=2, FUN=mean) |> data.frame() |> t() 
          } else if (input$sum_typeBarMean=='mean_sd'){
            mx <- (apply(data(), MARGIN=2, FUN=sd) + apply(data(), MARGIN=2, FUN=mean)) |> data.frame() |> t()
          } else{
            mx <- (sapply(data(), sem)+apply(data(), MARGIN=2, FUN=mean)) |> data.frame() |> t()
          }
        } else {
          if (input$sum_typeBarMedian=='median_only'){
            mx <- apply(data(), MARGIN=2, FUN=median) |> data.frame() |> t()
            
          } else{
            # mx <- (ci95(data())*(1.5/sqrt(length(data())))+apply(data(), MARGIN=2, FUN=median))  
            mx <- ci95(data())
          }
        }
      }else{
        mx <- apply(data(), MARGIN=2, FUN=max) |> data.frame() |> t()
      }
    }else{
      mx <- apply(data(), MARGIN=2, FUN=max) |> data.frame() |> t()
    }
    
    
    row.names(mx)<-NULL
    mx <- data.frame(mx)
    colnames(mx) <- colnames(df)
    if (isTRUE(input$reverseX)){
      mx <- rev(mx)
    }else{
      mx <- mx
    }
    mxx <- yaxisMax()
    
    
    statup <- data.frame(x = as.numeric(left), xend = as.numeric(right), y = (mxx+(mxx*first)), yend = (mxx+(mxx*first)))
    #To order the lowest x on left side always
    statup <- statup |> mutate(x_min=pmin(x,xend),x_max=pmax(x,xend)) |>
      select(-x,-xend) |> rename(x = x_min, xend = x_max) |> select(x, xend,everything())
    statup$diff <- statup$xend-statup$x
    statup <- statup  |> 
      group_by(x) |> 
      mutate(
        new_x = dist^((diff) - 1),
        y = y * new_x,
        yend = yend * new_x
      ) |> select(-new_x) |> 
      ungroup()  |> 
      group_by(xend)  |> 
      mutate(
        new_xend = dist^((diff) - 1),
        y = y * new_xend,
        yend = yend * new_xend
      ) |> select(-new_xend) |>
      ungroup()
    
    sL <- t(mx[statup$x])+(t(mx[statup$x])*tipL)
    rownames(sL) <- NULL
    colnames(sL) <- 'yendL'
    
    sR <- t(mx[statup$xend])+(t(mx[statup$xend])*tipL)
    rownames(sR) <- NULL
    colnames(sR) <- 'yendR'
    
    statup <- cbind(statup,sL,sR)
    
    
    for (i in 1:nrow(statup)){
      for (j in 1: nrow(statup)){
        if (statup[i,1]>statup[j,1]&&statup[i,1]<statup[j,2]){
          if(statup[i,3]==statup[j,3] && statup[i,5]==statup[j,5]){
            statup[i,3] <- statup[i,3]*dist
            statup[i,4] <- statup[i,4]*dist
          }
        }
        if (statup[i,2]>statup[j,1]&&statup[i,2]<statup[j,2]){
          if(statup[i,4]==statup[j,4]&& statup[i,5]==statup[j,5]){
            statup[i,3] <- statup[i,3]*dist
            statup[i,4] <- statup[i,4]*dist
          }
        }
      }
    }
    #Labels
    if (input$pvalVpos=='top'){
      yT <- statup$y+statup$y*0.03 |> as.numeric()
    } else {
      yT <- statup$y-statup$y*0.03 |> as.numeric()
    }
    if (input$pvalHpos=='left'){
      if (isTRUE(input$flipPlot)){
        xT <- as.numeric(right)-0.05
      } else{
        xT <- as.numeric(left)+0.05
      }
      
    } else if (input$pvalHpos=='right'){
      if (isTRUE(input$flipPlot)){
        xT <- as.numeric(left)+0.05
      } else {
        xT <- as.numeric(right)-0.05
      }
      
    } else {
      xT <- apply(data.frame(as.numeric(left),as.numeric(right)), MARGIN=1,FUN=mean) |> as.numeric()
    }
    
    # P value processing
    star4 <- paste0(rep('*', 4), collapse = "")
    star3 <- paste0(rep('*', 3), collapse = "")
    
    
    results_list <- list()
    
    for (i in 1:length(input$grplist)) {
      current_tab <- if (input$ttestType == 'tSt') tsTest() else phTest()
      n <- match(input$grplist[i], current_tab[, 1])
      
      if (!is.na(n)) {
        t_val <- ""
        
        # P value asterisk
        if (input$askPvalType == 'star') {
          t_val <- as.character(current_tab[n, ncol(current_tab)])
          
          # For APA and NEJM styles, cap at 3 stars
          if (input$askPvalStyle != 'default') {
            if (t_val == star4) {
              t_val <- star3
            }
          }
          
        } else {# Raw P value
          p_raw <- as.numeric(current_tab[n, (ncol(current_tab) - 1)])
          
          # Zero before point (APA has none, NEJM/Default has)
          s_prefix <- if (input$askPvalStyle == 'apa') "" else "0"
          ns_p <- if (p_raw>=1) p_raw else gsub('0.','.',p_raw)
          
          if (input$askPvalStyle == 'default') {
            t_val <- paste0('*P* = ', p_raw)
          } else {
            # APA and NEJM
            if (p_raw <= 0.001) {
              t_val <- paste0("*p* < ", s_prefix, ".001")
            } else if (p_raw <= 0.01) {
              t_val <- paste0("*p* < ", s_prefix, ".01")
            } else if (p_raw <= 0.05) {
              t_val <- paste0("*p* < ", s_prefix, ".05")
            } else {
              t_val <- paste0("*p* = ", s_prefix, ns_p)
            }
          }
        }
        results_list[[i]] <- data.frame(text = paste0('<b>',t_val,'</b>'),
                                        size = ifelse(t_val=='ns' || isTRUE(str_detect(t_val,'P')) || isTRUE(str_detect(t_val, 'p')),input$pvalSize/5, input$pvalSize/2.5),
                                        vjust = ifelse(t_val=='ns' || isTRUE(str_detect(t_val,'P')) || isTRUE(str_detect(t_val, 'p')), 0.5, 0.75),stringsAsFactors = FALSE)
        
      }
    }
    
    # Combine all results into the final dataframe
    text <- do.call(rbind, results_list)
    
    colnames(text) <- c('text','size','vjust')
    statup <- cbind(statup,xT,yT,text)
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
    
    statup <- statup |> group_by(x) |>
      arrange(y) |>
      mutate(yendL=endP(y=y,diff=diff,list=yendL)) |>
      ungroup() |> 
      group_by(xend) |> 
      arrange(yend) |> 
      mutate(yendR=endP(y=y,diff=diff,list=yendR)) |> 
      ungroup()
    
    for (i in 1:nrow(statup)){
      for (j in 1: nrow(statup)){
        if (statup[i,1]>statup[j,1]&&statup[i,1]<statup[j,2]){
          if(statup[i,3]>statup[j,3] && statup[i,5]==statup[j,5]){
            statup[i,6] <- statup[j,3]+statup[j,3]*tipL
          }
        }
        if (statup[i,2]>statup[j,1]&&statup[i,2]<statup[j,2]){
          if(statup[i,3]>statup[j,3]&& statup[i,5]==statup[j,5]){
            statup[i,7] <- statup[j,4]+statup[j,4]*tipL
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
      tipS <- input$tipLength/1000
      statup <- statup |> mutate(yendL=y-(y*tipS)) |> mutate(yendR=yend-(yend*tipS))
      return(statup)
    } else{
      statup <- statup |> mutate(yendL=y) |> mutate(yendR=yend)
      return(statup)
    }
  })
  # output$test <- renderTable({
  #   req(input$grplist)
  #   statBrackets()
  # })
  plotTopM <- reactive({
    if(isFALSE(input$flipPlot)){
      if (!isTruthy(input$runAnalysisFinal)) {
        return(30)
      }
      df <- tryCatch(segAdd(), error = function(e) NULL)
      
      if (is.null(df) || nrow(df) == 0) {
        return(30) # Default margin
      } else {
        layers <- length(unique(df$y))+1
        return((input$topMargin + layers * 25))
      }
    }else{
      return(30)
    }
  })
  plotRightM <- reactive({
    if(isTRUE(input$flipPlot)){
      if (!isTruthy(input$runAnalysisFinal)) {
        return(30)
      }
      df <- tryCatch(segAdd(), error = function(e) NULL)
      
      if (is.null(df) || nrow(df) == 0) {
        return(30) # Default margin
      } else {
        layers <- length(unique(df$y))+1
        return((20 + layers * 25))
      }
    }else{
      return(30)
    }
  })
  btn_val <- reactiveVal(FALSE)
  observeEvent(input$addBrackets,{
    btn_val(!btn_val())
    if(btn_val()==FALSE){
      updateActionButton(session,'addBrackets','Add Brackets to Plot')
    }else {
      updateActionButton(session,'addBrackets','Remove Brackets')
    }
  })
  
  segAdd <- reactive({
    req(statBrackets())
    if(isFALSE(btn_val())){
      return(statBrackets()*0)
    }else{
      return(statBrackets())
    }
  })
  
  
  # Conditional content for Stat panel
  output$stat_main_content <- renderUI({
    if (!isTruthy(input$submitFile) && !isTruthy(input$pasteBtn)) {
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
    }  else {
      uiOutput('StatAccordion')
    }
  })
  
  ## Workbook preparation for statistical analysis report for downloading
  
  StatReport <- reactive({
    wb <- openxlsx::createWorkbook()
    sheetName <- paste("Stat Report_", input$sheetlist, collapse = '')
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
      return(paste(x, collapse = "&"))
    } else {
      return(as.character(x))
    }
  }
  
  savesetting_df <- reactive({
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
        
        # --- Graph Settings: Box-Jitter ---
        "Change plot subtype (Box)",
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
        "Change plot subtype (Violin)",
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
        
        # --- Graph Settings: Bar ---
        "Box Width (Bar)",
        "Line Width (Bar)",
        "Bar Display",
        "Summary Statistics (Bar Mean)",
        "Summary Statistics (Bar Median)",
        "Stat Summ Linewidth (Bar)",
        "Stat Summ Bar Width (Bar)",
        "Errorbar Direction",
        "Add Datapoints (Bar)",
        
        # --- Graph Settings: Connecting Lines ---
        "Add Connecting Line",
        "Line Width (Connect)",
        "Line Colour (Connect)",
        "Line Type (Connect)",
        
        # --- Plot Area ---
        "Add Grids",
        "Major Grids",
        "Minor Grids",
        "Choose Axis (Grid)",
        "Grids Colour",
        "Add Borders",
        "Add Background Colour",
        "Plot Background Color",
        "Legend Title",
        "Legend Position",
        "Legend Text Size",
        "Legend Title Size",
        "Legend Key Size",
        "Legend Border Size",
        "Datapoints Count View",
        
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
        "Gradient Colour 1",
        "Gradient Colour 2",
        "Select Theme Generator (Grouped)",
        "Display Contrast",
        "Border Colour",
        "Border Shade",
        "Shape Opacity",
        "Datapoint Outline Colour",
        "Datapoint Fill Colour",
        "DP Gradient Colour 1",
        "DP Gradient Colour 2",
        "DP Fill Gradient Colour 1",
        "DP Fill Gradient Colour 2",
        "Datapoint Outline Colour (Grouped)",
        "Datapoint Fill Colour (Grouped)",
        
        # --- Statistics ---
        "Select Test Type (T/S)",
        "Select Test Type (Para/NonPara)",
        "Paired Samples",
        "Perform Multiple Comparisons",
        "Repeated-Measured Samples",
        "Select Groups (Control Comp)",
        "Select Group Comparison Type",
        "Select Groups (Group Comp)",
        "Correction Method (General)",
        "Correction Method (Paired)",
        "Correction Method (Grouped)",
        "Select Control",
        "Correction Method (Control)",
        "Correction Method (Control Grouped)",
        "Correction Method (Paired Control)",
        "Correction Method (Paired Control Grouped)",
        "Select Groups (Two Sample)",
        "Select Groups (Annotation)",
        "Bracket Type",
        "Significance Report Type",
        "P Value Style",
        "P Value Text Size",
        "P Value Horizontal Pos",
        "P Value Vertical Pos",
        "P Value Area Margin",
        "Vertical Positioning (Bracket)",
        "Inter-bracket Distance",
        "Tip Length",
        "Gap Distance (Bracket)",
        
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
        
        # --- Graph Settings: Box-Jitter ---
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
        
        # --- Graph Settings: Bar ---
        "barwidth",
        "linewidthBar",
        "barFunc",
        "sum_typeBarMean",
        "sum_typeBarMedian",
        "statLineBar",
        "statWidthBar",
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
        "shadevalue",
        "shapeAlpha",
        "dpcolor",
        "dpfill",
        "dpgrad1",
        "dpgrad2",
        "dpgradF1",
        "dpgradF2",
        "dpcolorG",
        "dpfillG",
        
        # --- Statistics ---
        "ttestType",
        "paratestType",
        "askPaired",
        "askComp",
        "askPairedssT",
        "statContCols",
        "compList",
        "statCols",
        "askCorrection",
        "askCorrectionP",
        "askCorrectionG",
        "askControl",
        "askCorrectionC",
        "askCorrectionCG",
        "askCorrectionPC",
        "askCorrectionPCG",
        "statTwoCol",
        "grplist",
        "askTipType",
        "askPvalType",
        "askPvalStyle",
        "pvalSize",
        "pvalHpos",
        "pvalVpos",
        "topMargin",
        "firstBrack",
        "distWidth",
        "tipLength",
        "gapWidth",
        
        # --- Export/Size ---
        "width",
        "height",
        "lockRatio",
        "selectFileType",
        "selectDPI"
      ),
      
      Data = sapply(list(
        # --- File & Data Settings ---
        # input$sheetlist,
        input$dataGroup,
        input$grpSwitch,
        # input$selectedCols,
        input$askPlotType,
        input$askPlotTypeG,
        input$askPlotTypeIIG,
        input$askPlotTypeII,
        
        # --- Graph Settings: Box-Jitter ---
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
        
        # --- Graph Settings: Violin ---
        input$viotype,
        input$linewidthVio,
        input$boxWidthVio,
        input$boxColVio,
        input$boxColCust,
        input$askQuantLine,
        input$quantLineSize,
        input$endTrim,
        input$innerDistVio,
        
        # --- Graph Settings: Jitter ---
        input$scatter,
        input$pointsize,
        input$pointDist,
        input$pointshape,
        input$pointMethod,
        input$sum_typeJitter,
        input$statLine,
        input$statWidth,
        
        # --- Graph Settings: Raincloud ---
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
        
        # --- Graph Settings: Bar ---
        input$barwidth,
        input$linewidthBar,
        input$barFunc,
        input$sum_typeBarMean,
        input$sum_typeBarMedian,
        input$statLineBar,
        input$statWidthBar,
        input$askSide,
        input$askJitter,
        
        # --- Graph Settings: Connecting Lines ---
        input$askConnectLine,
        input$connectLineSize,
        input$connectLineCol,
        input$connectLineType,
        
        # --- Plot Area ---
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
        
        # --- Plot Title ---
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
        
        # --- Axes ---
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
        
        # --- Fonts ---
        input$font,
        input$plotFont,
        input$Xfontcol,
        input$Xfontsz,
        input$Xlinebreak,
        input$Yfontcol,
        input$Yfontsz,
        input$Ylinebreak,
        
        # --- Theme ---
        input$choosetheme,
        input$boxtheme,
        input$grad1,
        input$grad2,
        input$choosethemeII,
        input$grayscale,
        input$boxbordercol,
        input$shadevalue,
        input$shapeAlpha,
        input$dpcolor,
        input$dpfill,
        input$dpgrad1,
        input$dpgrad2,
        input$dpgradF1,
        input$dpgradF2,
        input$dpcolorG,
        input$dpfillG,
        
        # --- Statistics ---
        input$ttestType,
        input$paratestType,
        input$askPaired,
        input$askComp,
        input$askPairedssT,
        input$statContCols,
        input$compList,
        input$statCols,
        input$askCorrection,
        input$askCorrectionP,
        input$askCorrectionG,
        input$askControl,
        input$askCorrectionC,
        input$askCorrectionCG,
        input$askCorrectionPC,
        input$askCorrectionPCG,
        input$statTwoCol,
        input$grplist,
        input$askTipType,
        input$askPvalType,
        input$askPvalStyle,
        input$pvalSize,
        input$pvalHpos,
        input$pvalVpos,
        input$topMargin,
        input$firstBrack,
        input$distWidth,
        input$tipLength,
        input$gapWidth,
        
        # --- Export/Size ---
        input$width,
        input$height,
        input$lockRatio,
        input$selectFileType,
        input$selectDPI
      ), flattenID), stringsAsFactors = F
    )
    
    ## For dynamically generated inputs
    # Point shape Box
    palSID <- paste("pointshapeBox", seq_along(colorCount()),sep = '_')
    palSVal <- sapply(palSID, function(id){
      val <- input[[id]]
      if (is.null(val)) return(21)
      return(flattenID(val))
    })
    palShape <- data.frame(
      Parameter = c(rep("Point Shape Box",length(colorCount()))),
      inputID = palSID,
      Data = as.character(palSVal) , stringsAsFactors = F
    )
    settings_df <- rbind(settings_df,palShape)
    # Point shape Rain
    palSID <- paste("pointshapeRain", seq_along(colorCount()),sep = '_')
    palSVal <- sapply(palSID, function(id){
      val <- input[[id]]
      if (is.null(val)) return(21)
      return(flattenID(val))
    })
    palShape <- data.frame(
      Parameter = c(rep("Point Shape Raincloud",length(colorCount()))),
      inputID = palSID,
      Data = as.character(palSVal) , stringsAsFactors = F
    )
    settings_df <- rbind(settings_df,palShape)
    # Point shape Jitter
    palSID <- paste("pointshape", seq_along(colorCount()),sep = '_')
    palSVal <- sapply(palSID, function(id){
      val <- input[[id]]
      if (is.null(val)) return(21)
      return(flattenID(val))
    })
    palShape <- data.frame(
      Parameter = c(rep("Point Shape Jitter",length(colorCount()))),
      inputID = palSID,
      Data = as.character(palSVal) , stringsAsFactors = F
    )
    settings_df <- rbind(settings_df,palShape)
    # Palette themes
    palSID <- paste("colors", seq_along(colorCount()),sep = '_')
    palSVal <- sapply(palSID, function(id){
      val <- input[[id]]
      if (is.null(val)) return('#CCCCCC')
      return(flattenID(val))
    })
    palShape <- data.frame(
      Parameter = c(rep("Shape Palette",length(colorCount()))),
      inputID = palSID,
      Data = as.character(palSVal) , stringsAsFactors = F
    )
    settings_df <- rbind(settings_df,palShape)
    # Data point Palette color themes
    palSID <- paste("colorsdp", seq_along(colorCount()),sep = '_')
    palSVal <- sapply(palSID, function(id){
      val <- input[[id]]
      if (is.null(val)) return('#CCCCCC')
      return(flattenID(val))
    })
    palShape <- data.frame(
      Parameter = c(rep("DP Shape Color Palette",length(colorCount()))),
      inputID = palSID,
      Data = as.character(palSVal) , stringsAsFactors = F
    )
    settings_df <- rbind(settings_df,palShape)
    # Data point Palette fill themes
    palSID <- paste("fillsdp", seq_along(colorCount()),sep = '_')
    palSVal <- sapply(palSID, function(id){
      val <- input[[id]]
      if (is.null(val)) return('#CCCCCC')
      return(flattenID(val))
    })
    palShape <- data.frame(
      Parameter = c(rep("DP Shape Fill Palette",length(colorCount()))),
      inputID = palSID,
      Data = as.character(palSVal) , stringsAsFactors = F
    )
    settings_df <- rbind(settings_df,palShape)
    return(settings_df)
  })
  
  ### Download handler for saving Settings File
  output$savesetting <- downloadHandler(
    filename = function() { paste0("SENsabled_Settings_", Sys.Date(), ".xlsx") },
    content = function(file) {
      openxlsx::write.xlsx(savesetting_df(), file= file, asTable = T)
    }
  )
  
  ### Processing for the setting file data to update all the inputs ###
  observeEvent(input$reuseset, {
    file <- input$usesetting
    ext <- tools::file_ext(file$datapath)
    req(data())
    req(file)
    validate(need(ext == "xlsx", "Please upload a xlsx file"))
    uploaded_inputs <- openxlsx::read.xlsx(file$datapath, sheet = 1)
    uploaded_inputs <- data.frame(uploaded_inputs, stringsAsFactors = FALSE)
    
    if (isTRUE(identical(uploaded_inputs[,2],savesetting_df()[,2])) &&
        sum(grepl("^colors_",uploaded_inputs[,2])) == length(current_colnames())){  
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
        
        try({
          # Text / Numeric
          updateTextInput(session, id, value = value)
          if (is.numeric(value) || !is.na(as.numeric(value))) {
            updateNumericInputIcon(session, id, value = as.numeric(value))
          }
          # Select / Picker
          updateSelectInput(session, id, selected = value)
          updatePickerInput(session, id, selected = value)
          
          # NoUiSlider (numeric)
          if (!is.na(as.numeric(value))) {
            updateNoUiSliderInput(session, id, value = as.numeric(value))
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
      
      showNotification("All inputs updated from file.", type = "message")
      
    }else {
      showNotification("Please upload correct settings file. 
                       Make sure there were no changes made in the file and the column numbers are equal.", 
                       type = "error")
    }
  }, priority = 100)
  
  ##End of Server##
})


shinyApp(ui = ui, server = server)
