


### Source Code for SEN'sabale Plotting App ###
### Author: Sumit Sen ###
### TIFR, Mumbai ###


## Running the app in an interactive session ##

##Loading Necessary Packages##

if (interactive()) {
  pkg <-
    #For running the app
    c(
      'shiny',
      'shinythemes',
      'shinyjs',
      'shinyBS',
      'shinycssloaders',
      'rJava',
      #For handling the data
      'openxlsx',
      'datasets',
      'tidyr',
      'dplyr',
      'DT',
      'tidyverse',
      'remotes',
      'magrittr',
      #For creating the plot
      'ggplot2',
      'ggbeeswarm',
      #For beautifying the plot
      'extrafont',
      'colorspace',
      'colourpicker',
      'bsplus',
      'DescTools',
      #For saving the data
      'rclipboard',
      #For Stats
      'FSA',
      'dunn.test',
      'broom',
      'asht',
      'car',
      #For making stat report
      'xlsx'
    )
  
  for (i in pkg) {
    print(i)
    if (require(i, character.only = TRUE)) {
      print(paste(i, "is loaded correctly"))
    } else {
      print(paste("trying to install", i))
      install.packages(i)
      if (require(i, character.only = TRUE)) {
        print(paste(i, "installed and loaded"))
      } else {
        stop(paste("could not install", i))
      }
    }
  }
  
  
  ##Copy Plot to clipboard code was written by Stephane Laurent
  ##from Stackexchange and modified accordingly
  js <- '
'
jsV <- '
'
}
#Tab Panel enable/disable script was taken from bretauv
##of Stackexchange and modified accordingly
css <- "
.nav li a.disabled, #savesettingV.disabled, #savesetting.disabled, #compTabInp.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}
.nav li:hover{
border-bottom:1px solid #333;
}
#modalout .checkbox label{
color:#333333!important;
}
.modal-body{
overflow-x:scroll;
}
.suggested {
        color: #87ce0a;
        font-weight: bold;
        border:2px solid #87ce0a;
        background-color: #fffbcf;
        padding:3px;
        border-radius:10px;
        width:50px;
}
#reportdnld, #reportdnldV{
      background-color:#bbd5b0;
      border:none;
}
#reportdnld:hover, #reportdnldV:hover{
      background-color:#cddcc7;
      color:#444;
}

.checkbox {
      font-weight:800;
}
/* CSS modification of the Shiny Notification to the top right corner from default position*/
.shiny-notification {
          position:fixed;
          top: calc(2%);
          right: calc(1%);
        }
# CSS fix for overflowing DataTable in modal box
.modal-lg div{
  overflow-y:scroll;
}

#themeToggle,
.visually-hidden {
  dislay:block;
  position: absolute;
  width: 1px;
  height: 1px;
  clip: rect(0 0 0 0);
  clip: rect(0, 0, 0, 0);
  overflow: hidden;
  padding:3px;
},
#themeToggle + span .fa-sun {
font-size: 16pt;
padding:3px;
}

"
#######################################
####### The UI Code Starts Here #######
#######################################

options(shiny.maxRequestSize = 250 * 1024^2) #Max file size to be uploaded is 250mb
ui <- shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(css),
    # shinyjs::runjs(js),
    # themeSelector(),
    theme = shinythemes::shinytheme("darkly"),
    
    tags$head(tags$style(
      HTML((css)
      )
    )),
    # App Title
    titlePanel("SEN'sable Plotting v2.0"),
    
    ######################
    #Main Navbar sections#
    ######################
    
    tabsetPanel(
      ### File Upload###
      tabPanel(
        "Upload File",
        titlePanel("Uploading Files"),
        sidebarLayout(
          sidebarPanel(
            #Uploading the file
            fileInput('file1', 'Choose XLSX File', accept = c('.xlsx')),
            
            #Rendering Selection of Plot type and Submit Button
            uiOutput('fileupload'),
            
            #Choosing the header type
            checkboxInput(
              'header',
              'Header (select this if the first row is the column title)',
              TRUE
            ),
            
            #Choosing the sheet from the xlsx file
            uiOutput('sheetnames'),
            uiOutput("colnames"),
            checkboxInput(
              inputId = "themeToggle",
              label = tagList(
                tags$span('Change Display Mode'),
                tags$span(class = "visually-hidden", "toggle theme"),
                tags$span(class = "fa fa-sun", `aria-hidden` = "true")
                
              )
            )
          ),
          mainPanel(
            #Main User-Data Table Output
            dataTableOutput('contents')
          )
          
        ),
        tags$script(
          "// Define css theme filepaths
        const themes = {
            dark: 'shinythemes/css/darkly.min.css',
            light: 'shinythemes/css/flatly.min.css'
        };

        // Function that creates a new link element
        function newLink(theme) {
            let el = document.createElement('link');
            el.setAttribute('rel', 'stylesheet');
            el.setAttribute('type', 'text/css');
            el.setAttribute('href', theme);
            return el;
        }

        // Function that removes <link> of current theme by href
        function removeLink(themeUrl) {
            let el = document.querySelector(`link[href='${themeUrl}']`);
            if (el) {
                el.parentNode.removeChild(el);
            }
        }

        // Define vars
        const darkThemeLink = newLink(themes.dark);
        const lightThemeLink = newLink(themes.light);
        const head = document.getElementsByTagName('head')[0];
        const toggle = document.getElementById('themeToggle');

        // Define extra CSS for dark theme specific elements (e.g., DataTables)
        const extraDarkThemeCSS = '.dataTables_length label select {color: white;} .dataTables_filter label, .dataTables_info {color: white!important;} .paginate_button { background: white!important;} tbody{color:#ccc;} thead { color: white;}';
        const extraDarkThemeStyleElement = document.createElement('style');
        extraDarkThemeStyleElement.appendChild(document.createTextNode(extraDarkThemeCSS));

        // --- Initial Setup (Runs when the script loads) ---
        // Set initial theme to light mode and ensure checkbox state matches.
        // Use a DOMContentLoaded listener to ensure elements are available.
        document.addEventListener('DOMContentLoaded', function() {
            // Ensure the checkbox is unchecked for light mode
            toggle.checked = false;

            // Remove any existing theme links before adding the default
            removeLink(themes.dark);
            removeLink(themes.light); // Remove both to be sure

            // Add the light theme as default
            head.appendChild(lightThemeLink);

            // Ensure extra dark theme styles are NOT present initially
            if (head.contains(extraDarkThemeStyleElement)) {
                head.removeChild(extraDarkThemeStyleElement);
            }
        });

        // --- Event Listener for Toggle ---
        toggle.addEventListener('input', function() {
            if (toggle.checked) { // If checkbox is checked (switch to dark)
                removeLink(themes.light); // Remove light theme
                head.appendChild(darkThemeLink); // Add dark theme
                head.appendChild(extraDarkThemeStyleElement); // Add extra dark theme CSS
            } else { // If checkbox is unchecked (switch to light)
                removeLink(themes.dark); // Remove dark theme
                if (head.contains(extraDarkThemeStyleElement)) {
                    head.removeChild(extraDarkThemeStyleElement); // Remove extra dark theme CSS
                }
                head.appendChild(lightThemeLink); // Add light theme
            }
        });"
        )
      ),
      ### Violin Plot ###
      tabPanel(
        "Violin Plot",
        disabled = TRUE,
        pageWithSidebar(
          div(),
          sidebarPanel(tabsetPanel(
            id = 'violinTab',
            # Tab to manipulate general parameters of the plot
            tabPanel(
              "General Settings",
              br(),
              bsplus::bs_accordion('Vioplot') |>
                bsplus::bs_set_opts(use_head_link =
                              T) |>
                bsplus::bs_append(
                  title = 'Modify Violin Shape',
                  content = list(
                    #Selecting border width of the plot
                    sliderInput(
                      "borderWidthV",
                      "Border Width:",
                      min = 0,
                      max = 2.5,
                      value = 0.8
                    ),
                    #Choosing scatter plot
                    radioButtons(
                      "radio1V",
                      "Add Quartile Boxes to the plot:",
                      choices = c("Yes", "No"),
                      inline =
                        T
                    ),
                    #Choosing the Box Width
                    uiOutput("slider1V"),
                    #Selecting horizontal line for median
                    uiOutput("hLineV")
                  )
                ) |>
                bsplus::bs_append(
                  title = 'Modify Plot Axes',
                  content = list(
                    #Y-axis Title
                    textInput(
                      "aytitleV",
                      label = "Y-axis title",
                      placeholder = "Type in the title here",
                      value = "Y-axis"
                    ),
                    checkboxInput('chksymbolYV', label = "Add Special Characters", value = F),
                    uiOutput('showsymbolYV'),
                    #X-axis Title
                    textInput(
                      "axtitleV",
                      label = "X-axis title",
                      placeholder = "Type in the title here",
                      value = NA
                    ),
                    checkboxInput('chksymbolXV', label =
                                    "Add Special Characters", value = F),
                    uiOutput('showsymbolXV'),
                    
                    #X-axis rotation
                    sliderInput(
                      "XrotateV",
                      label = "X-axis rotation",
                      min = 0,
                      max = 90,
                      value =
                        0
                    ),
                    
                    #Choosing Y-axis transformation
                    radioButtons(
                      "logscaleV",
                      label = "Convert Y-axis in Log scale:",
                      choices = c(
                        "NA" = "none",
                        "Log10" = "log10",
                        "Log2" = "log2"
                      ),
                      inline = T
                    ),
                    #Choosing the Y-axis limits
                    numericInput(
                      'minYV',
                      "Y-axis minimum limit:",
                      value = NA,
                      min = NA,
                      max = NA
                    ),
                    numericInput(
                      'maxYV',
                      'Y-axis maximum limit:',
                      value = NA,
                      min = NA,
                      max = NA
                    ),
                    sliderInput(
                      'axislineV',
                      'Axes linewidth:',
                      value = 1,
                      min = 0.2,
                      max = 2
                    )
                  )
                ) |>
                bsplus::bs_append(
                  title = 'Modify Axes Font',
                  content = list(
                    #Selecting type face
                    selectInput(
                      'fontV',
                      'Select font:',
                      choices = c(
                        'Arial',
                        'Bookman Old Style',
                        'Calibri',
                        'Candara',
                        'Century Gothic',
                        'Corbel',
                        'Garamond',
                        'Georgia',
                        'Lucida Bright',
                        'Lucida Sans',
                        'Microsoft Sans Serif',
                        'Segoe UI',
                        'Tahoma',
                        'Times New Roman',
                        'Verdana'
                      )
                    ),
                    #Selecting font  size
                    sliderInput(
                      "XfontcolV",
                      "Select Font Size for X-axis texts:",
                      min = 5,
                      max = 50,
                      value = 18
                    ),
                    sliderInput(
                      "XfontszV",
                      "Select Font Size for the X-axis title:",
                      min = 5,
                      max = 50,
                      value = 18
                    ),
                    sliderInput(
                      "XlinebreakV",
                      "Adjust Line break of X-axis text",
                      min = 5,
                      max = 40,
                      value = 30
                    ),
                    sliderInput(
                      "YfontcolV",
                      "Select Font Size for Y-axis texts:",
                      min = 5,
                      max = 50,
                      value = 18
                    ),
                    sliderInput(
                      "YfontszV",
                      "Select Font Size for the Y-axis title:",
                      min = 5,
                      max = 50,
                      value = 18
                    ),
                    sliderInput(
                      "YlinebreakV",
                      "Adjust Line break of Y-axis title",
                      min = 5,
                      max = 40,
                      value = 30
                    )
                  )
                ) |>
                bsplus::bs_append(
                  title = 'Modify Plot Theme',
                  content = list(
                    radioButtons(
                      "themeV",
                      label = "Choose plot background theme:",
                      choices = c(
                        "Classical" = 'theme_classic',
                        "Minimal" = 'theme_minimal',
                        "Gray" = 'theme_gray',
                        "Dark" = "theme_dark",
                        "Light" = 'theme_light'
                      ),
                      inline = T,
                      selected = c("theme_classic")
                    ),
                    #Choosing theme generation
                    radioButtons(
                      "choosethemeV",
                      label = "Choose theme generator:",
                      choices = c(
                        "Default" = "default",
                        "Preset Themes" =
                          'preset',
                        "Select a Gradient" =
                          'gradient',
                        "Select from Palette" =
                          'palette'
                      ),
                      selected = c("default")
                    ),
                    uiOutput('themelistV'),
                    uiOutput('colUpdateV'),
                    radioButtons(
                      "viobordercol",
                      label = "Choose box border colour:",
                      choices = c(
                        'Default (Black)' = 'default',
                        "Darker" = 'dark',
                        "Lighter" = 'light'
                      ),
                      selected = c('default')
                    ),
                    uiOutput('shadevalV')
                  )
                ),
              radioButtons(
                "dpviewV",
                "Add datapoints count to your plot:",
                choices = c('No' = 0, 'Yes' =
                              5),
                inline = T
              ),
              #Adding annotation on plot
              tagList(
                uiOutput('grpselectV'),
                uiOutput('askAnnotationV'),
                uiOutput('chooseAnnotationV')
              )
            )
          )),
          mainPanel(
            h2("Visualize your plot here"),
            div(
              div(
                # tableOutput('testModBox'),
                uiOutput("plot_notice2"),
                # uiOutput('themeSubmitV'),
                shinycssloaders::withSpinner(
                  uiOutput("MyVPlotFinal"),
                  type = 5,
                  color = "#9e9e9e",
                  size = 2
                ),
                br()
              ),
              div(
                style = "display:inline-block",
                numericInput(
                  'widthV',
                  label = "Select Width of your plot",
                  min = 100,
                  max = 800,
                  value = 500
                )
              ),
              div(
                style = "display:inline-block",
                numericInput(
                  'heightV',
                  label = "Select Height of your plot",
                  min = 100,
                  max = 800,
                  value = 400
                )
              ),
              div(
                style = "display:inline-flex;
                          justify-content:space-between;
                          height:45px;
                          width:80%;
                          margin:auto;",
                actionButton(
                  "copybtn",
                  "Copy Plot",
                  icon = icon("copy"),
                  class = "btn-primary",
                  title = "Click to copy on clickboard"
                ),
                downloadButton(
                  "downloadVPlot",
                  "Download Plot",
                  icon = shiny::icon("download"),
                  title = "Click to download your plot"
                ),
                #Save Settings button
                downloadButton(
                  "savesettingV",
                  "Save settings",
                  icon = icon("gear"),
                  class = "btn-primary",
                  title = "Click to save your plot settings"
                ),
                div(
                  fileInput(
                    "usesettingV",
                    label = NULL,
                    buttonLabel = "Select Settings File",
                    accept = c(".csv")
                  ),
                  title = "Click to load your saved csv file to reuse previous setting"
                ),
                actionButton("reusesetV", "Upload")
              )
            ),
            style = "position:fixed;
                        display:flex;
                        align-content:center;
                        flex-direction:column;
                        right: 0;
                        width:65%;
                        height:650px;
                        overflow-y:scroll;
                        z-index:999;"
          )
        )
      ),
      ### Box-Jitter Plot ###
      tabPanel("Box Plot", pageWithSidebar(
        div(),
        sidebarPanel(tabsetPanel(
          id = 'boxTab',
          # Tab to manipulate general parameters of the plot
          tabPanel(
            "General Settings",
            br(),
            bsplus::bs_accordion('Boxplot') |>
              bsplus::bs_set_opts(use_heading_link =
                            TRUE) |>
              bsplus::bs_append(
                title = 'Modify Box shape',
                content = list(
                  #Choosing box-width
                  sliderInput(
                    'boxwidth',
                    label =
                      'Select box width',
                    min = 0,
                    max = 1,
                    value = 0.6
                  ),
                  #Choosing to add notch or not
                  div(
                    checkboxInput("notch", label = "Add Notches", value = TRUE),
                    style = 'font-weight:800;'
                  ),
                  #Choosing to add Outliers or not
                  radioButtons(
                    "outlier",
                    label = "Mark Outliers",
                    choices = c("No" = NA, "Yes" = 5),
                    inline = T
                  ),
                  #Controlling box outline width
                  sliderInput(
                    'linewidth',
                    label = 'Select line width of the boxes',
                    min = 0,
                    max = 2.5,
                    value = 1.2
                  ),
                  #Choosing scatter plot
                  radioButtons(
                    "radio1",
                    "Add Datapoints to the plot:",
                    choices = c("Yes", "No"),
                    inline =
                      T
                  ),
                  #Choosing the Scatter Width
                  uiOutput("slider1"),
                  #Choosing the Point Width
                  uiOutput("slider2"),
                  #Choosing the point shape
                  uiOutput("dropDown1")
                )
              ) |>
              bsplus::bs_append(
                title = 'Modify Axes',
                content = list(
                  #Y-axis Title
                  textInput(
                    "aytitle",
                    label = "Y-axis title",
                    placeholder = "Type in the title here",
                    value = c("Y-axis")
                  ),
                  checkboxInput('chksymbolY', label =
                                  "Add Special Characters", value = F),
                  uiOutput('showsymbolY'),
                  #X-axis Title
                  textInput(
                    "axtitle",
                    label = "X-axis title",
                    placeholder = "Type in the title here",
                    value = NA
                  ),
                  checkboxInput('chksymbolX', label =
                                  "Add Special Characters", value = F),
                  uiOutput('showsymbolX'),
                  
                  #X-axis rotation
                  sliderInput(
                    "Xrotate",
                    label = "X-axis rotation",
                    min = 0,
                    max = 90,
                    value = 0
                  ),
                  
                  #Choosing Y-axis transformation
                  radioButtons(
                    "logscale",
                    label = "Convert Y-axis in Log scale:",
                    choices = c(
                      "NA" = "none",
                      "Log10" = "log10",
                      "Log2" = "log2"
                    ),
                    inline = T
                  ),
                  #Choosing the Y-axis limits
                  numericInput(
                    'minY',
                    "Y-axis minimum limit:",
                    value = NA,
                    min = NA,
                    max = NA
                  ),
                  numericInput(
                    'maxY',
                    'Y-axis maximum limit:',
                    value = NA,
                    min = NA,
                    max = NA
                  ),
                  sliderInput(
                    'axisline',
                    'Axes linewidth:',
                    value = 1,
                    min = 0.2,
                    max = 2
                  ),
                  checkboxInput('askbreak', 'Add Y-axis break', value =
                                  F),
                  uiOutput('showbreak')
                )
              ) |>
              bsplus::bs_append(
                title = 'Modify Font',
                content = list(
                  #Select type face
                  selectInput(
                    'font',
                    'Select font:',
                    choices = c(
                      'Arial',
                      'Bookman Old Style',
                      'Calibri',
                      'Candara',
                      'Century Gothic',
                      'Corbel',
                      'Garamond',
                      'Georgia',
                      'Lucida Bright',
                      'Lucida Sans',
                      'Microsoft Sans Serif',
                      'Segoe UI',
                      'Tahoma',
                      'Times New Roman',
                      'Verdana'
                    )
                  ),
                  #Selecting font  size
                  sliderInput(
                    "Xfontcol",
                    "Select Font Size for X-axis texts:",
                    min = 5,
                    max = 50,
                    value = 18
                  ),
                  sliderInput(
                    "Xfontsz",
                    "Select Font Size for X-axis title:",
                    min = 5,
                    max = 50,
                    value = 18
                  ),
                  sliderInput(
                    "Xlinebreak",
                    "Adjust Line break of X-axis text",
                    min = 5,
                    max = 40,
                    value = 30
                  ),
                  sliderInput(
                    "Yfontcol",
                    "Select Font Size for Y-axis texts:",
                    min = 5,
                    max = 50,
                    value = 18
                  ),
                  sliderInput(
                    "Yfontsz",
                    "Select Font Size for Y-axis title:",
                    min = 5,
                    max = 50,
                    value = 18
                  ),
                  sliderInput(
                    "Ylinebreak",
                    "Adjust Line break of Y-axis title",
                    min = 5,
                    max = 40,
                    value = 30
                  )
                )
              ) |>
              bsplus::bs_append(
                title = 'Modify Plot Theme',
                content = list(
                  #Choosing the plot background
                  radioButtons(
                    "theme",
                    label = "Choose plot background theme:",
                    choices = c(
                      "Classical" = 'theme_classic',
                      "Minimal" = 'theme_minimal',
                      "Gray" = 'theme_gray',
                      "Dark" = "theme_dark",
                      "Light" = 'theme_light'
                    ),
                    inline = T,
                    selected = c("theme_classic")
                  ),
                  #Choosing theme generation
                  radioButtons(
                    "choosetheme",
                    label = "Choose theme generator:",
                    choices = c(
                      "Default" = "default",
                      "Preset Themes" =
                        'preset',
                      "Select a Gradient" =
                        'gradient',
                      "Select from Palette" =
                        'palette'
                    ),
                    selected = c("default")
                  ),
                  uiOutput('themelist'),
                  radioButtons(
                    "boxbordercol",
                    label = "Choose box border colour:",
                    choices = c(
                      'Default (Black)' = 'default',
                      "Darker" = 'dark',
                      "Lighter" = 'light'
                    ),
                    selected = c('default')
                  ),
                  uiOutput('shadeval'),
                  #Choosing datapoint colors
                  radioButtons(
                    "dpcolor",
                    label = "Choose datapoint colour:",
                    choices = c(
                      "Default (Black)" = 'default',
                      "Same as box colour" =
                        'box',
                      "Same as border colour" =
                        'border',
                      "Select a Gradient" =
                        'dpgradient',
                      "Select from Palette" = 'dppalette'
                    ),
                    selected = c("default")
                  ),
                  uiOutput('manualcolor'),
                  uiOutput('colUpdate')
                )
              ),
            radioButtons(
              "dpview",
              "Add datapoints count to your plot:",
              choices = c('No' = 0, 'Yes' =
                            5),
              inline = T
            ),
            #Adding annotation on plot
            tagList(
              uiOutput('grpselect'),
              uiOutput('askAnnotationV'),
              uiOutput('chooseAnnotation')
            )
          ),
          #Tab for significance analysis
          tabPanel(
            "Statistical Analysis",
            br(),
            #Get descriptive statistics
            actionButton('descstat', 'Get Descriptive Stat', style =
                           "margin-bottom:20px;"),
            uiOutput('descsStatOut'),
            #Running Normality test
            actionButton(
              "swtestsubmit",
              "Run Normality Test",
              title = 'Click to perform Shapiro-Wilk test',
              style = "margin-bottom:20px;"
            ),
            uiOutput("normalityTest"),
            br(),
            #Running the comparison tests
            checkboxInput("comptest", label =
                            "Run significance test for your dataset:"),
            tagList(
              uiOutput("comptestshow1"),
              uiOutput("comptestshow2"),
              uiOutput("comptestBtn"),
              uiOutput("posthoctitle"),
              uiOutput("posthocList"),
              uiOutput("posthocBtn"),
              uiOutput("tableModal")
            )
          )
        )),
        
        ##Mainpanel for Boxplot##
        
        mainPanel(
          h2("Visualize your plot here"),
          div(
            div(
              # tableOutput('testModBox'),
              uiOutput("plot_notice"),
              # uiOutput('themeSubmit'),
              shinycssloaders::withSpinner(
                uiOutput("MyBPlotFinal"),
                type = 5,
                color = "#9e9e9e",
                size = 2
              ),
              br()
            ),
            div(
              style = "display:inline-block",
              numericInput(
                'width',
                label = "Select Width of your plot",
                min = 100,
                max = 800,
                value = 500
              )
            ),
            div(
              style = "display:inline-block",
              numericInput(
                'height',
                label = "Select Height of your plot",
                min = 100,
                max = 800,
                value = 400
              )
            ),
            div(
              style = "display:inline-flex;
                          justify-content:space-between;
                          height:45px;
                          width:80%;
                          margin:3px;",
              actionButton(
                "copybtn2",
                "Copy Plot",
                icon = icon("copy"),
                class = "btn-primary",
                title = "Click to copy on clickboard"
              ),
              downloadButton(
                "downloadBPlot",
                "Download Plot",
                icon = shiny::icon("download"),
                title = "Click to download your plot"
              ),
              #Save Settings button
              downloadButton(
                "savesetting",
                "Save settings",
                icon = icon("gear"),
                class = "btn-primary",
                title = "Click to save your plot settings"
              ),
              div(
                fileInput(
                  "usesetting",
                  label = NULL,
                  buttonLabel = "Select Settings File",
                  accept = c(".csv")
                ),
                title = "Click to load your saved csv file to reuse previous setting"
              ),
              actionButton("reuseset", "Upload")
            )
          ),
          style = "position:fixed;
            display:flex;
            align-content:center;
            flex-direction:column;
            right: 0;
            width:65%;
            height:600px;
            overflow-y:scroll;
            z-index:999;"
        )
      )),
      #Tab for significance analysis
      tabPanel(
        "Statistical Analysis",pageWithSidebar(div(),
          sidebarPanel(#Get descriptive statistics
            actionButton('descstatV', 'Get Descriptive Stat', style =
                           "margin-bottom:20px;"),
            # uiOutput('descsStatOutV'),
            #Running Normality test
            actionButton(
              "swtestsubmitV",
              "Run Normality Test",
              title = 'Click to perform Shapiro-Wilk test',
              style = "margin-bottom:20px;"
            ),
            # uiOutput('normalityTestV'),
            br(),
            #Running the comparison tests
            checkboxInput("comptestV", label =
                            "Run significance test for your dataset:"),
            tagList(
              uiOutput("comptestshow1V"),
              uiOutput("comptestshow2V"),
              uiOutput("comptestBtnV"),
              uiOutput("posthoctitleV"),
              uiOutput("posthocListV"),
              uiOutput("posthocBtnV"),
              uiOutput("tableModalV")
            )),
          mainPanel(uiOutput('stattableout'))
        )
      ),
      tabPanel(
        "Info",
        p(
          "Thank You for using this app. I made this app to make customizable
        plots of mainly Violin and Box-Jitter-type plots. This app uses R at
        its core to handle the basic programming and the 'Shiny' Package to build
        the interface. The following packages have also been used to make this app
        for which I am thankful to its developers. For the Web-Based UI 'shinythemes',
        'shinyjs', 'shinyBS', and 'shinycssloaders' were used. For data handling 'openxlsx',
        'datasets', 'DT', 'tidyr', 'dplyr' and 'tidyverse' packages were used. For generation of
        the plot 'ggplot2' was mainly used along with an add on 'ggbeeswarm' to
        generate the jitter plot. For the theme generation 'DescTools', 'colorspace' and 'colourPicker' packages
        were used. For doing the statistical calculation majorly basic R was used. Apart from that 'FSA',
        'dunn.test', and 'asht' was used. For creating visual data tables 'broom'
        package was used.",
          style = "width:75%; padding:50px;text-align:center;
        margin:0 auto;margin-top:10%;
        font-size:17px;"
        ),
        p(
          "Please write to me at (sumit.sen@tifr.res.in) for your feedback or if you find any issues/ bugs.",
          style = "width:75%; padding-top:10px;text-align:center;margin:0 auto;margin-top:10%"
        ),
        p("App developed by Sumit. CC 2025.", style = "width:75%;padding-top:5px;text-align:center;margin:0 auto;"),
        p(
          a("Twitter", href = "https://twitter.com/SumitSen616"),
          "||",
          a("LinkedIn", href = "https://www.linkedin.com/in/sumitsen616/"),
          style = "width:75%;padding-top:5px;text-align:center;margin:0 auto;"
        )
        
      )
    )
  )
)

###########################
##Starting of Server Code##
###########################

server <- shinyServer(function(input, output, session) {
  #added "session" because updateSelectInput requires it
  ##Disabling the Plot UI Tabs
  
  shinyjs::disable(selector = '.tabbable a[data-value="Violin Plot"')
  shinyjs::disable(selector = '.tabbable a[data-value="Box Plot"')
  
  #Disabling save setting button by default and enabling only when 'Theme' tab is selected
  shinyjs::disable("savesetting")
  observeEvent(input$boxTab, {
    if (input$boxTab == 'theme') {
      shinyjs::enable("savesetting")
    }
  })
  shinyjs::disable("savesettingV")
  observeEvent(input$violinTab, {
    if (input$violinTab == 'themeV') {
      shinyjs::enable("savesettingV")
    }
  })
  observeEvent(
    input$MyVPlot,
    {
      shinyjs::runjs(
        '   async function getImageBlobFromUrl(url) {
            const fetchedImageData = await fetch(url);
            const blob = await fetchedImageData.blob();
            return blob;
          }
          $(document).ready(function () {
            $("#copybtn").on("click", async () => {
              const src = $("#MyVPlot>img").attr("src");
              try {
                const blob = await getImageBlobFromUrl(src);
                await navigator.clipboard.write([
                  new ClipboardItem({
                    [blob.type]: blob
                  })
                ]);
                alert("Image copied to clipboard!");
              } catch (err) {
                console.error(err.name, err.message);
                alert("There was an error while copying image to clipboard :/");
              }
            });
          });'
      )
    })
  observeEvent(
    input$MyBPlot,
    {
      shinyjs::runjs(
         'async function getImageBlobFromUrl(url) {
          const fetchedImageData = await fetch(url);
          const blob = await fetchedImageData.blob();
          return blob;
        }
        $(document).ready(function () {
          $("#copybtn2").on("click", async () => {
            const src = $("#MyBPlot>img").attr("src");
            try {
              const blob = await getImageBlobFromUrl(src);
              await navigator.clipboard.write([
                new ClipboardItem({
                  [blob.type]: blob
                })
              ]);
              alert("Image copied to clipboard!");
            } catch (err) {
              console.error(err.name, err.message);
              alert("There was an error while copying image to clipboard :/");
            }
          });
        });'
      )
    })

  sheetName <- reactive({
    req(input$file1) #  require that the input is available
    inFile <- input$file1
    sheet <- openxlsx::getSheetNames(inFile$datapath)
  })
  colNames <- reactive({
    req(input$file1, input$sheetlist)
    inFile <- input$file1
    df <- openxlsx::read.xlsx(xlsxFile = inFile$datapath,
                              sheet = input$sheetlist)
    colnames(df)
  })
  observe({
    req(sheetName())
    updateSelectInput(session,
                      "sheetlist",
                      "Select the sheet to plot your data",
                      choices = sheetName())
  })
  data <- reactive({
    req(input$file1, input$sheetlist, input$selectedCols) #  require that the input is available
    inFile <- input$file1
    df <- openxlsx::read.xlsx(
      xlsxFile = inFile$datapath,
      sheet = input$sheetlist,
      skipEmptyRows = TRUE,
      cols = which(colNames() %in% input$selectedCols),
      colNames = input$header,
      check.names = FALSE
    )
    df
  })
  
  
  
  #Render Submit button only when the file is uploaded
  output$fileupload <- renderUI({
    req(input$file1)
    tagList(
      radioButtons(
        'askPlotType',
        label = 'Choose the type of plot:',
        choices = c("Violin Plot" = 'violin', "Box-Jitter Plot" = 'box')
      ),
      actionButton('submitFile', 'Submit')
    )
  })
  
  #Show Data Table upon clicking submit button:
  observeEvent(input$submitFile, {
    output$contents <- renderDataTable({
      format(data(), nsmall = 2)
    })
    output$sheetnames <- renderUI({
      selectInput("sheetlist",
                  "Select the sheet to plot your data",
                  choices = sheetName())
    })
    output$colnames <- renderUI({
      req(colNames())
      checkboxGroupInput(
        "selectedCols",
        "Select Columns",
        choices = colNames(),
        selected = colNames()
      )
    })
    #Enabling the tab after uploading the data and clicking Submit
    if (input$askPlotType == 'violin') {
      shinyjs::enable(selector = '.tabbable a[data-value="Violin Plot"')
      shinyjs::disable(selector = '.tabbable a[data-value="Box Plot"')
    }
    else if (input$askPlotType == 'box') {
      shinyjs::disable(selector = '.tabbable a[data-value="Violin Plot"')
      shinyjs::enable(selector = '.tabbable a[data-value="Box Plot"')
    }
  })
  
  observeEvent(input$Vtheme, {
    showNotification("Hello")
  })
  
  
  orderdata <- reactive({
    newcols <- tidyselect::all_of(colnames(data()))
    order_data <-
      data() |> tidyr::pivot_longer(cols = newcols,
                              names_to = "variable",
                              values_to = "value") |>
      dplyr::arrange(variable)
    # for (i in 1:nrow(orderdata)){
    #   if (is.na(order_data[i,2])==TRUE){
    #     order_data <- order_data[-i,]
    #   }else{
    #     order_data <- order_data[i,]
    #   }
    # }
    # orderdata <- na.omit(order_data)
  })
  
  
  axistitleY <- reactive({
    aytitle <- as.character(req(input$aytitle))
  })
  axistitleX <- reactive({
    axtitle <- (input$axtitle)
  })
  output$showsymbolY <- renderUI({
    if (input$chksymbolY == T) {
      tagList(
        selectInput(
          'symbolsY',
          label = c('Select from the list'),
          choices = intToUtf8(
            c(
              0x03B1,
              0x03B2,
              0x03B3,
              0x03B4,
              0x03B5,
              0x03B6,
              0x03B7,
              0x03B8,
              0x03B9,
              0x03BA,
              0x03BB,
              0x03BC,
              0x03BD,
              0x03BE,
              0x03BF,
              0x03C0,
              0x03C1,
              0x03C2,
              0x03C3,
              0x03C4,
              0x03C5,
              0x03C6,
              0x03C7,
              0x03C8,
              0x03C9,
              0x00B0,
              0x00B2,
              0x00B3
            ),
            multiple = T
          )
        ),
        actionButton('addsymbolY', label = c('Add Character'))
      )
    }
  })
  output$showsymbolX <- renderUI({
    if (input$chksymbolX == T) {
      tagList(
        selectInput(
          'symbolsX',
          label = c('Select from the list'),
          choices = intToUtf8(
            c(
              0x03B1,
              0x03B2,
              0x03B3,
              0x03B4,
              0x03B5,
              0x03B6,
              0x03B7,
              0x03B8,
              0x03B9,
              0x03BA,
              0x03BB,
              0x03BC,
              0x03BD,
              0x03BE,
              0x03BF,
              0x03C0,
              0x03C1,
              0x03C2,
              0x03C3,
              0x03C4,
              0x03C5,
              0x03C6,
              0x03C7,
              0x03C8,
              0x03C9,
              0x00B0,
              0x00B2,
              0x00B3
            ),
            multiple = T
          )
        ),
        actionButton('addsymbolX', label = c('Add Character'))
      )
    }
  })
  observeEvent(input$addsymbolY, {
    updateTextInput(session,
                    "aytitle",
                    value = paste(input$aytitle, input$symbolsY, sep = ''))
  })
  observeEvent(input$addsymbolX, {
    updateTextInput(session, "axtitle", value = paste(input$axtitle, input$symbolsX))
  })
  output$showsymbolYV <- renderUI({
    if (input$chksymbolYV == T) {
      tagList(
        selectInput(
          'symbolsYV',
          label = c('Select from the list'),
          choices = intToUtf8(
            c(
              0x03B1,
              0x03B2,
              0x03B3,
              0x03B4,
              0x03B5,
              0x03B6,
              0x03B7,
              0x03B8,
              0x03B9,
              0x03BA,
              0x03BB,
              0x03BC,
              0x03BD,
              0x03BE,
              0x03BF,
              0x03C0,
              0x03C1,
              0x03C2,
              0x03C3,
              0x03C4,
              0x03C5,
              0x03C6,
              0x03C7,
              0x03C8,
              0x03C9,
              0x00B0,
              0x00B2,
              0x00B3
            ),
            multiple = T
          )
        ),
        actionButton('addsymbolYV', label = c('Add Character'))
      )
    }
  })
  output$showsymbolXV <- renderUI({
    if (input$chksymbolXV == T) {
      tagList(
        selectInput(
          'symbolsXV',
          label = c('Select from the list'),
          choices = intToUtf8(
            c(
              0x03B1,
              0x03B2,
              0x03B3,
              0x03B4,
              0x03B5,
              0x03B6,
              0x03B7,
              0x03B8,
              0x03B9,
              0x03BA,
              0x03BB,
              0x03BC,
              0x03BD,
              0x03BE,
              0x03BF,
              0x03C0,
              0x03C1,
              0x03C2,
              0x03C3,
              0x03C4,
              0x03C5,
              0x03C6,
              0x03C7,
              0x03C8,
              0x03C9,
              0x00B0,
              0x00B2,
              0x00B3
            ),
            multiple = T
          )
        ),
        actionButton('addsymbolXV', label = c('Add Character'))
      )
    }
  })
  observeEvent(input$addsymbolYV, {
    updateTextInput(session,
                    "aytitleV",
                    value = paste(input$aytitleV, input$symbolsYV, sep =
                                    ''))
  })
  observeEvent(input$addsymbolXV, {
    updateTextInput(session,
                    "axtitleV",
                    value = paste(input$axtitleV, input$symbolsXV, sep =
                                    ''))
  })
  xRotate <- reactive({
    Xrotate <- as.numeric(input$Xrotate)
  })
  logaxis <- reactive({
    logscale <- switch(input$logscale,
                       none = 'identity',
                       log10 = 'log10',
                       log2 = 'log2')
  })
  logaxisV <- reactive({
    logscale <- switch(input$logscaleV,
                       none = 'identity',
                       log10 = 'log10',
                       log2 = 'log2')
  })
  yaxisMin <- reactive({
    if (is.na(input$minY) == TRUE) {
      min <- NA
    } else{
      min <- input$minY
    }
  })
  yaxisMinV <- reactive({
    if (is.na(input$minYV) == TRUE) {
      min <- NA
    } else{
      min <- input$minYV
    }
  })
  yaxisMax <- reactive({
    if (is.na(input$maxY) == TRUE) {
      min <- NA
    } else{
      min <- input$maxY
    }
  })
  yaxisMaxV <- reactive({
    if (is.na(input$maxYV) == TRUE) {
      min <- NA
    } else{
      min <- input$maxYV
    }
  })
  output$slider1 <- renderUI({
    if (input$radio1 == "Yes") {
      sliderInput(
        "scatter",
        "Scatter Width:",
        min = 0.5,
        max = 5,
        value = 2
      )
    }
  })
  output$slider1V <- renderUI({
    if (input$radio1V == "Yes") {
      sliderInput(
        "boxWidthV",
        "Box Width:",
        min = 0.05,
        max = 0.3,
        value = 0.1
      )
    }
  })
  output$slider2 <- renderUI({
    if (input$radio1 == "Yes") {
      sliderInput(
        "pointsize",
        "Point Width:",
        min = 0.5,
        max = 2,
        value = 1
      )
    }
  })
  
  output$dropDown1 <- renderUI({
    if (input$radio1 == 'Yes') {
      selectInput(
        'pointshape',
        label = "Point Shape:",
        choices = c(
          "Default" = '19',
          "Empty Square" = '0',
          "Empty Circle" = '1',
          "Filled Square" = '15',
          "Filled Circle" = '16',
          "Filled Triangle" = '17',
          "Filled Diamond" = '18',
          "Plus" = '3',
          "Cross" = '4',
          "Asterisk" = '8'
        )
      )
    }
  })
  output$hLineV <- renderUI({
    radioButtons(
      'hLineShpV',
      label = "Mark the highest/lowest median with a horizontal line",
      choices = c(
        "None" = 'default',
        "Highest" = 'high',
        "Lowest" = 'low'
      ),
      inline = TRUE
    )
    
    
  })
  addScatter <- reactive({
    if (input$radio1 == "Yes") {
      scatter <- (input$scatter)
    }
  })
  addScatterV <- reactive({
    if (input$radio1V == "Yes") {
      scatter <- (input$scatterV)
    }
  })
  addPoint <- reactive({
    if (input$radio1 == "Yes") {
      (input$pointsize)
    }
  })
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
  addDataPointLabelV <- reactive({
    tempdata <- data()
    newdf <- data.frame()
    for (i in 1:ncol(tempdata)) {
      df <- data.frame(length(na.omit(tempdata[, i])))
      newdf <- rbind(newdf, df)
    }
    text <- newdf
    minval <- min(na.omit(tempdata))
    if (is.na(input$minYV)) {
      Ycord <- as.data.frame(rep(minval - (minval / 1.5), ncol(tempdata)))
    } else {
      Ycord <- input$minYV
    }
    
    Xcord <- data.frame(1:ncol(tempdata))
    finaldf <- cbind(text, Xcord, Ycord)
    colnames(finaldf) <- c('text', 'x', 'y')
    as.data.frame(finaldf)
  })
  #Break axis
  output$showbreak <- renderUI({
    if (input$askbreak == T) {
      tagList(
        numericInput('breakmin', 'Choose lower breakpoint', val = 0),
        numericInput('breakmax', 'Choose upper breakpoint', val = 0)
      )
    }
  })
  
  #Theme
  addTheme <- reactive({
    theme <- switch(
      input$theme,
      theme_minimal = theme_minimal(),
      theme_classic = theme_classic(),
      theme_gray = theme_gray(),
      theme_dark = theme_dark(),
      theme_light = theme_linedraw()
    )
  })
  addThemeV <- reactive({
    theme <- switch(
      input$themeV,
      theme_minimal = theme_minimal(),
      theme_classic = theme_classic(),
      theme_gray = theme_gray(),
      theme_dark = theme_dark(),
      theme_light = theme_linedraw()
    )
  })
  
  #Selecting Color values
  
  # dpval <- reactiveValues(df=data.frame(character(),stringsAsFactors = FALSE))#for Data points
  # observeEvent(input$dpgocol,{#for data points
  #
  #   if (nrow(dpval$df) < length(data())) {
  #     tempval <- data.frame(input$dpcolorInp,stringsAsFactors = FALSE)
  #     dpval$df <- rbind(dpval$df,tempval)
  #   } else{
  #     showNotification("Can't select more", type = "warning")
  #   }
  #   output$dpcolorTabs <- renderUI({
  #     tags$div(
  #       tagList(
  #         lapply(1:nrow(dpval$df), function(i) {
  #           tags$span(id="span2",
  #                     style= paste0("background-color:",as.character(dpval$df[i,]),
  #                                   ";padding:4px 10px;margin:5px;border:2px solid black;")
  #           )
  #         })
  #       ),style="margin-top:10px; margin-bottom:10px;"
  #     )
  #   })
  #
  # })
  # observeEvent(input$dpresetall,{
  #   dpval$df <- NULL
  #   dpval$df <- data.frame(character(),stringsAsFactors = FALSE)
  #   output$dpcolorTabs <- renderUI({
  #     tags$div(
  #       tagList(
  #         lapply(1:nrow(dpval$df), function(i) {
  #           tags$span(id="span2",
  #                     style= paste0("display:none")
  #           )
  #         })
  #       ),style="margin-top:10px; margin-bottom:10px;"
  #     )
  #   })
  # })
  
  
  #Theme generator processing
  coltabs <- reactive({
    lapply(seq_along(data()), function(i) {
      div(colourInput(
        paste("colors", i, sep = '_'),
        paste("Col", i, sep = ''),
        value = c("#CCCCCC")
      ), style = "width:80px;display:inline-block;")
    })
  })
  coltabsV <- reactive({
    lapply(seq_along(data()), function(i) {
      div(colourInput(
        paste("colorsV", i, sep = '_'),
        paste("Col", i, sep = ''),
        value = c("#CCCCCC")
      ), style = "width:80px;display:inline-block;")
    })
  })
  dpcoltabs <- reactive({
    lapply(seq_along(data()), function(i) {
      div(colourInput(
        paste("colorsdp", i, sep = '_'),
        paste("Col", i, sep = ''),
        value = c("#CCCCCC")
      ), style = "width:80px;display:inline-block;")
    })
  })
  #Box theme display
  output$themelist <- renderUI({
    if (input$choosetheme == 'preset') {
      tagList(
        #Choosing the preset theme
        radioButtons(
          "boxtheme",
          label = "Choose preset themes for the boxes:",
          choices = c(
            'Purrrrple' = 'purples',
            'Shred of Green' = 'greens',
            'Cherry Blossom' = 'pinks',
            'Center Of the Earth' = 'oranges',
            'Cold-feet' = 'blues',
            '22K Gold' = 'golds',
            'Desaturated Rainbow' = 'rainbows',
            'London Weather' = 'greys'
          )
        ),
        div(
          colourInput(
            'grad1',
            "Choose Color 1",
            palette = c("square"),
            value = '#EEE1EF'
          ),
          colourInput(
            'grad2',
            "Choose Color 2",
            palette = c("square"),
            value = '#554994'
          ),
          coltabs(),
          style = "display:none;"
        ),
        radioButtons(
          "grayscale",
          label = "Check Contrast:",
          choices = c("No", "Yes"),
          inline = T
        )
      )
    }
    else if (input$choosetheme == 'gradient') {
      tagList(
        colourInput(
          'grad1',
          "Choose Color 1",
          palette = c("square"),
          value = '#EEE1EF'
        ),
        colourInput(
          'grad2',
          "Choose Color 2",
          palette = c("square"),
          value = '#554994'
        ),
        radioButtons(
          "grayscale",
          label = "Check Contrast:",
          choices = c("No", "Yes"),
          inline = T
        ),
        div(
          radioButtons(
            "boxtheme",
            label = "Choose preset themes for the boxes:",
            choices = c(
              'Purrrrple' = 'purples',
              'Shred of Green' = 'greens',
              'Cherry Blossom' = 'pinks',
              'Center Of the Earth' = 'oranges',
              'Cold-feet' = 'blues',
              '22K Gold' = 'golds',
              'Desaturated Rainbow' = 'rainbows',
              'London Weather' = 'greys'
            )
          ),
          coltabs(),
          style = "display:none;"
        )
      )
      
    }
    else if (input$choosetheme == 'palette') {
      tagList(
        #Choosing from a palette
        coltabs(),
        radioButtons(
          "grayscale",
          label = "Check Contrast:",
          choices = c("No", "Yes"),
          inline = T
        ),
        div(
          radioButtons(
            "boxtheme",
            label = "Choose preset themes for the boxes:",
            choices = c(
              'Purrrrple' = 'purples',
              'Shred of Green' = 'greens',
              'Cherry Blossom' = 'pinks',
              'Center Of the Earth' = 'oranges',
              'Cold-feet' = 'blues',
              '22K Gold' = 'golds',
              'Desaturated Rainbow' = 'rainbows',
              'London Weather' = 'greys'
            )
          ),
          colourInput(
            'grad1',
            "Choose Color 1",
            palette = c("square"),
            value = '#EEE1EF'
          ),
          colourInput(
            'grad2',
            "Choose Color 2",
            palette = c("square"),
            value = '#554994'
          ),
          style = "display:none;"
        )
      )
    }
    else if (input$choosetheme == 'default') {
      tagList(
        #For default case
        div(
          radioButtons(
            "boxtheme",
            label = "Choose preset themes for the boxes:",
            choices = c(
              'Purrrrple' = 'purples',
              'Shred of Green' = 'greens',
              'Cherry Blossom' = 'pinks',
              'Center Of the Earth' = 'oranges',
              'Cold-feet' = 'blues',
              '22K Gold' = 'golds',
              'Desaturated Rainbow' = 'rainbows',
              'London Weather' = 'greys'
            )
          ),
          colourInput(
            'grad1',
            "Choose Color 1",
            palette = c("square"),
            value = '#EEE1EF'
          ),
          colourInput(
            'grad2',
            "Choose Color 2",
            palette = c("square"),
            value = '#554994'
          ),
          coltabs(),
          style = "display:none;"
        )
      )
    }
  })
  
  userTheme <- reactive({
    #nrow(val$df)<length(data())
    if (input$choosetheme == 'preset') {
      #Take pre-selected color palette and feed it to ggplot
      usertheme <- switch(
        input$boxtheme,
        purples = colorRampPalette(c("#EEE1EF", "#554994"))(ncol(data())),
        greens = colorRampPalette(c("#97C4B8", "#064420"))(ncol(data())),
        pinks = colorRampPalette(c('#FDE4DE', '#F56093'))(ncol(data())),
        oranges = colorRampPalette(c('#E38B29', '#CD104D'))(ncol(data())),
        blues = colorRampPalette(c('#2C74B3', '#0A2647'))(ncol(data())),
        golds = colorRampPalette(c('#DB8E1E', '#5E3D0C'))(ncol(data())),
        rainbows = colorRampPalette(c('#bae1ff', '#ffb3ba'))(ncol(data())),
        greys = colorRampPalette(c('#C0C0C2', '#373737'))(ncol(data()))
      )
      if (input$grayscale == "Yes") {
        boxtheme <- ColToGray(usertheme)
      } else{
        boxtheme <- usertheme
      }
    }
    else if (input$choosetheme == 'gradient') {
      usertheme <- colorRampPalette(c(input$grad1, input$grad2))(ncol(data()))
      if (input$grayscale == "Yes") {
        boxtheme <- ColToGray(usertheme)
      } else{
        boxtheme <- usertheme
      }
    }
    else if (input$choosetheme == 'palette') {
      boxtheme <- lapply(seq_along(data()), function(i) {
        input[[paste("colors", i, sep = '_')]]
      })
      if (input$grayscale == "Yes") {
        boxtheme <- ColToGray(boxtheme)
      } else{
        boxtheme <- boxtheme
      }
    }
  })
  
  addboxTheme <- reactive({
    if (input$choosetheme == 'default') {
      usertheme <- colorRampPalette(c("#EEE1EF", "#554994"))(ncol(data()))
    } else{
      usertheme <- userTheme()
    }
  })
  output$shadeval <- renderUI({
    if (input$boxbordercol == 'dark' || input$boxbordercol == 'light') {
      sliderInput(
        'shadevalue',
        label = 'Select border shade amount',
        min = 0.1,
        max = 1,
        value = 0.2
      )
    }
  })
  boxborder <- reactive({
    if (input$boxbordercol == 'default') {
      boxborder <- rep(c("black"), each = length(colnames(data())))
    } else if (input$boxbordercol == 'light') {
      boxborder <- lighten(addboxTheme(), input$shadevalue)
    } else if (input$boxbordercol == 'dark') {
      boxborder <- darken(addboxTheme(), input$shadevalue)
    }
  })
  
  #Violin theme display
  output$themelistV <- renderUI({
    if (input$choosethemeV == 'preset') {
      tagList(
        #Choosing the preset theme
        radioButtons(
          "viotheme",
          label = "Choose preset themes for the boxes:",
          choices = c(
            'Purrrrple' = 'purples',
            'Shred of Green' = 'greens',
            'Cherry Blossom' = 'pinks',
            'Center Of the Earth' = 'oranges',
            'Cold-feet' = 'blues',
            '22K Gold' = 'golds',
            'Desaturated Rainbow' = 'rainbows',
            'London Weather' = 'greys'
          )
        ),
        div(
          colourInput(
            'grad1V',
            "Choose Color 1",
            palette = c("square"),
            value = '#EEE1EF'
          ),
          colourInput(
            'grad2V',
            "Choose Color 2",
            palette = c("square"),
            value = '#554994'
          ),
          coltabsV(),
          style = "display:none;"
        ),
        radioButtons(
          "grayscaleV",
          label = "Check Contrast:",
          choices = c("No", "Yes"),
          inline = T
        )
      )
    }
    else if (input$choosethemeV == 'gradient') {
      tagList(
        colourInput(
          'grad1V',
          "Choose Color 1",
          palette = c("square"),
          value = '#EEE1EF'
        ),
        colourInput(
          'grad2V',
          "Choose Color 2",
          palette = c("square"),
          value = '#554994'
        ),
        radioButtons(
          "grayscaleV",
          label = "Check Contrast:",
          choices = c("No", "Yes"),
          inline = T
        ),
        div(
          radioButtons(
            "viotheme",
            label = "Choose preset themes for the boxes:",
            choices = c(
              'Purrrrple' = 'purples',
              'Shred of Green' = 'greens',
              'Cherry Blossom' = 'pinks',
              'Center Of the Earth' = 'oranges',
              'Cold-feet' = 'blues',
              '22K Gold' = 'golds',
              'Desaturated Rainbow' = 'rainbows',
              'London Weather' = 'greys'
            )
          ),
          coltabsV(),
          style = "display:none;"
        )
      )
      
    }
    else if (input$choosethemeV == 'palette') {
      tagList(
        #Choosing from a palette
        coltabsV(),
        radioButtons(
          "grayscaleV",
          label = "Check Contrast:",
          choices = c("No", "Yes"),
          inline = T
        ),
        div(
          radioButtons(
            "viotheme",
            label = "Choose preset themes for the boxes:",
            choices = c(
              'Purrrrple' = 'purples',
              'Shred of Green' = 'greens',
              'Cherry Blossom' = 'pinks',
              'Center Of the Earth' = 'oranges',
              'Cold-feet' = 'blues',
              '22K Gold' = 'golds',
              'Desaturated Rainbow' = 'rainbows',
              'London Weather' = 'greys'
            )
          ),
          colourInput(
            'grad1V',
            "Choose Color 1",
            palette = c("square"),
            value = '#EEE1EF'
          ),
          colourInput(
            'grad2V',
            "Choose Color 2",
            palette = c("square"),
            value = '#554994'
          ),
          style = "display:none;"
        )
      )
    }
    else if (input$choosethemeV == 'default') {
      tagList(
        #For default case
        div(
          radioButtons(
            "viotheme",
            label = "Choose preset themes for the boxes:",
            choices = c(
              'Purrrrple' = 'purples',
              'Shred of Green' = 'greens',
              'Cherry Blossom' = 'pinks',
              'Center Of the Earth' = 'oranges',
              'Cold-feet' = 'blues',
              '22K Gold' = 'golds',
              'Desaturated Rainbow' = 'rainbows',
              'London Weather' = 'greys'
            )
          ),
          colourInput(
            'grad1V',
            "Choose Color 1",
            palette = c("square"),
            value = '#EEE1EF'
          ),
          colourInput(
            'grad2V',
            "Choose Color 2",
            palette = c("square"),
            value = '#554994'
          ),
          coltabsV(),
          style = "display:none;"
        )
      )
    }
  })
  
  userThemeV <- reactive({
    #nrow(val$df)<length(data())
    if (input$choosethemeV == 'preset') {
      #Take pre-selected color palette and feed it to ggplot
      userthemeV <- switch(
        input$viotheme,
        purples = colorRampPalette(c("#EEE1EF", "#554994"))(ncol(data())),
        greens = colorRampPalette(c("#97C4B8", "#064420"))(ncol(data())),
        pinks = colorRampPalette(c('#FDE4DE', '#F56093'))(ncol(data())),
        oranges = colorRampPalette(c('#E38B29', '#CD104D'))(ncol(data())),
        blues = colorRampPalette(c('#2C74B3', '#0A2647'))(ncol(data())),
        golds = colorRampPalette(c('#DB8E1E', '#5E3D0C'))(ncol(data())),
        rainbows = colorRampPalette(c('#bae1ff', '#ffb3ba'))(ncol(data())),
        greys = colorRampPalette(c('#C0C0C2', '#373737'))(ncol(data()))
      )
      if (input$grayscaleV == "Yes") {
        viotheme <- ColToGray(userthemeV)
      } else{
        viotheme <- userthemeV
      }
    }
    else if (input$choosethemeV == 'gradient') {
      userthemeV <- colorRampPalette(c(input$grad1V, input$grad2V))(ncol(data()))
      if (input$grayscaleV == "Yes") {
        viotheme <- ColToGray(userthemeV)
      } else{
        viotheme <- userthemeV
      }
    }
    else if (input$choosethemeV == 'palette') {
      viotheme <- lapply(seq_along(data()), function(i) {
        input[[paste("colorsV", i, sep = '_')]]
      })
      if (input$grayscaleV == "Yes") {
        viotheme <- ColToGray(viotheme)
      } else{
        viotheme <- viotheme
      }
    }
  })
  
  addvioTheme <- reactive({
    if (input$choosethemeV == 'default') {
      usertheme <- colorRampPalette(c("#EEE1EF", "#554994"))(ncol(data()))
    } else{
      usertheme <- userThemeV()
    }
  })
  vioborder <- reactive({
    if (input$viobordercol == 'default') {
      vioborder <- rep(c("black"), each = length(colnames(data())))
    } else if (input$viobordercol == 'light') {
      vioborder <- lighten(addvioTheme(), input$shadevalueV)
    } else if (input$viobordercol == 'dark') {
      vioborder <- darken(addvioTheme(), input$shadevalueV)
    }
  })
  output$shadevalV <- renderUI({
    if (input$viobordercol == 'dark' || input$viobordercol == 'light') {
      sliderInput(
        'shadevalueV',
        label = 'Select border shade amount',
        min = 0.1,
        max = 1,
        value = 0.2
      )
    }
  })
  ##Data point color process
  output$manualcolor <- renderUI({
    if (input$dpcolor == 'dpgradient') {
      tagList(
        colourInput(
          'dpgrad1',
          "Choose Color 1",
          palette = c("square"),
          value = '#EEE1EF'
        ),
        colourInput(
          'dpgrad2',
          "Choose Color 2",
          palette = c("square"),
          value = '#554994'
        ),
        div(dpcoltabs(), style = "display:none;")
      )
    } else if (input$dpcolor == 'dppalette') {
      tagList(dpcoltabs(),
              div(
                colourInput(
                  'dpgrad1',
                  "Choose Color 1",
                  palette = c("square"),
                  value = '#EEE1EF'
                ),
                colourInput(
                  'dpgrad2',
                  "Choose Color 2",
                  palette = c("square"),
                  value = '#554994'
                ),
                style = "display:none;"
              ))
    } else if (input$dpcolor == 'default') {
      tagList(div(
        colourInput(
          'dpgrad1',
          "Choose Color 1",
          palette = c("square"),
          value = '#EEE1EF'
        ),
        colourInput(
          'dpgrad2',
          "Choose Color 2",
          palette = c("square"),
          value = '#554994'
        ),
        dpcoltabs(),
        style = "display:none;"
      ))
    }
  })
  dpcolors <- reactive({
    if (input$dpcolor == 'default') {
      dpcolors <- rep(c('#000000'), ncol(data()))
    } else if (input$dpcolor == 'box') {
      dpcolors <- addboxTheme()
    } else if (input$dpcolor == 'border') {
      dpcolors <-  boxborder()
    } else if (input$dpcolor == 'dpgradient') {
      dpcolors <- colorRampPalette(c(input$dpgrad1, input$dpgrad2))(ncol(data()))
    } else if (input$dpcolor == 'dppalette') {
      dpcolors <- lapply(seq_along(data()), function(i) {
        input[[paste("colorsdp", i, sep = '_')]]
      })
    }
    df <- data.frame()
    for (i in 1:ncol(data())) {
      test <- as.data.frame(unlist(rep(dpcolors[i], nrow(data(
        
      )[i]))))
      df <- rbind(df, test)
    }
    colnames(df) <- c('point color')
    dpcolors <- as.list(df)
  })
  
  #Font-family
  fontfamily <- reactive({
    # Load the fonts
    # font_import()
    loadfonts(device = 'win')
    input$font
  })
  fontfamilyV <- reactive({
    # Load the fonts
    # font_import()
    loadfonts(device = 'win')
    input$fontV
  })
  
  #Output for Violin plot
  #### Main GGplot
  output$plot_notice2 <- renderUI({
    if ((input$aytitleV == "")) {
      p("Y-axis title is required to generate a plot", style = "color:#B5001C;background-color:#e7e7e7;padding:10px;border:1px solid #d6d6d6;")
    } else{
      
    }
  })
  
  addhLine <- reactive({
    if (input$hLineShpV == 'default') {
      addhline <- 0
    } else if (input$hLineShpV == 'high') {
      addhline <- max(apply(na.omit(data()), 2, median))
    } else if (input$hLineShpV == 'low') {
      addhline <- min(apply(na.omit(data()), 2, median))
    }
  })
  hlwdV <- reactive({
    if (input$hLineShpV == 'default') {
      linewidth <- 0
    } else {
      linewidth <- 0.4
    }
  })
  
  plotinputV <- function() {
    x <- orderdata()
    x_axis <- c(factor(orderdata()$variable, levels = colnames(data())))
    x_axis_col <- gsub('[.]', ' ', colnames(data()))
    if ((input$radio1V == "Yes")) {
      ggplot(x, aes(x = x_axis, y = value)) +
        geom_violin(
          mapping = aes(fill = x_axis, color = variable),
          size = input$borderWidthV,
          trim = TRUE,
          scale = "count"
        ) +
        stat_summary(fun = median) +
        stat_boxplot(
          geom = "errorbar",
          width = 0.15,
          mapping=aes(color = variable),
          lwd = 1
        ) +
        geom_hline(
          yintercept = addhLine(),
          color = '#2c2c2c',
          linetype = 5,
          linewidth = hlwdV()
        ) +
        geom_boxplot(
          width = input$boxWidthV,
          size = 1,
          mapping=aes(color = variable),
          outlier.shape = NA
        ) +
        scale_color_manual(values = vioborder()) +
        addThemeV() +
        labs(
          y = str_wrap(input$aytitleV, width = input$YlinebreakV),
          x = input$axtitleV
        ) +
        coord_trans(limy = c(yaxisMinV(), yaxisMaxV())) +
        scale_fill_manual(values = addvioTheme()) +
        scale_y_continuous(trans = logaxisV()) +
        scale_x_discrete(label = str_wrap(x_axis_col,width=input$XlinebreakV), guide = guide_axis(angle =
                                                                  as.numeric(input$XrotateV))) +
        theme(
          axis.text.x = element_text(size = input$XfontcolV, color = "black"),
          axis.title.x = element_text(size = input$XfontszV, color = "black"),
          axis.text.y = element_text(size = input$YfontcolV, color = "black"),
          axis.title.y = element_text(size = input$YfontszV, color = "black"),
          legend.position = "none",
          axis.line = element_line(linewidth = input$axislineV),
          text = element_text(family = fontfamilyV())
        ) + geom_text(
          data = addDataPointLabelV(),
          mapping = aes(
            x = x,
            y = y,
            label = paste(c(addDataPointLabelV()$text))
          ),
          size = as.numeric(input$dpviewV)
        )
    }
    else{
      ggplot(x, aes(x = x_axis, y = value)) +
        geom_violin(
          mapping = aes(fill = x_axis, color = variable),
          size = input$borderWidthV,
          trim = TRUE,
          scale = "count"
        ) +
        scale_color_manual(values = rep(c("black"), each = length(colnames(data(
          
        ))))) +
        stat_summary(fun = median) +
        stat_boxplot(
          geom = "errorbar",
          width = 0.15,
          color = 1,
          lwd = 1
        ) +
        geom_hline(
          yintercept = addhLine(),
          color = '#2c2c2c',
          linetype = 5,
          linewidth = hlwdV()
        ) +
        addThemeV() +
        labs(
          y = str_wrap(input$aytitleV, width = input$YlinebreakV),
          x = input$axtitleV
        ) +
        coord_trans(limy = c(yaxisMinV(), yaxisMaxV())) +
        scale_fill_manual(values = addvioTheme()) +
        scale_y_continuous(trans = logaxisV()) +
        scale_x_discrete(label = str_wrap(x_axis_col,width=input$XlinebreakV), guide = guide_axis(angle =
                                                                  as.numeric(input$XrotateV))) +
        theme(
          axis.text.x = element_text(size = input$XfontcolV, color = "black"),
          axis.title.x = element_text(size = input$XfontszV, color = "black"),
          axis.text.y = element_text(size = input$YfontcolV, color = "black"),
          axis.title.y = element_text(size = input$YfontszV, color = "black"),
          legend.position = "none",
          axis.line = element_line(linewidth = input$axislineV),
          text = element_text(family = fontfamilyV())
        ) + geom_text(
          data = addDataPointLabelV(),
          mapping = aes(
            x = x,
            y = y,
            label = paste(c(addDataPointLabelV()$text))
          ),
          size = as.numeric(input$dpviewV)
        )
    }
  }
  plotAnnoteinputV <- function() {
    x <- orderdata()
    x_axis <- c(factor(orderdata()$variable, levels = colnames(data())))
    x_axis_col <- gsub('[.]', ' ', colnames(data()))
    if (input$radio1V == "Yes") {
      ggplot(x, aes(x = x_axis, y = value)) +
        geom_violin(
          mapping = aes(fill = x_axis, color = variable),
          size = input$borderWidthV,
          trim = TRUE,
          scale = "count"
        ) +
        scale_color_manual(values = rep(c("black"), each = length(colnames(data(
          
        ))))) +
        stat_summary(fun = median) +
        stat_boxplot(
          geom = "errorbar",
          width = 0.15,
          color = 1,
          lwd = 1
        ) +
        geom_hline(
          yintercept = addhLine(),
          color = '#2c2c2c',
          linetype = 5,
          linewidth = hlwdV()
        ) +
        geom_boxplot(
          width = input$boxWidthV,
          size = 1,
          color = c('black'),
          outlier.shape = NA
        ) +
        addThemeV() +
        labs(
          y = str_wrap(input$aytitleV, width = input$YlinebreakV),
          x = input$axtitleV
        ) +
        coord_trans(limy = c(yaxisMinV(), yaxisMaxV())) +
        scale_fill_manual(values = addvioTheme()) +
        scale_y_continuous(trans = logaxisV()) +
        scale_x_discrete(label = str_wrap(x_axis_col,width=input$XlinebreakV), guide = guide_axis(angle =
                                                                  as.numeric(input$XrotateV))) +
        theme(
          axis.text.x = element_text(size = input$XfontcolV, color = "black"),
          axis.title.x = element_text(size = input$XfontszV, color = "black"),
          axis.text.y = element_text(size = input$YfontcolV, color = "black"),
          axis.title.y = element_text(size = input$YfontszV, color = "black"),
          legend.position = "none",
          axis.line = element_line(linewidth = input$axislineV),
          text = element_text(family = fontfamilyV())
        ) +
        geom_segment(
          pairLinesV(),
          mapping = aes(
            x = x1,
            xend = x2,
            y = y1,
            yend = y2
          ),
          size = 0.8
        ) +
        geom_segment(
          pairLinesV(),
          mapping = aes(
            x = x1,
            xend = x1,
            y = y1 - (2 * (y1) / 100),
            yend = y2
          ),
          size = 0.8
        ) +
        geom_segment(
          pairLinesV(),
          mapping = aes(
            x = x2,
            xend = x2,
            y = y1 - (2 * (y1) / 100),
            yend = y2
          ),
          size = 0.8
        ) +
        geom_text(
          pairLinesV(),
          mapping = aes(
            x = (x1 + x2) / 2,
            y = y1 + (3 * (y1) / 100),
            label = paste(c(pairLinesV()[, 5]))
          ),
          size = input$annoteSizeV
        ) +
        geom_text(
          pairLinesV(),
          mapping = aes(
            x = (x1 + x2) / 2,
            y = y1 + (input$pvaldistV * (y1) / 100),
            label = paste(c(pairLinesV()[, 6]))
          ),
          size = input$pvalfontV
        ) + geom_text(
          data = addDataPointLabelV(),
          mapping = aes(
            x = x,
            y = y,
            label = paste(c(addDataPointLabelV()$text))
          ),
          size = as.numeric(input$dpviewV)
        )
    }
    else if ((input$radio1V == "No")) {
      ggplot(x, aes(x = x_axis, y = value)) +
        geom_violin(
          mapping = aes(fill = x_axis, color = variable),
          size = input$borderWidthV,
          trim = TRUE,
          scale = "count"
        ) +
        scale_color_manual(values = rep(c("black"), each = length(colnames(data(
          
        ))))) +
        stat_summary(fun = median) +
        stat_boxplot(
          geom = "errorbar",
          width = 0.15,
          color = 1,
          lwd = 1
        ) +
        geom_hline(
          yintercept = addhLine(),
          color = '#2c2c2c',
          linetype = 5,
          linewidth = hlwdV()
        ) +
        addThemeV() +
        labs(
          y = str_wrap(input$aytitleV, width = input$YlinebreakV),
          x = input$axtitleV
        ) +
        coord_trans(limy = c(yaxisMinV(), yaxisMaxV())) +
        scale_fill_manual(values = addvioTheme()) +
        scale_y_continuous(trans = logaxisV()) +
        scale_x_discrete(label = str_wrap(x_axis_col,width=input$XlinebreakV), guide = guide_axis(angle =
                                                                  as.numeric(input$XrotateV))) +
        theme(
          axis.text.x = element_text(size = input$XfontcolV, color = "black"),
          axis.title.x = element_text(size = input$XfontszV, color = "black"),
          axis.text.y = element_text(size = input$YfontcolV, color = "black"),
          axis.title.y = element_text(size = input$YfontszV, color = "black"),
          legend.position = "none",
          axis.line = element_line(linewidth = input$axislineV),
          text = element_text(family = fontfamilyV())
        ) + geom_segment(
          pairLinesV(),
          mapping = aes(
            x = x1,
            xend = x2,
            y = y1,
            yend = y2
          ),
          size = 0.8
        ) +
        geom_segment(
          pairLinesV(),
          mapping = aes(
            x = x1,
            xend = x1,
            y = y1 - (2 * (y1) / 100),
            yend = y2 + (2 * (y2) / 100)
          ),
          size = 0.8
        ) +
        geom_segment(
          pairLinesV(),
          mapping = aes(
            x = x2,
            xend = x2,
            y = y1 - (2 * (y1) / 100),
            yend = y2 + (2 * (y2) / 100)
          ),
          size = 0.8
        ) +
        geom_text(
          pairLinesV(),
          mapping = aes(
            x = (x1 + x2) / 2,
            y = y1 + (3 * (y1) / 100),
            label = paste(c(pairLinesV()[, 5]))
          ),
          size = input$annoteSizeV
        ) +
        geom_text(
          pairLinesV(),
          mapping = aes(
            x = (x1 + x2) / 2,
            y = y1 + (input$pvaldistV * (y1) / 100),
            label = paste(c(pairLinesV()[, 6]))
          ),
          size = input$pvalfontV
        ) + geom_text(
          data = addDataPointLabelV(),
          mapping = aes(
            x = x,
            y = y,
            label = paste(c(addDataPointLabelV()$text))
          ),
          size = as.numeric(input$dpviewV)
        )
    }
  }
  
  output$MyVPlot <- renderPlot({
    if (nrow(grpdataV$df) != 0) {
      plotAnnoteinputV()
    }
    else{
      plotinputV()
    }
  })
  output$MyVPlotFinal <- renderUI({
    shinycssloaders::withSpinner(
      plotOutput(
        "MyVPlot",
        width = input$widthV,
        height = input$heightV
      ),
      type = getOption("spinner.type", default = 6),
      color = getOption("spinner.color", default = "#BCE290"),
      size = getOption("spinner.size", default = 1.5)
    )
  })
  
  #Download ViolinPlot
  output$downloadVPlot <- downloadHandler(
    filename = "Violinplot.png",
    content = function(file) {
      png(file)
      if (nrow(grpdataV$df) != 0) {
        print(plotAnnoteinputV())
      }
      else{
        print(plotinputV())
      }
      dev.off()
    }
  )
  #Download Violinplot settings
  output$savesettingV <- downloadHandler(
    filename = function() {
      paste("graph_setting_violin-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(graphsettingsV(), file)
    }
  )
  
  ###output for box plot###
  
  output$plot_notice <- renderUI({
    if ((input$aytitle == "")) {
      p("Y-axis title is required to generate a plot", style = "color:#B5001C;background-color:#e7e7e7;padding:10px;border:1px solid #d6d6d6;")
    } else{
      
    }
  })
  ## GGplot
  boxplot <- reactive({
    x <- orderdata()
    x_axis <- c(factor(orderdata()$variable, levels = colnames(data())))
    x_axis_col <- gsub('[.]', ' ', colnames(data()))
    ggplot(x, aes(x = x_axis, y = value)) +
      geom_boxplot(
        mapping = aes(fill = x_axis, color = variable),
        width = input$boxwidth,
        size = 1,
        lwd = input$linewidth,
        fatten = 2,
        show.legend = NA,
        notch = input$notch,
        outlier.shape = as.numeric(input$outlier)
      ) +
      stat_summary(fun = median) +
      stat_boxplot(
        geom = "errorbar",
        width = 0.15,
        mapping = aes(color = variable),
        lwd = 1
      ) +
      scale_color_manual(values = boxborder()) +
      addTheme() +
      labs(y = str_wrap(axistitleY(), width = input$Ylinebreak),
           x = axistitleX()) +
      coord_trans(y = logaxis(), limy = c(yaxisMin(), yaxisMax())) +
      scale_fill_manual(values = addboxTheme()) +
      scale_x_discrete(label = str_wrap(x_axis_col,width=input$Xlinebreak), guide = guide_axis(angle = xRotate())) +
      # scale_y_break(c(0,0))+
      theme(
        axis.text.x = element_text(size = input$Xfontcol, color = "black"),
        axis.title.x = element_text(size = input$Xfontsz, color = "black"),
        axis.text.y = element_text(color = "black", size = input$Yfontcol),
        axis.title.y = element_text(size = input$Yfontsz, color = "black"),
        legend.position = "none",
        axis.line = element_line(linewidth = input$axisline),
        text = element_text(family = fontfamily())
      ) + geom_text(
        data = addDataPointLabel(),
        mapping = aes(
          x = x,
          y = y,
          label = paste(c(addDataPointLabel()$text))
        ),
        size = as.numeric(input$dpview)
      )
  })
  
  plotinput <- function() {
    if ((input$radio1 == "Yes")) {
      boxplot() +
        geom_beeswarm(
          size = addPoint(),
          cex = addScatter(),
          pch = as.numeric(input$pointshape),
          colour = unlist(dpcolors())
        )
    }
    else{
      boxplot()
    }
  }
  plotAnnoteinput <- function() {
      plotinput() +
        geom_segment(
          pairLines(),
          mapping = aes(
            x = x1,
            xend = x2,
            y = y1,
            yend = y2
          ),
          size = 0.8
        ) +
        geom_segment(
          pairLines(),
          mapping = aes(
            x = x1,
            xend = x1,
            y = y1 - (2 * (y1) / 100),
            yend = y2
          ),
          size = 0.8
        ) +
        geom_segment(
          pairLines(),
          mapping = aes(
            x = x2,
            xend = x2,
            y = y1 - (2 * (y1) / 100),
            yend = y2
          ),
          size = 0.8
        ) +
        geom_text(
          pairLines(),
          mapping = aes(
            x = (x1 + x2) / 2,
            y = y1 + (3 * (y1) / 100),
            label = paste(c(pairLines()[, 5]))
          ),
          size = input$annoteSize
        ) +
        geom_text(
          pairLines(),
          mapping = aes(
            x = (x1 + x2) / 2,
            y = y1 + (input$pvaldist * (y1) / 100),
            label = paste(c(pairLines()[, 6]))
          ),
          size = input$pvalfont
        ) +
        geom_text(
          data = addDataPointLabel(),
          mapping = aes(
            x = x,
            y = y,
            label = paste(c(addDataPointLabel()$text))
          ),
          size = as.numeric(input$dpview)
        )
   
  }
  
  output$MyBPlot <- renderPlot({
    if (nrow(grpdata$df) != 0) {
      plotAnnoteinput()
    }
    else{
      plotinput()
    }
  })
  output$MyBPlotFinal <- renderUI({
    shinycssloaders::withSpinner(
      plotOutput(
        "MyBPlot",
        width = input$width,
        height = input$height
      ),
      type = getOption("spinner.type", default = 6),
      color = getOption("spinner.color", default = "#BCE290"),
      size = getOption("spinner.size", default = 1.5)
    )
  })
  
  #Download BoxPlot as PNG
  output$downloadBPlot <- downloadHandler(
    filename = "Boxplot.png",
    content = function(file) {
      png(file)
      if (nrow(grpdata$df) != 0) {
        print(plotAnnoteinput())
      }
      else{
        print(plotinput())
      }
      dev.off()
    }
  )
  
  #Download Boxplot settings
  output$savesetting <- downloadHandler(
    filename = function() {
      paste("graph_setting_box-scatter-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(graphsettings(), file)
    }
  )
  
  ######################
  ###Output for stats###
  ######################
  
  ###Descriptive Statistics for Box plot###
  
  descStatCalc <- reactive({
    descTab <- data.frame()
    for (i in 1:ncol(data())) {
      descStat <- data.frame(as.matrix(summary(na.omit(data(
        
      )[, i]))))
      descStat <- rbind(length(data()[, i]), descStat)
      descSD <- apply(data()[i], 2, sd, na.rm = T)
      descErr <- descSD / sqrt(nrow(data()[i]))
      descStat <- rbind(descStat, descSD, descErr)
      colnames(descStat) <- c(colnames(data()[i]))
      descTab <- append(descTab, descStat)
    }
    descTab <- data.frame(descTab, check.names = F)
    rows <- data.frame(
      Parameter = c(
        'N',
        'Minimum',
        "1st Quartile",
        'Median',
        'Mean',
        '3rd Quartile',
        'Maximum',
        'Std. Dev.',
        'Std. Err.'
      )
    )
    descTab <- cbind(rows, descTab)
    rownames(descTab) <- descTab$Parameter
    descTab <- descTab[, 2:ncol(descTab)]
  })
  
  output$descTable <- renderTable(descStatCalc(), rownames = T)
  output$descUI <- renderUI('descTable')
  
  #Descriptive test report download
  output$descDnld <- downloadHandler(
    filename = function() {
      paste("Descriptive Stat report-", Sys.Date(), "csv", sep = ".")
    },
    content = function(file) {
      write.csv(descStatCalc(), file)
    }
  )
  
  #Descriptive test report display
  output$descsStatOut <- renderUI({
    shinyBS::bsModal(
      'descModal',
      title = "Result of Descriptive Statistics",
      'descstat',
      tableOutput('descTable'),
      downloadButton('descDnld', 'Download Report', style = "margin-top:10px;")
    )
  })
  
  ###Descriptive Statistics for Violin Plot###
  
  descStatCalcV <- reactive({
    descTab <- data.frame()
    for (i in 1:ncol(data())) {
      descStat <- data.frame(as.matrix(summary(na.omit(data(
        
      )[, i]))))
      descStat <- rbind(length(data()[, i]), descStat)
      descSD <- apply(data()[i], 2, sd, na.rm = T)
      descErr <- descSD / sqrt(nrow(data()[i]))
      descStat <- rbind(descStat, descSD, descErr)
      colnames(descStat) <- c(colnames(data()[i]))
      descTab <- append(descTab, descStat)
    }
    descTab <- data.frame(descTab, check.names = F)
    rows <- data.frame(
      Parameter = c(
        'N',
        'Minimum',
        "1st Quartile",
        'Median',
        'Mean',
        '3rd Quartile',
        'Maximum',
        'Std. Dev.',
        'Std. Err.'
      )
    )
    descTab <- cbind(rows, descTab)
    rownames(descTab) <- descTab$Parameter
    descTab <- descTab[, 2:ncol(descTab)]
  })
  
  
  # output$descUIV <- renderUI('descTableV')
  
  #Descriptive test report download
  output$descDnldV <- downloadHandler(
    filename = function() {
      paste("Descriptive Stat report-", Sys.Date(), "csv", sep = ".")
    },
    content = function(file) {
      write.csv(descStatCalcV(), file)
    }
  )
  
  #Descriptive test report display
  # output$descsStatOutV <- renderUI({
  #   shinyBS::bsModal(
  #     'descModalV',
  #     title = "Result of Descriptive Statistics",
  #     'descstatV',
  #     tableOutput('descTableV'),
  #     downloadButton('descDnldV', 'Download Report', style = "margin-top:10px;")
  #   )
  # })
  observeEvent(input$descstatV,{
    output$stattableout <- renderUI({
      tagList(
        h2("Result of Descriptive statistics"),
        ## Normality test report display
        output$descTableV <- renderTable(descStatCalcV(), rownames = T),
        downloadButton('descDnldV', 'Download Report', style = "margin-top:10px;")
      )
    })
  })
  
  ###Normality test for Box Plot###
  
  #generation of Normality test report
  SWtestdnld <- reactive({
    normdf <- data.frame()
    dataN <- na.omit(data())
    if (nrow(dataN) > 3000 || nrow(dataN) < 3) {
      data.frame(
        Test_Report = c(
          'Can not perform Shapiro test for sample number less than 3 or more than 3000.'
        )
      )
    } else{
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
        if (testrep[i, 2] < 0.05) {
          tempdf2 <- paste0('No')
          df <- rbind(df, tempdf2)
        } else if (testrep[i, 2] > 0.05) {
          tempdf2 <- paste0("Yes")
          df <- rbind(df, tempdf2)
        }
      }
      testrep <- cbind(testrep, df)
      testrep <- data.frame(testrep[, 4], testrep[1], testrep[2], testrep[5])
      colnames(testrep) <- c(
        'Condition',
        'Shapiro-Wilk Statistics',
        'p.Value',
        'Passed normality test (p<0.05)?'
      )
      testrep <- testrep
    }
  })
  
  #Normality test report download
  output$dnldswtest <- downloadHandler(
    #Normality test report download
    filename = function() {
      paste("normality_test_report-", Sys.Date(), "csv", sep = ".")
    },
    content = function(file) {
      write.csv(SWtestdnld(), file)
    }
  )
  
  ## Normality test report display
  output$SWtest <- renderTable({
    format(SWtestdnld(), nsmall = 5)
  })
  
  output$normalityTest <- renderUI({
    shinyBS::bsModal(
      'swtestbox',
      title = "Result of Shapiro-Wilk Test",
      trigger = 'swtestsubmit',
      size = 'large',
      tableOutput('SWtest'),
      downloadButton("dnldswtest", "Download Report", style = "margin-top:10px;")
    )
  })
  
  ###Normality test for Violin Plot###
  
  SWtestdnldV <- reactive({
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
        if (testrep[i, 2] < 0.05) {
          tempdf2 <- paste0('No')
          df <- rbind(df, tempdf2)
        } else if (testrep[i, 2] > 0.05) {
          tempdf2 <- paste0("Yes")
          df <- rbind(df, tempdf2)
        }
      }
      testrep <- cbind(testrep, df)
      testrep <- data.frame(testrep[, 4], testrep[1], testrep[2], testrep[5])
      colnames(testrep) <- c(
        'Condition',
        'Shapiro-Wilk Statistics',
        'p.Value',
        'Passed normality test (p<0.05)?'
      )
      testrep <- testrep
    }
  })
  
  #Normality test report download
  output$dnldswtestV <- downloadHandler(
    #Normality test report download
    filename = function() {
      paste("normality_test_report-", Sys.Date(), "csv", sep = ".")
    },
    content = function(file) {
      write.csv(SWtestdnldV(), file)
    }
  )
  
  
 
  
    # shinyBS::bsModal(
    #   'swtestvio',
    #   title = "Result of Shapiro-Wilk Test",
    #   trigger = 'swtestsubmitV',
    #   size = 'large',
    #   tableOutput('SWtestV'),
    #   downloadButton("dnldswtestV", "Download Report", style = "margin-top:10px;")
    # )
    observeEvent(input$swtestsubmitV,{
      output$stattableout <- renderUI({
        tagList(
          h2("Result of Shapiro-Wilk Test"),
        ## Normality test report display
        output$SWtestV <- renderTable({
          format(SWtestdnldV(), nsmall = 5)
        }),
        downloadButton("dnldswtestV", "Download Report", style = "margin-top:10px;")
        )
    })
  })
  
  
  
  ###Significance test###
  
  #Asking for significance test (box plot)
  
  testcheck <- reactive({
    if (ncol(data()) > 2) {
      list(tags$span("Pairwise Comparison"),
           tags$span(
             tags$span("Groupwise Comparison"),
             tags$span("Suggested", class = "suggested", title = 'As there are multiple groups, it is recommended to use a groupwise comparison (ANNOVA or Kruskal Wallis test)')
           ))
    } else if (ncol(data()) <= 2) {
      list(tags$span(
        tags$span("Pairwise Comparison"),
        tags$span("Suggested", class = "suggested", title = "As there are only two groups, it is recommended to use a pairwise comparison (Welch's-t or Mann-Whitney U test)")
      ),
      tags$span("Groupwise Comparison"))
    }
  })
  
  normcheck <- reactive({
    if (has_element(SWtestdnld(), 'No') == T) {
      list(tags$span("Parametric Test"),
           tags$span(
             tags$span("Nonparametric Test"),
             tags$span("Suggested", class = "suggested", title = 'One group or more shows skewed distribution. It is recommended to use a nonparametric test (Mann-Whitney U or Kruskal Wallis test)')
           ))
    } else if (has_element(SWtestdnld(), 'No') == F) {
      list(tags$span(
        tags$span("Parametric Test"),
        tags$span("Suggested", class = "suggested", title = "All the groups are distributed normally. It is recommended to use a parametric test (Welch's-t or ANNOVA test)")
      ),
      tags$span("Nonparametric Test"))
    }
  })
  output$comptestshow1 <- renderUI({
    if (input$comptest == T) {
      radioButtons(
        "comptestA",
        label = "Choose parameters to perform significance test:",
        choiceValues = c('para', 'nonpara'),
        choiceNames = normcheck()
      )
    }
  })
  output$comptestshow2 <- renderUI({
    if (input$comptest == T) {
      radioButtons(
        "comptestB",
        label = "",
        choiceValues = c('pair', 'group'),
        choiceNames = testcheck()
      )
    }
  })
  output$comptestBtn <- renderUI({
    if (input$comptest == T) {
      actionButton("comptestrun", "Run Test", style = "margin-bottom:10px;")
    }
  })
  
  #Asking for significance test (violin plot)
  
  testcheckV <- reactive({
    if (ncol(data()) > 2) {
      list(tags$span("Pairwise Comparison"),
           tags$span(
             tags$span("Groupwise Comparison"),
             tags$span("Suggested", class = "suggested", title = 'As there are multiple groups, it is recommended to use a groupwise comparison (ANNOVA or Kruskal Wallis test)')
           ))
    } else if (ncol(data()) <= 2) {
      list(tags$span(
        tags$span("Pairwise Comparison"),
        tags$span("Suggested", class = "suggested", title = "As there are only two groups, it is recommended to use a pairwise comparison (Welch's-t or Mann-Whitney U test)")
      ),
      tags$span("Groupwise Comparison"))
    }
  })
  
  normcheckV <- reactive({
    if (has_element(SWtestdnldV(), 'No') == F) {
      list(tags$span("Parametric Test"),
           tags$span(
             tags$span("Nonparametric Test"),
             tags$span("Suggested", class = "suggested", title = 'One group or more shows skewed distribution. It is recommended to use a nonparametric test (Mann-Whitney U or Kruskal Wallis test)')
           ))
    } else if (has_element(SWtestdnldV(), 'No') == T) {
      list(tags$span(
        tags$span("Parametric Test"),
        tags$span("Suggested", class = "suggested", title = "All the groups are distributed normally. It is recommended to use a parametric test (Welch's-t or ANNOVA test)")
      ),
      tags$span("Nonparametric Test"))
    }
  })
  
  
  output$comptestshow1V <- renderUI({
    if (input$comptestV == T) {
      radioButtons(
        "comptestAV",
        label = "Choose parameters to perform significance test:",
        choiceValues = c('para', 'nonpara'),
        choiceNames = normcheckV()
      )
    }
  })
  output$comptestshow2V <- renderUI({
    if (input$comptestV == T) {
      radioButtons(
        "comptestBV",
        label = "",
        choiceValues = c('pair', 'group'),
        choiceNames = testcheckV()
      )
    }
  })
  output$comptestBtnV <- renderUI({
    if (input$comptestV == T) {
      actionButton("comptestrunV", "Run Test", style = "margin-bottom:10px;")
    }
  })
  
  observeEvent(input$swtestsubmit, {
    if (length(input$file1) == 0) {
      showNotification("Please upload your data first",
                       type = "error",
                       duration = 2)
    }
  })
  observeEvent(input$swtestsubmitV, {
    if (length(input$file1) == 0) {
      showNotification("Please upload your data first",
                       type = "error",
                       duration = 2)
    }
  })
  observeEvent(input$comptestrun, {
    if (length(input$file1) == 0) {
      showNotification("Please upload your data first",
                       type = "error",
                       duration = 2)
    }
  })
  observeEvent(input$comptestrunV, {
    if (length(input$file1) == 0) {
      showNotification("Please upload your data first",
                       type = "error",
                       duration = 2)
    }
  })
  
  ## JS- To initiate RUN TEST Modal box notification
  output$mixTable <- renderUI({
    tagList(tableOutput('sigtable'), br(), sgrepdnbt())
    # br(),
    # posthocask()
  })
  # output$mixTableV <- renderUI({
  #   tagList(tableOutput('sigtableV'), br(), sgrepdnbtV())
  #   # br(),
  #   # posthocaskV()
  # })
  output$tableModal <- renderUI({
    if (input$comptest == T) {
      if (input$comptestB == "group") {
        shinyBS::bsModal("tableout1",
                sigtestTitle(),
                "comptestrun",
                size = "large",
                uiOutput("mixTable"))
      }
      else if (input$comptestB == "pair") {
        shinyBS::bsModal("tableout2",
                PairTestTitl(),
                "comptestrun",
                size = "large",
                uiOutput("modalout"))
      }
    }
  })
  observeEvent(input$comptestrunV,{
    output$stattableout <- renderUI({
      if (input$comptestV == T) {
        if (input$comptestBV == "group") {
            tagList(
              h2(sigtestTitleV()),
              output$sigtableV <- renderTable({
                format(sigtestInpV(), nsmall = 5)
              }),
              sgrepdnbtV()
            )
          
          
          # shinyBS::bsModal(
          #   "tableout1V",
          #   sigtestTitleV(),
          #   "comptestrunV",
          #   size = "large",
          #   uiOutput("mixTableV")
          # )
        }
        else if (input$comptestBV == "pair") {
            tagList(
            h2(PairTestTitlV()),
            uiOutput("modaloutV")
          )
          # shinyBS::bsModal(
          #   "tableout2V",
          #   PairTestTitlV(),
          #   "comptestrunV",
          #   size = "large",
          #   uiOutput("modaloutV")
          # )
        }
      }
    })
  })

  
  #Pairwise Modal box output
  groups <- reactive({
    combn(colnames(data()), 2)
  })
  
  # Reactive expression to gather checkbox states
  checkbox_states <- reactive({
    sapply(1:ncol(groups()), function(i) {
      input[[paste0("group", i)]]
    })
  })
  
  # Render the action button only when at least one checkbox is selected
  output$submitComp <- renderUI({
    if (any(checkbox_states() == TRUE)) {
      actionButton('compTabInp', "Submit Selected", style = "margin-top:10px;")
    } else {
      div(
        'Please select at least one pair',
        style = 'padding:10px;
      background-color:#fabaac; font-weight:bolder;margin-top:10px;
'
      )
    }
  })
  output$submitCompV <- renderUI({
    if (any(checkbox_states() == TRUE)) {
      actionButton('compTabInpV', "Submit Selected", style = "margin-top:10px;")
    } else {
      div(
        'Please select at least one pair',
        style = 'padding:10px;
      background-color:#fabaac; font-weight:bolder;margin-top:10px;
'
      )
    }
  })
  
  
  output$modalout <- renderUI({
    tagList(
      # tableOutput("table"),
      # selectInput("input1", "Select Condition 1", choices = c(colnames(data()))),
      # selectInput("input2", "Select Condition 2", choices = c(colnames(data()))),
      h4('Choose groups to test for statistical significance'),
      div(lapply(1:ncol(groups(
        
      )), function(i) {
        comparison_label <- paste0(groups()[1, i], " vs ", groups()[2, i])
        checkboxInput(
          inputId = paste0("group", i),
          label = comparison_label,
          value = FALSE
        )
      }), style = "background-color:#efefef; padding:10px;"),
      # actionButton("submit", "Select"),
      # actionButton('reset', "Reset Table"),
      uiOutput('submitComp'),
      tableOutput("SigPairOut"),
      uiOutput("ptRepOut")
    )
    
  })
  
  
  output$modaloutV <- renderUI({
    tagList(
      # tableOutput("table"),
      # selectInput("input1", "Select Condition 1", choices = c(colnames(data()))),
      # selectInput("input2", "Select Condition 2", choices = c(colnames(data()))),
      h4('Choose groups to test for statistical significance'),
      div(lapply(1:ncol(groups(
        
      )), function(i) {
        comparison_label <- paste0(groups()[1, i], " vs ", groups()[2, i])
        checkboxInput(
          inputId = paste0("group", i),
          label = comparison_label,
          value = FALSE
        )
      }), style = "background-color:#efefef; padding:10px;overflow-y:scroll;max-height:300px;"),
      # actionButton("submit", "Select"),
      # actionButton('reset', "Reset Table"),
      uiOutput('submitCompV'),
      tableOutput("SigPairOutV"),
      uiOutput("ptRepOutV")
    )
    
  })
  #Data preparation from user input for pairwise calculation
  #for box plot
  userdata <- reactiveValues(df = data.frame(Condition1 = character(), Condition2 = character()))
  observeEvent(input$compTabInp, {
    #For box plot
    if (length(groups()) == 0) {
      showNotification('Please select at least one pair.', type = c("warning"))
    } else{
      selected <- lapply(1:ncol(groups()), function(i) {
        if (input[[paste0("group", i)]]) {
          paste0(groups()[1, i], " vs ", groups()[2, i])
        } else {
          NULL
        }
      })
      selected <- selected[!sapply(selected, is.null)]
      
      selected_df <- do.call(rbind, lapply(selected, function(comp) {
        groups <- unlist(strsplit(comp, " vs "))
        data.frame(
          Group1 = groups[1],
          Group2 = groups[2],
          stringsAsFactors = FALSE
        )
      }))
      userdata$df <- (selected_df)
    }
    
  })
  observeEvent(input$comptestrun, {
    output$statdnld <- renderUI({
      downloadButton('reportdnld', 'Download Complete Stat Report')
    })
  })
  #For Violin plot
  
  userdataV <- reactiveValues(df = data.frame(Condition1 = character(), Condition2 = character()))
  observeEvent(input$compTabInpV, {
    #For box plot
    if (length(groups()) == 0) {
      showNotification('Please select at least one pair.', type = c("warning"))
    } else{
      selected <- lapply(1:ncol(groups()), function(i) {
        if (input[[paste0("group", i)]]) {
          paste0(groups()[1, i], " vs ", groups()[2, i])
        } else {
          NULL
        }
      })
      selected <- selected[!sapply(selected, is.null)]
      
      selected_df <- do.call(rbind, lapply(selected, function(comp) {
        groups <- unlist(strsplit(comp, " vs "))
        data.frame(
          Group1 = groups[1],
          Group2 = groups[2],
          stringsAsFactors = FALSE
        )
      }))
      userdataV$df <- (selected_df)
    }
    
  })
  observeEvent(input$comptestrunV, {
    output$statdnldV <- renderUI({
      downloadButton('reportdnldV', 'Download Complete Stat Report')
    })
  })
  
  ##calculation of pairwise Tests##
  
  PairTestTitl <- reactive({
    if (input$comptestA == 'para') {
      #for box plot || for vio Plot
      boxTitle <- paste("Result of Welch's t- Test")
    }
    else if (input$comptestA == 'nonpara') {
      #for box plot || for vio plot
      boxTitle <- paste("Result of Wilcoxon-Mann-Whitney Test")
    }
    return(boxTitle)
  })
  
  PairTestTitlV <- reactive({
    if (input$comptestAV == 'para') {
      #for box plot || for vio Plot
      boxTitle <- paste("Result of Welch's t- Test")
    }
    else if (input$comptestAV == 'nonpara') {
      #for box plot || for vio plot
      boxTitle <- paste("Result of Wilcoxon-Mann-Whitney Test")
    }
    return(boxTitle)
  })
  
  PairTestCal <- eventReactive(input$compTabInp, {
    #for box plot
    
    #Function to get Z-statistics from Mann Whitney U test as
    #the current package doesn't provide one
    
    mwz <- function(x, y) {
      xy <- data.frame(x, y)
      xy_long <- tidyr::pivot_longer(
        xy,
        names_to = 'para',
        values_to = 'val',
        cols = colnames(xy)
      )
      xy_order <- dplyr::arrange(xy_long, val)
      xy_rank <- data.frame(xy_order, rownames(xy_order), row.names = NULL)
      colnames(xy_rank) <- c('para', 'val', 'rank')
      xy_reorder <- dplyr::arrange(xy_rank, para)
      xy_wide <- pivot_wider(xy_reorder,
                             names_from = para,
                             values_from = c(val, rank))
      ranksumx <- sum(as.numeric(unlist(xy_wide$rank_x)))
      rankmeanx <- mean(as.numeric(unlist(xy_wide$rank_x)))
      ranksumy <- sum(as.numeric(unlist(xy_wide$rank_y)))
      rankmeany <- mean(as.numeric(unlist(xy_wide$rank_y)))
      
      rankUx <- (length(x) * length(y)) + (((length(x) + 1) * length(x)) /
                                             2) - ranksumx
      rankUy <- (length(x) * length(y)) + (((length(y) + 1) * length(y)) /
                                             2) - ranksumy
      
      U_wert <- min(rankUx, rankUy)
      expU <- (length(x) * length(y)) / 2
      stdErrU <- sqrt((length(x) * length(y)) * (length(x) + length(y) +
                                                   1) / 12)
      z <- (U_wert - expU) / stdErrU
      print(abs(z))
    }
    #Testing the significant difference
    testdata <- userdata$df
    if (nrow(testdata) == 0) {
      showNotification("Please select the conditions first.", type = "warning")
    }
    else{
      if (input$comptestA == 'para' && input$comptestB == 'pair') {
        colname <- as.factor(colnames(data()))
        dfNew <- data.frame()
        for (i in 1:nrow(testdata)) {
          # conditionx <- gsub("[[:punct:]]",'',testdata[i,1])
          # conditiony <- gsub("[[:punct:]]",'',testdata[i,2])
          x <- match(as.factor(testdata[i, 1]), colname)
          y <- match(as.factor(testdata[i, 2]), colname)
          testrow <- t.test(data()[, x], data()[, y])
          testrow <- tidy(testrow)
          dfNew <- rbind(dfNew, testrow)
        }
        # colnames(dfNew) <- c("Difference in Means", "Mean of Condition1", "Mean of Condition2", "t Statistics",
        #                      "p_value", "Parameter", "Lower Level of 95% CI", "Upper Level of 95% CI", "Method", "Alternative")
        dffinal <- data.frame(dfNew[4], dfNew[6], dfNew[5], dfNew[, 1], dfNew[, 7], dfNew[, 8], dfNew[, 10])
        dffinal <- unite(dffinal, tempCol, c(conf.low, conf.high), sep =
                           ' to ')
        dffinal <- cbind(testdata, dffinal)
        colnames(dffinal) <- c(
          'Group1',
          'Group2',
          't Statistic',
          'DF',
          'p Value',
          'Diff in means',
          '95% Confidence intervals',
          'Method'
        )
        row.names(dffinal) <- NULL
        dffinal <- dffinal
      }
      
      else if (input$comptestA == 'nonpara' &&
               input$comptestB == 'pair') {
        # colname <- gsub("[[:punct:]]",'',colnames(data()))
        colname <- as.factor(colnames(data()))
        dfnew <- data.frame()
        for (i in 1:nrow(testdata)) {
          # conditionx <- gsub("[[:punct:]]",'',testdata[i,1])
          # conditiony <- gsub("[[:punct:]]",'',testdata[i,2])
          x <- match(as.factor(testdata[i, 1]), colname)
          y <- match(as.factor(testdata[i, 2]), colname)
          testrow <- wilcox.test(data()[, x], data()[, y])
          testrow <- data.frame(tidy(testrow))
          zVal <- mwz(data()[, x], data()[, y])
          testrow <- cbind(zVal, testrow)
          dfnew <- rbind(dfnew, testrow)
        }
        
        dffinal <- data.frame(dfnew[, 1], dfnew[, 3], dfnew[, 5])
        dffinal <- cbind(testdata, dffinal)
        colnames(dffinal) <- c('Group1', 'Group2', 'Z-Statistic', 'p Value', 'Method')
        row.names(dffinal) <- NULL
        dffinal <- dffinal
      }
    }
  })
  
  PairTestCalV <- eventReactive(input$compTabInpV, {
    #For vio plot
    
    #Function to get Z-statistics from Mann Whitney U test as
    #the current package doesn't provide one
    
    mwz <- function(x, y) {
      xy <- data.frame(x, y)
      xy_long <- tidyr::pivot_longer(
        xy,
        names_to = 'para',
        values_to = 'val',
        cols = colnames(xy)
      )
      xy_order <- dplyr::arrange(xy_long, val)
      xy_rank <- data.frame(xy_order, rownames(xy_order), row.names = NULL)
      colnames(xy_rank) <- c('para', 'val', 'rank')
      xy_reorder <- dplyr::arrange(xy_rank, para)
      xy_wide <- pivot_wider(xy_reorder,
                             names_from = para,
                             values_from = c(val, rank))
      ranksumx <- sum(as.numeric(unlist(xy_wide$rank_x)))
      rankmeanx <- mean(as.numeric(unlist(xy_wide$rank_x)))
      ranksumy <- sum(as.numeric(unlist(xy_wide$rank_y)))
      rankmeany <- mean(as.numeric(unlist(xy_wide$rank_y)))
      
      rankUx <- (length(x) * length(y)) + (((length(x) + 1) * length(x)) /
                                             2) - ranksumx
      rankUy <- (length(x) * length(y)) + (((length(y) + 1) * length(y)) /
                                             2) - ranksumy
      
      U_wert <- min(rankUx, rankUy)
      expU <- (length(x) * length(y)) / 2
      stdErrU <- sqrt((length(x) * length(y)) * (length(x) + length(y) +
                                                   1) / 12)
      z <- (U_wert - expU) / stdErrU
      print(abs(z))
    }
    #Testing the significant difference
    testdata <- userdataV$df
    if (nrow(testdata) == 0) {
      showNotification("Please select the conditions first.", type = "warning")
    }
    else{
      if (input$comptestAV == 'para' && input$comptestBV == 'pair') {
        colname <- as.factor(colnames(data()))
        dfNew <- data.frame()
        for (i in 1:nrow(testdata)) {
          # conditionx <- gsub("[[:punct:]]",'',testdata[i,1])
          # conditiony <- gsub("[[:punct:]]",'',testdata[i,2])
          x <- match(as.factor(testdata[i, 1]), colname)
          y <- match(as.factor(testdata[i, 2]), colname)
          testrow <- t.test(data()[, x], data()[, y])
          testrow <- tidy(testrow)
          dfNew <- rbind(dfNew, testrow)
        }
        # colnames(dfNew) <- c("Difference in Means", "Mean of Condition1", "Mean of Condition2", "t Statistics",
        #                      "p_value", "Parameter", "Lower Level of 95% CI", "Upper Level of 95% CI", "Method", "Alternative")
        dffinal <- data.frame(dfNew[4], dfNew[6], dfNew[5], dfNew[, 1], dfNew[, 7], dfNew[, 8], dfNew[, 10])
        dffinal <- unite(dffinal, tempCol, c(conf.low, conf.high), sep =
                           ' to ')
        dffinal <- cbind(testdata, dffinal)
        colnames(dffinal) <- c(
          'Group1',
          'Group2',
          't Statistic',
          'DF',
          'p Value',
          'Diff in means',
          '95% Confidence intervals',
          'Method'
        )
        row.names(dffinal) <- NULL
        dffinal <- dffinal
      }
      
      else if (input$comptestAV == 'nonpara' &&
               input$comptestBV == 'pair') {
        # colname <- gsub("[[:punct:]]",'',colnames(data()))
        colname <- as.factor(colnames(data()))
        dfnew <- data.frame()
        for (i in 1:nrow(testdata)) {
          # conditionx <- gsub("[[:punct:]]",'',testdata[i,1])
          # conditiony <- gsub("[[:punct:]]",'',testdata[i,2])
          x <- match(as.factor(testdata[i, 1]), colname)
          y <- match(as.factor(testdata[i, 2]), colname)
          testrow <- wilcox.test(data()[, x], data()[, y])
          testrow <- data.frame(tidy(testrow))
          zVal <- mwz(data()[, x], data()[, y])
          testrow <- cbind(zVal, testrow)
          dfnew <- rbind(dfnew, testrow)
        }
        
        dffinal <- data.frame(dfnew[, 1], dfnew[, 3], dfnew[, 5])
        dffinal <- cbind(testdata, dffinal)
        colnames(dffinal) <- c('Group1', 'Group2', 'Z-Statistic', 'p Value', 'Method')
        row.names(dffinal) <- NULL
        dffinal <- dffinal
      }
    }
  })
  
  
  
  ##Pairwise test output##
  observeEvent(input$compTabInp, {
    #for box plot
    if (nrow(userdata$df) == 0) {
      showNotification("Please select the conditions first.", type = "warning")
      output$SigPairOut <- renderTable(data.frame())
    } else if (nrow(userdata$df != 0)) {
      tagList(
        output$SigPairOut <- renderTable(format(PairTestCal(), nsmall = 5)),
        
        output$ptRepOut <- renderUI(pairTestDnld())
        
      )
    }
  })
  
  observeEvent(input$compTabInpV, {
    #for vio plot
    if (nrow(userdataV$df) == 0) {
      showNotification("Please select the conditions first.", type = "warning")
      output$SigPairOutV <- renderTable(data.frame())
    } else if (nrow(userdataV$df != 0)) {
      tagList(
        output$SigPairOutV <- renderTable(format(PairTestCalV(), nsmall = 5)),
        
        output$ptRepOutV <- renderUI(pairTestDnldV())
        
      )
    }
  })
  
  pairTestDnld <- eventReactive(input$compTabInp, {
    if (nrow(userdata$df) == 0) {
      paste("")
    } else{
      #Download Button for downloading pairwise comparison report
      downloadButton("pairtestbtn", label = "Download Report", style = "margin-top:10px;")
    }
  })
  
  pairTestDnldV <- eventReactive(input$compTabInpV, {
    if (nrow(userdataV$df) == 0) {
      paste("")
    } else{
      #Download Button for downloading pairwise comparison report
      downloadButton("pairtestbtnV", label = "Download Report", style =
                       "margin-top:10px;")
    }
  })
  
  #Server function of the download handler for pairwise comparison report
  output$pairtestbtn <- downloadHandler(
    #box plot
    filename = function() {
      paste("Significance_test_report-", Sys.Date(), "csv", sep = ".")
    },
    content = function(file) {
      write.csv(PairTestCal(), file, row.names = F)
    }
  )
  
  output$pairtestbtnV <- downloadHandler(
    #violin plot
    filename = function() {
      paste("Significance_test_report-", Sys.Date(), "csv", sep = ".")
    },
    content = function(file) {
      write.csv(PairTestCalV(), file, row.names = F)
    }
  )
  
  #Preparation of the download button
  sgrepdnbt <- eventReactive(input$comptestrun, {
    downloadButton("sgdnld", "Download Report", style = "margin-top:10px;margin-bottom:10px;")
  })
  sgrepdnbtV <- eventReactive(input$comptestrunV, {
    downloadButton("sgdnldV", "Download Report", style = "margin-top:10px;margin-bottom:10px;")
  })
  
  #Levene's test for equal variance for full stat report
  levTest <- reactive({
    levdf <- data.frame(tidy(car::leveneTest(value ~ variable, na.omit(orderdata(
      
    )))))
    if (levdf$p.value < 0.05)
      (temp <- c(
        'The groups do not have equal variances on the dependent variable'
      ))
    else{
      temp <- c('The groups have approximately equal variances on the dependent variable')
    }
    levdf <- data.frame(levdf[, 1], levdf[, 2], levdf[, 3], temp)
    colnames(levdf) <- c('Statistics', 'p Value', 'DF', 'Remarks')
    return(levdf)
  })
  
  ###########################################################
  
  ###Calculation for group wise significance tests###
  
  
  ##For Box plot##
  sigtestInp <- reactive({
    if (input$comptestA == 'nonpara' && input$comptestB == 'group') {
      ##Kruskal Wallis calculation##
      kwrep <- kruskal.test(data())
      kwrepTab <- data.frame(kwrep$statistic, kwrep$parameter, kwrep$p.value)
      colnames(kwrepTab) <- c('Kruskal-Wallis chi-squared', 'DF', 'p Value')
      if (kwrep$p.value < 0.05) {
        sgreps <- append(kwrepTab,
                         c(
                           'There was a significant difference between sample medians'
                         ))
        sgreps <- data.frame(sgreps)
        colnames(sgreps) <- c('Kruskal-Wallis chi-squared',
                              'DF',
                              'p Value',
                              'Remarks')
      } else if (kwrep$p.value >= 0.05) {
        sgreps <- append(kwrepTab,
                         c('No significant difference found between sample medians'))
        sgreps <- data.frame(sgreps)
        colnames(sgreps) <- c('Kruskal-Wallis chi-squared',
                              'DF',
                              'p Value',
                              'Remarks')
      } else{
        
      }
    }
    else if (input$comptestA == 'para' &&
             input$comptestB == 'group') {
      ##ANNOVA test calculation##
      varT <- car::leveneTest(value ~ variable, na.omit(orderdata()))
      if (varT$`Pr(>F)`[1] < 0.05) {
        aovrep <- oneway.test(value ~ variable, na.omit(orderdata()))
      } else{
        aovrep <- oneway.test(value ~ variable, na.omit(orderdata()), var.equal = T)
      }
      aovrepTab <- data.frame(aovrep$statistic,
                              aovrep$parameter[1],
                              aovrep$parameter[2],
                              aovrep$p.value)
      colnames(aovrepTab) <- c('F statistic',
                               'DF between Groups',
                               'DF within groups',
                               'p Value')
      if (aovrep$p.value < 0.05 && varT$`Pr(>F)`[1] < 0.05) {
        sgreps <- append(
          aovrepTab,
          c(
            'There is a significant difference between two groups, while not assuming eql variance.'
          )
        )
        sgreps <- data.frame(sgreps)
        colnames(sgreps) <- c('F statistic',
                              'DF between Groups',
                              'DF within groups',
                              'p Value',
                              'Remarks')
      } else if (aovrep$p.value < 0.05 && varT$`Pr(>F)`[1] > 0.05) {
        sgreps <- append(
          aovrepTab,
          c(
            'There is a significant difference between two groups, while assuming eql variance.'
          )
        )
        sgreps <- data.frame(sgreps)
        colnames(sgreps) <- c('F statistic',
                              'DF between Groups',
                              'DF within groups',
                              'p Value',
                              'Remarks')
      } else if (aovrep$p.value >= 0.05 &&
                 varT$`Pr(>F)`[1] < 0.05) {
        sgreps <- append(
          aovrepTab,
          c(
            'There is no significant difference, while not assuming eql variance.'
          )
        )
        sgreps <- data.frame(sgreps)
        colnames(sgreps) <- c('F statistic',
                              'DF between Groups',
                              'DF within groups',
                              'p Value',
                              'Remarks')
      } else if (aovrep$p.value >= 0.05 &&
                 varT$`Pr(>F)`[1] > 0.05) {
        sgreps <- append(
          aovrepTab,
          c(
            'There is no significant difference, while assuming eql variance.'
          )
        )
        sgreps <- data.frame(sgreps)
        colnames(sgreps) <- c('F statistic',
                              'DF between Groups',
                              'DF within groups',
                              'p Value',
                              'Remarks')
      }
    }
    sgreps <- sgreps
  })
  output$sigtable <- renderTable({
    format(sigtestInp(), nsmall = 5)
  })
  
  ##For Violin plot##
  sigtestInpV <- reactive({
    if (input$comptestAV == 'nonpara' && input$comptestBV == 'group') {
      ##Kruskal Wallis calculation##
      kwrep <- kruskal.test(data())
      kwrepTab <- data.frame(kwrep$statistic, kwrep$parameter, kwrep$p.value)
      colnames(kwrepTab) <- c('Kruskal-Wallis chi-squared', 'DF', 'p Value')
      if (kwrep$p.value < 0.05) {
        sgreps <- append(kwrepTab,
                         c(
                           'There was a significant difference between sample medians'
                         ))
        sgreps <- data.frame(sgreps)
        colnames(sgreps) <- c('Kruskal-Wallis chi-squared',
                              'DF',
                              'p Value',
                              'Remarks')
      } else if (kwrep$p.value >= 0.05) {
        sgreps <- append(kwrepTab,
                         c('No significant difference found between sample medians'))
        sgreps <- data.frame(sgreps)
        colnames(sgreps) <- c('Kruskal-Wallis chi-squared',
                              'DF',
                              'p Value',
                              'Remarks')
      } else{
        
      }
    }
    else if (input$comptestAV == 'para' &&
             input$comptestBV == 'group') {
      ##ANNOVA test calculation##
      varT <- car::leveneTest(value ~ variable, na.omit(orderdata()))
      if (varT$`Pr(>F)`[1] < 0.05) {
        aovrep <- oneway.test(value ~ variable, na.omit(orderdata()))
      } else{
        aovrep <- oneway.test(value ~ variable, na.omit(orderdata()), var.equal = T)
      }
      aovrepTab <- data.frame(aovrep$statistic,
                              aovrep$parameter[1],
                              aovrep$parameter[2],
                              aovrep$p.value)
      colnames(aovrepTab) <- c('F statistic',
                               'DF between Groups',
                               'DF within groups',
                               'p Value')
      if (aovrep$p.value < 0.05 && varT$`Pr(>F)`[1] < 0.05) {
        sgreps <- append(
          aovrepTab,
          c(
            'There is a significant difference between two groups, while not assuming eql variance.'
          )
        )
        sgreps <- data.frame(sgreps)
        colnames(sgreps) <- c('F statistic',
                              'DF between Groups',
                              'DF within groups',
                              'p Value',
                              'Remarks')
      } else if (aovrep$p.value < 0.05 && varT$`Pr(>F)`[1] > 0.05) {
        sgreps <- append(
          aovrepTab,
          c(
            'There is a significant difference between two groups, while assuming eql variance.'
          )
        )
        sgreps <- data.frame(sgreps)
        colnames(sgreps) <- c('F statistic',
                              'DF between Groups',
                              'DF within groups',
                              'p Value',
                              'Remarks')
      } else if (aovrep$p.value >= 0.05 &&
                 varT$`Pr(>F)`[1] < 0.05) {
        sgreps <- append(
          aovrepTab,
          c(
            'There is no significant difference, while not assuming eql variance.'
          )
        )
        sgreps <- data.frame(sgreps)
        colnames(sgreps) <- c('F statistic',
                              'DF between Groups',
                              'DF within groups',
                              'p Value',
                              'Remarks')
      } else if (aovrep$p.value >= 0.05 &&
                 varT$`Pr(>F)`[1] > 0.05) {
        sgreps <- append(
          aovrepTab,
          c(
            'There is no significant difference, while assuming eql variance.'
          )
        )
        sgreps <- data.frame(sgreps)
        colnames(sgreps) <- c('F statistic',
                              'DF between Groups',
                              'DF within groups',
                              'p Value',
                              'Remarks')
      }
    }
    sgreps <- sgreps
  })

  
  
  sigtestTitle <- reactive({
    if (input$comptestA == 'nonpara') {
      #for box plot|| for vio plot
      sigtitl <- paste("Result of Kruskal-Wallis Test")
    } else if (input$comptestA == 'para') {
      #for box plot|| for vio plot
      sigtitl <- paste("Result of One way ANNOVA Test")
    }
    return(sigtitl)
  })
  sigtestTitleV <- reactive({
    if (input$comptestAV == 'nonpara') {
      #for box plot|| for vio plot
      sigtitl <- paste("Result of Kruskal-Wallis Test")
    } else if (input$comptestAV == 'para') {
      #for box plot|| for vio plot
      sigtitl <- paste("Result of One way ANNOVA Test")
    }
    return(sigtitl)
  })
  
  #Kruskal-Wallis Report Download#
  sigtestrep <- reactive({
    #preparation of the report
    if (input$comptestA == 'nonpara' &&
        input$comptestB == 'group') {
      sgtest <- as.data.frame.list(kruskal.test(data()), row.names = F)
    } else if (input$comptestA == 'para' &&
               input$comptestB == 'group') {
      owa <- as.data.frame.list(oneway.test(value ~ variable, na.omit(orderdata()), var.equal = F))
      owat <- as.data.frame.list(oneway.test(value ~ variable, na.omit(orderdata()), var.equal = T))
      owarep <- rbind(owa[2, ], owat[2, ])
      colnames(owarep) <- c("Welch F test statistics",
                            "df",
                            "p value",
                            "Method Used",
                            "Data")
      sgtest <- owarep[, 1:4]
    }
    sgtest <- sgtest
  })
  
  sigtestrepV <- reactive({
    #preparation of the report
    if (input$comptestAV == 'nonpara' &&
        input$comptestBV == 'group') {
      sgtest <- as.data.frame.list(kruskal.test(data()), row.names = F)
    } else if (input$comptestAV == 'para' &&
               input$comptestBV == 'group') {
      owa <- as.data.frame.list(oneway.test(value ~ variable, na.omit(orderdata()), var.equal = F))
      owat <- as.data.frame.list(oneway.test(value ~ variable, na.omit(orderdata()), var.equal = T))
      owarep <- rbind(owa[2, ], owat[2, ])
      colnames(owarep) <- c("Welch F test statistics",
                            "df",
                            "p value",
                            "Method Used",
                            "Data")
      sgtest <- owarep[, 1:4]
    }
    sgtest <- sgtest
  })
  
  #Server function of the download handler
  output$sgdnld <- downloadHandler(
    filename = function() {
      paste("Significance_test_report-", Sys.Date(), "csv", sep = ".")
    },
    content = function(file) {
      write.csv(sigtestrep(), file, row.names = F)
    }
  )
  output$sgdnldV <- downloadHandler(
    filename = function() {
      paste("Significance_test_report-", Sys.Date(), "csv", sep = ".")
    },
    content = function(file) {
      write.csv(sigtestrepV(), file, row.names = F)
    }
  )
  
  
  ##Post Hoc Analysis
  
  posthoctitleInp <- reactive({
    #for box plot
    if (input$comptestA == 'nonpara' &&
        input$comptestB == 'group') {
      sigtitl <- paste("Post Hoc Test: Dunn's Method")
    } else if (input$comptestA == 'para' &&
               input$comptestB == 'group') {
      sigtitl <- paste("Post Hoc Test: Tukey's HSD Test")
    }
    sigtitl <- sigtitl
  })
  posthoctitleInpV <- reactive({
    #for vio plot
    if (input$comptestAV == 'nonpara' &&
        input$comptestBV == 'group') {
      sigtitl <- paste("Post Hoc Test: Dunn's Method")
    } else if (input$comptestAV == 'para' &&
               input$comptestBV == 'group') {
      sigtitl <- paste("Post Hoc Test: Tukey's HSD Test")
    }
    sigtitl <- sigtitl
  })
  
  posthoclistInp <- reactive({
    #for box plot
    if (input$comptestA == 'nonpara' &&
        input$comptestB == 'group') {
      sigtitl <- c(
        "No Correction" = 'none',
        "Bonferroni" = 'bonferroni',
        "Sidak" = 'sidak',
        "Holm" = 'holm',
        "Holm-Sidak" = 'hs',
        "Hochberg" = 'hochberg',
        "Benjamini-Hochberg" = 'bh',
        "Benjamini-Yekutieli" = 'by'
      )
    } else if (input$comptestA == 'para' &&
               input$comptestB == 'group') {
      sigtitl <- c("95% Confidence Interval" = 0.95,
                   "99% Confidence Interval" = 0.99)
    }
    sigtitl <- sigtitl
  })
  posthoclistInpV <- reactive({
    #for bio plot
    if (input$comptestAV == 'nonpara' &&
        input$comptestBV == 'group') {
      sigtitl <- c(
        "No Correction" = 'none',
        "Bonferroni" = 'bonferroni',
        "Sidak" = 'sidak',
        "Holm" = 'holm',
        "Holm-Sidak" = 'hs',
        "Hochberg" = 'hochberg',
        "Benjamini-Hochberg" = 'bh',
        "Benjamini-Yekutieli" = 'by'
      )
    } else if (input$comptestAV == 'para' &&
               input$comptestBV == 'group') {
      sigtitl <- c("95% Confidence Interval" = 0.95,
                   "99% Confidence Interval" = 0.99)
    }
    sigtitl <- sigtitl
  })
  
  observeEvent(input$comptestrun, {
    output$posthoctitle <- renderText({
      if (input$comptest == T && input$comptestB == 'group') {
        posthoctitleInp()
      } else{
        
      }
    })
    output$posthocList <- renderUI({
      if (input$comptest == T && input$comptestB == 'group') {
        selectInput("methods", label = "Select a method to be used", choices = posthoclistInp())
      } else{
        
      }
    })
    
    #Displaying Post Hoc Analysis Result on Modal box
    output$posthocBtn <- renderUI({
      tagList(
        if (input$comptest == T && input$comptestB == 'group') {
          actionButton("runposthoc", "Run Post Hoc Test", style = 'margin-bottom:10px;')
        } else{
          
        },
        shinyBS::bsModal(
          "posthocModal",
          "Post Hoc Analysis",
          "runposthoc",
          size = "large",
          tableOutput("posthocTable"),
          downloadButton("downphtrprt", "Download Report")
        )
      )
      
    })
  })
  
  observeEvent(input$comptestrunV, {
    #For vio plot
    output$posthoctitleV <- renderText({
      if (input$comptestV == T && input$comptestBV == 'group') {
        posthoctitleInpV()
      } else{
        
      }
      
    })
    output$posthocListV <- renderUI({
      if (input$comptestV == T && input$comptestBV == 'group') {
        selectInput("methodsV", label = "Select a method to be used", choices = posthoclistInpV())
      } else{
        
      }
    })
    
    #Displaying Post Hoc Analysis Result on Modal box
    
    output$posthocBtnV <- renderUI({
      tagList(
        if (input$comptestV == T && input$comptestBV == 'group') {
          actionButton("runposthocV", "Run Post Hoc Test", style = 'margin-bottom:10px;')
        } else{
          
        }
        # shinyBS::bsModal(
        #   "posthocModalV",
        #   "Post Hoc Analysis",
        #   "runposthocV",
        #   size = "large",
        #   tableOutput("posthocTableV"),
        #   downloadButton("downphtrprtV", "Download Report")
        # )
      )
    })
  })
  observeEvent(input$runposthocV,{
    output$stattableout <- renderUI({
      tagList(
        h2("Post Hoc Analysis"),
        tableOutput("posthocTableV"),
        downloadButton("downphtrprtV", "Download Report")
      )
    })
  })
  ##Post Hoc test calculation and table preparation
  posthocinput <- eventReactive(input$methods, {
    userselect <- input$methods
  })
  posthocinputV <- eventReactive(input$methodsV, {
    userselect <- input$methodsV
  })
  posthoctest <- reactive({
    if (input$comptestA == 'nonpara' && input$comptestB == 'group') {
      dunntest <- dunn.test(
        x = na.omit(orderdata())$value,
        g = na.omit(orderdata())$variable,
        list = T,
        method = posthocinput()
      )
      dunntest <- as.data.frame(dunntest)
      newcol <- paste("Dunn's Test with ", posthocinput(), " correction")
      newcol <- as.data.frame.list(rep(newcol, nrow(dunntest)))
      newcol <- t(newcol)
      row.names(newcol) <- NULL
      dunntest <- cbind(dunntest, newcol)
      dunntest <- data.frame(dunntest[, 5],
                             dunntest[, 1],
                             dunntest[, 2],
                             dunntest[, 3],
                             dunntest[, 4],
                             dunntest[, 6])
      colnames(dunntest) <- c("Groups",
                              "Chi-squared",
                              "Z-score",
                              "p Value",
                              "Corrected p Value",
                              "Method")
      phtrep <- dunntest
    } else if (input$comptestA == 'para' &&
               input$comptestB == 'group') {
      model <- aov(na.omit(orderdata())$value ~ na.omit(orderdata())$variable)
      tuktest <- TukeyHSD(model, conf.level = as.numeric(posthocinput()))
      tuktest <- as.data.frame.list(tuktest)
      tuktest$row_names <- row.names(tuktest)
      colnames(tuktest) <- c(
        "Difference of Mean",
        "Lower Level of CI",
        "Upper Level of CI",
        "Corrected p value",
        "Comparisons"
      )
      tuktest <- unite(tuktest,
                       tempCol,
                       c(`Lower Level of CI`, `Upper Level of CI`),
                       sep = ' to ')
      tuktest <- data.frame(tuktest[, 4], tuktest[, 1], tuktest[, 2], tuktest[, 3])
      colnames(tuktest) <- c('Groups',
                             'Diff in means',
                             '95% Confidence intervals',
                             'Corrected p Value')
      phtrep <- tuktest
    }
    phtrep <- phtrep
  })
  posthoctestV <- reactive({
    if (input$comptestAV == 'nonpara' && input$comptestBV == 'group') {
      dunntest <- dunn.test(
        x = na.omit(orderdata())$value,
        g = na.omit(orderdata())$variable,
        list = T,
        method = posthocinputV()
      )
      dunntest <- as.data.frame(dunntest)
      newcol <- paste("Dunn's Test with ", posthocinputV(), " correction")
      newcol <- as.data.frame.list(rep(newcol, nrow(dunntest)))
      newcol <- t(newcol)
      row.names(newcol) <- NULL
      dunntest <- cbind(dunntest, newcol)
      dunntest <- data.frame(dunntest[, 5],
                             dunntest[, 1],
                             dunntest[, 2],
                             dunntest[, 3],
                             dunntest[, 4],
                             dunntest[, 6])
      colnames(dunntest) <- c("Groups",
                              "Chi-squared",
                              "Z-score",
                              "p Value",
                              "Corrected p Value",
                              "Method")
      phtrep <- dunntest
    } else if (input$comptestAV == 'para' &&
               input$comptestBV == 'group') {
      model <- aov(na.omit(orderdata())$value ~ na.omit(orderdata())$variable)
      tuktest <- TukeyHSD(model, conf.level = as.numeric(posthocinputV()))
      tuktest <- as.data.frame.list(tuktest)
      tuktest$row_names <- row.names(tuktest)
      colnames(tuktest) <- c(
        "Difference of Mean",
        "Lower Level of CI",
        "Upper Level of CI",
        "Corrected p value",
        "Comparisons"
      )
      tuktest <- unite(tuktest,
                       tempCol,
                       c(`Lower Level of CI`, `Upper Level of CI`),
                       sep = ' to ')
      tuktest <- data.frame(tuktest[, 4], tuktest[, 1], tuktest[, 2], tuktest[, 3])
      colnames(tuktest) <- c('Groups',
                             'Diff in means',
                             '95% Confidence intervals',
                             'Corrected p Value')
      phtrep <- tuktest
    }
    phtrep <- phtrep
  })
  
  output$posthocTable <- renderTable({
    format(posthoctest(), nsmall = 5)
  })
  output$posthocTableV <- renderTable({
    format(posthoctestV(), nsmall = 5)
  })
  
  #Post Hoc report download handler
  output$downphtrprt <- downloadHandler(
    #for box plot
    filename = function() {
      paste("Significance_Post-Hoc_test_report-",
            Sys.Date(),
            "csv",
            sep = ".")
    },
    content = function(file) {
      write.csv(posthoctest(), file, row.names = F)
    }
  )
  output$downphtrprtV <- downloadHandler(
    #For vio plot
    filename = function() {
      paste("Significance_Post-Hoc_test_report-",
            Sys.Date(),
            "csv",
            sep = ".")
    },
    content = function(file) {
      write.csv(posthoctestV(), file, row.names = F)
    }
  )
  
  ### Complete Stat report download for Box Plot
  # Reactive function to create the workbook and return the file path
  StatReport <- reactive({
    wb <- createWorkbook()
    sh <- createSheet(wb, 'Stat Report')
    row <- 1
    addDataFrame(
      data.frame(
        "Normality Test Report:" = double(),
        check.names = F
      ),
      sheet = sh,
      startRow = row,
      row.names = F
    )
    row <- row + 2
    addDataFrame(
      SWtestdnld(),
      sheet = sh,
      startRow = row,
      row.names = T
    )
    
    row <- row + nrow(SWtestdnld()) + 2
    addDataFrame(
      data.frame(
        "Descriptive Statistics Report:" = double(),
        check.names = F
      ),
      sheet = sh,
      startRow = row,
      row.names = F
    )
    
    row <- row + 2
    addDataFrame(
      descStatCalc(),
      sheet = sh,
      startRow = row,
      row.names = T
    )
    
    row <- row + nrow(descStatCalc()) + 2
    addDataFrame(
      data.frame(
        "Levene's Test for equal variance:" = double(),
        check.names = F
      ),
      sheet = sh,
      startRow = row,
      row.names = F
    )
    row <- row + 2
    addDataFrame(
      levTest(),
      sheet = sh,
      startRow =  row,
      row.names = F
    )
    
    row <- row + nrow(levTest()) + 2
    
    if (input$comptestB == 'pair') {
      addDataFrame(
        PairTestTitl(),
        sheet = sh,
        startRow = row,
        row.names = F
      )
    } else if (input$comptestB == 'group') {
      addDataFrame(
        sigtestTitle(),
        sheet = sh,
        startRow = row,
        row.names = F
      )
    }
    
    row <- row + 3
    if (input$comptestB == 'pair') {
      addDataFrame(
        PairTestCal(),
        sheet = sh,
        startRow = row,
        row.names = F
      )
    } else if (input$comptestB == 'group') {
      addDataFrame(
        sigtestInp(),
        sheet = sh,
        startRow = row,
        row.names = F
      )
      row <- row + nrow(sigtestInp()) + 2
      addDataFrame(
        posthoctitleInp(),
        sheet = sh,
        startRow = row,
        row.names = F
      )
      row <- row + 3
      addDataFrame(
        posthoctest(),
        sheet = sh,
        startRow = row,
        row.names = F
      )
      
    }
    
    # Save the workbook to a temporary file and return the file path
    tempFile <- tempfile(fileext = ".xlsx")
    saveWorkbook(wb, file = tempFile)
    return(tempFile)
  })
  
  # Download handler for comprehensive stat report
  output$reportdnld <- downloadHandler(
    filename = function() {
      paste("Statistic_Report_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Call the reactive to get the file path and copy it to the download location
      file.copy(StatReport(), file)
    }
  )
  
  ### Complete Stat report download for Violin Plot
  # Reactive function to create the workbook and return the file path
  StatReportV <- reactive({
    wb <- createWorkbook()
    sh <- createSheet(wb, 'Stat Report')
    row <- 1
    addDataFrame(
      data.frame(
        "Normality Test Report:" = double(),
        check.names = F
      ),
      sheet = sh,
      startRow = row,
      row.names = F
    )
    row <- row + 2
    addDataFrame(
      SWtestdnldV(),
      sheet = sh,
      startRow = row,
      row.names = T
    )
    
    row <- row + nrow(SWtestdnldV()) + 2
    addDataFrame(
      data.frame(
        "Descriptive Statistics Report:" = double(),
        check.names = F
      ),
      sheet = sh,
      startRow = row,
      row.names = F
    )
    
    row <- row + 2
    addDataFrame(
      descStatCalcV(),
      sheet = sh,
      startRow = row,
      row.names = T
    )
    
    row <- row + nrow(descStatCalcV()) + 2
    addDataFrame(
      data.frame(
        "Levene's Test for equal variance:" = double(),
        check.names = F
      ),
      sheet = sh,
      startRow = row,
      row.names = F
    )
    row <- row + 2
    addDataFrame(
      levTest(),
      sheet = sh,
      startRow =  row,
      row.names = F
    )
    
    row <- row + nrow(levTest()) + 2
    
    if (input$comptestBV == 'pair') {
      addDataFrame(
        PairTestTitlV(),
        sheet = sh,
        startRow = row,
        row.names = F
      )
    } else if (input$comptestBV == 'group') {
      addDataFrame(
        sigtestTitleV(),
        sheet = sh,
        startRow = row,
        row.names = F
      )
    }
    
    row <- row + 3
    if (input$comptestBV == 'pair') {
      addDataFrame(
        PairTestCalV(),
        sheet = sh,
        startRow = row,
        row.names = F
      )
    } else if (input$comptestBV == 'group') {
      addDataFrame(
        sigtestInpV(),
        sheet = sh,
        startRow = row,
        row.names = F
      )
      row <- row + nrow(sigtestInpV()) + 2
      addDataFrame(
        posthoctitleInpV(),
        sheet = sh,
        startRow = row,
        row.names = F
      )
      row <- row + 3
      addDataFrame(
        posthoctestV(),
        sheet = sh,
        startRow = row,
        row.names = F
      )
      
    }
    
    # Save the workbook to a temporary file and return the file path
    tempFile <- tempfile(fileext = ".xlsx")
    saveWorkbook(wb, file = tempFile)
    return(tempFile)
  })
  
  # Download handler for comprehensive stat report
  output$reportdnldV <- downloadHandler(
    filename = function() {
      paste("Statistic_Report_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Call the reactive to get the file path and copy it to the download location
      file.copy(StatReportV(), file)
    }
  )
  
  
  #################################################
  ##** Preparation for Annotation of the plotting #
  
  
  
  #Asking for adding significance bar
  
  observeEvent(input$compTabInp, {
    output$askAnnotation <- renderUI({
      tagList(
        uiOutput('statdnld'),
        radioButtons(
          "askBars",
          "Add significance annotations to the plot",
          choices = c('No' = 'no', 'Yes' = 'yes'),
          inline = T
        )
      )
      
    })
  })
  observeEvent(input$compTabInpV, {
    output$askAnnotationV <- renderUI({
      tagList(
        uiOutput('statdnldV'),
        radioButtons(
          "askBarsV",
          "Add significance annotations to the plot",
          choices = c('No' = 'no', 'Yes' = 'yes'),
          inline = T
        )
      )
      
    })
  })
  observeEvent(input$runposthocV, {
    # if (input$boxTab==T){
    #   output$askAnnotationV <- renderUI({
    #   tagList(
    #     uiOutput('statdnld'),
    #     br(),
    #     radioButtons(
    #       "askBars",
    #       "Add significance annotations to the plot",
    #       choices = c('No' = 'no', 'Yes' = 'yes'),
    #       inline = T
    #     )
    #   )
    # })
    # }else if (input$violinTab==T){
      output$askAnnotationV <- renderUI({
      tagList(
        uiOutput('statdnldV'),
        br(),
        radioButtons(
          "askBarsV",
          "Add significance annotations to the plot",
          choices = c('No' = 'no', 'Yes' = 'yes'),
          inline = T
        )
      )
      
    })
    # }
    
  })
  # observeEvent(input$runposthocV, {
  #   
  # })
  
  #Button to select groups
  
  comparisons <- reactive({
    # Generate list of unique pairwise comparisons
    groups <- combn(c(colnames(data())), 2)
  })
  comparisonsV <- reactive({
    # Generate list of unique pairwise comparisons
    groups <- combn(c(colnames(data())), 2)
  })
  
  
  output$chooseAnnotation <- renderUI({
    #for box plot
    if (input$askBars == 'yes' &&
        !is.null(input$askBars) && input$askBars != "") {
      tagList(
        h4('Choose the groups to compare'),
        div(lapply(1:ncol(
          comparisons()
        ), function(i) {
          comparison_label <- paste0(comparisons()[1, i], " vs ", comparisons()[2, i])
          checkboxInput(
            inputId = paste0("comparison", i),
            label = comparison_label,
            value = FALSE
          )
        }), style = 'padding:10px;border:1px solid;max-height:600px;overflow-Y:scroll;'),
        actionButton("submitAnnote", "Add Annotations", style = 'margin-top:10px;'),
        sliderInput(
          'annoteSize',
          "Select size of the text",
          min = 1,
          max = 10,
          value = 5
        ),
        sliderInput(
          'annoteDist',
          "Select distance between annotated-lines",
          min = 1,
          max = 15,
          value = 5
        ),
        radioButtons(
          "pvalshow",
          label = 'Show P Value',
          choiceNames = c('None', 'Only for ns', 'For all', 'Only P value'),
          choiceValues = c('none', 'ns', 'all', 'onlyP'),
          inline = T
        ),
        sliderInput(
          'pvaldist',
          'Adjust p value text position',
          min = -15,
          max = 15,
          value = -4
        ),
        sliderInput(
          'pvalfont',
          'Adjust p value text font size',
          min = 1,
          max = 15,
          value = 5
        ),
        shinyBS::bsModal(
          "annoteLines",
          "Select Groups for Annotation",
          "chooseGrps",
          uiOutput('grpModBox')
        ),
      )
    }
  })
  output$chooseAnnotationV <- renderUI({
    #For vio plot
    if (input$askBarsV == 'yes' &&
        !is.null(input$askBarsV) && input$askBarsV != "") {
      tagList(
        h4('Choose the groups to compare'),
        div(lapply(1:ncol(
          comparisonsV()
        ), function(i) {
          comparison_label <- paste0(comparisonsV()[1, i], " vs ", comparisonsV()[2, i])
          checkboxInput(
            inputId = paste0("comparisonV", i),
            label = comparison_label,
            value = FALSE
          )
        }), style = 'padding:10px;border:1px solid;max-height:600px;overflow-Y:scroll;'),
        actionButton("submitAnnoteV", "Add Annotations", style = 'margin-top:10px;'),
        sliderInput(
          'annoteSizeV',
          "Select size of the text",
          min = 1,
          max = 10,
          value = 5
        ),
        sliderInput(
          'annoteDistV',
          "Select distance between annotated-lines",
          min = 1,
          max = 15,
          value = 5
        ),
        radioButtons(
          "pvalshowV",
          label = 'Show P Value',
          choiceNames = c('None', 'Only for ns', 'For all', 'Only P value'),
          choiceValues = c('none', 'ns', 'all', 'onlyP'),
          inline = T
        ),
        sliderInput(
          'pvaldistV',
          'Adjust p value text position',
          min = -15,
          max = 15,
          value = -4
        ),
        sliderInput(
          'pvalfontV',
          'Adjust p value text font size',
          min = 1,
          max = 15,
          value = 5
        ),
        shinyBS::bsModal(
          "annoteLinesV",
          "Select Groups for Annotation",
          "chooseGrpsV",
          uiOutput('grpModBoxV')
        ),
      )
    }
  })
  
  
  grpdata <- reactiveValues(df = data.frame(Group1 = character(), Group2 = character()))
  grpdataV <- reactiveValues(df = data.frame(Group1 = character(), Group2 = character()))
  
  observeEvent(input$submitAnnote, {
    #For box plot
    selected <- lapply(1:ncol(comparisons()), function(i) {
      if (input[[paste0("comparison", i)]]) {
        paste0(comparisons()[1, i], " vs ", comparisons()[2, i])
      } else {
        NULL
      }
    })
    selected <- selected[!sapply(selected, is.null)]
    
    selected_df <- do.call(rbind, lapply(selected, function(comp) {
      groups <- unlist(strsplit(comp, " vs "))
      data.frame(
        Group1 = groups[1],
        Group2 = groups[2],
        stringsAsFactors = FALSE
      )
    }))
    grpdata$df <- (selected_df)
  })
  
  observeEvent(input$submitAnnoteV, {
    #For vio plot
    selected <- lapply(1:ncol(comparisonsV()), function(i) {
      if (input[[paste0("comparisonV", i)]]) {
        paste0(comparisonsV()[1, i], " vs ", comparisonsV()[2, i])
      } else {
        NULL
      }
    })
    selected <- selected[!sapply(selected, is.null)]
    
    selected_df <- do.call(rbind, lapply(selected, function(comp) {
      groups <- unlist(strsplit(comp, " vs "))
      data.frame(
        Group1 = groups[1],
        Group2 = groups[2],
        stringsAsFactors = FALSE
      )
    }))
    grpdataV$df <- (selected_df)
    
  })
  
  
  annotationCal <- eventReactive(input$submitAnnote, {
    #for box plot
    
    testdata <- grpdata$df
    #Calculation for Welch's t Test
    #Parametric pairwise comparison
    if (input$comptestA == 'para' && input$comptestB == 'pair') {
      colname <- as.factor(colnames(data()))
      dfNew <- data.frame()
      for (i in 1:nrow(testdata)) {
        # conditionx <- gsub("[[:punct:]]",'',testdata[i,1])
        # conditiony <- gsub("[[:punct:]]",'',testdata[i,2])
        x <- match(as.factor(testdata[i, 1]), colname)
        y <- match(as.factor(testdata[i, 2]), colname)
        testrow <- t.test(data()[, x], data()[, y])
        testrow <- tidy(testrow)
        dfNew <- rbind(dfNew, testrow)
      }
      colnames(dfNew) <- c(
        "Difference in Means",
        "Mean of Condition1",
        "Mean of Condition2",
        "t Statistics",
        "p_value",
        "Parameter",
        "Lower Level of 95% CI",
        "Upper Level of 95% CI",
        "Method",
        "Alternative"
      )
      dffinal <- cbind(testdata, dfNew)
      row.names(dffinal) <- NULL
      dffinal <- dffinal
    }
    #Calculation for Mann Whitney U test
    #Non-parametric pairwise comparison
    else if (input$comptestA == 'nonpara' &&
             input$comptestB == 'pair') {
      # colname <- gsub("[[:punct:]]",'',colnames(data()))
      colname <- as.factor(colnames(data()))
      dfNew <- data.frame()
      for (i in 1:nrow(testdata)) {
        # conditionx <- gsub("[[:punct:]]",'',testdata[i,1])
        # conditiony <- gsub("[[:punct:]]",'',testdata[i,2])
        x <- match(as.factor(testdata[i, 1]), colname)
        y <- match(as.factor(testdata[i, 2]), colname)
        testrow <- wmwTest(data()[, x], data()[, y])
        testrow <- tidy(testrow)
        dfNew <- rbind(dfNew, testrow)
      }
      colnames(dfNew) <- c(
        "Mann Whitney estimate",
        "Mann Whitney estimate",
        "p_value",
        "Tie factor",
        "Lower Level of 95% CI",
        "Upper Level of 95% CI",
        "Method",
        "Alternative"
      )
      dffinal <- cbind(testdata, dfNew)
      row.names(dffinal) <- NULL
      dffinal <- dffinal
      
    }
    #Calculation for PostHoc Dunn's test
    #Non-parametric groupwise comparison
    else if (input$comptestA == 'nonpara' &&
             input$comptestB == 'group') {
      dunntest <- dunn.test(
        x = na.omit(orderdata())$value,
        g = na.omit(orderdata())$variable,
        list = T,
        method = posthocinput()
      )
      dunntest <- as.data.frame(dunntest)
      dunndf <- data.frame(p_value = dunntest$P.adjusted,
                           comparison = dunntest$comparisons)
      newdf <- data.frame()
      for (i in 1:nrow(dunndf)) {
        spp <- data.frame(unlist(strsplit(dunndf[i, 2], ' ')))
        spp <- t(spp)
        newdf <- rbind(newdf, spp)
      }
      row.names(newdf) <- NULL
      newdf <- data.frame(Condition1 = newdf[, 1], Condition2 = newdf[, 3])
      newdf <- cbind(newdf, dunndf$p_value)
      df <- data.frame()
      for (i in 1:nrow(testdata)) {
        for (j in (1:nrow(newdf))) {
          x <- match(testdata[i, ], newdf[j, ])
          if (is.na(x[1]) == FALSE && is.na(x[2]) == FALSE) {
            tempdf <- data.frame(newdf[j, ])
          } else{
            NULL
          }
        }
        df <- rbind(df, tempdf)
      }
      colnames(df) <- c('Condition1', 'Condtion2', 'p_value')
      dffinal <- df
      row.names(dffinal) <- NULL
    }
    
    #Calculation for PostHoc Tukey's HSD test
    #Parametric groupwise comparison
    else if (input$comptestA == 'para' &&
             input$comptestB == 'group') {
      model <- aov(na.omit(orderdata())$value ~ na.omit(orderdata())$variable)
      tuktest <- TukeyHSD(model, conf.level = as.numeric(posthocinput()))
      tuktest <- as.data.frame.list(tuktest)
      tuktest$row_names <- row.names(tuktest)
      row.names(tuktest) <- NULL
      colnames(tuktest) <- c(
        "Difference of Mean",
        "Lower Level of CI",
        "Upper Level of CI",
        "Corrected p value",
        "Comparisons"
      )
      tukdf <- data.frame(p_value = tuktest$`Corrected p value`,
                          comparison = tuktest$Comparisons)
      newdf <- data.frame()
      for (i in 1:nrow(tukdf)) {
        spp <- data.frame(unlist(strsplit(tukdf[i, 2], '-')))
        spp <- t(spp)
        newdf <- rbind(newdf, spp)
      }
      row.names(newdf) <- NULL
      newdf <- data.frame(Condition1 = newdf[, 1], Condition2 = newdf[, 2])
      newdf <- cbind(newdf, tukdf$p_value)
      df <- data.frame()
      for (i in 1:nrow(testdata)) {
        for (j in (1:nrow(newdf))) {
          x <- match(testdata[i, ], newdf[j, ])
          if (is.na(x[1]) == FALSE && is.na(x[2]) == FALSE) {
            tempdf <- data.frame(newdf[j, ])
          } else{
            NULL
          }
          
        }
        df <- rbind(df, tempdf)
      }
      colnames(df) <- c('Condition1', 'Condtion2', 'p_value')
      dffinal <- df
      row.names(dffinal) <- NULL
      dffinal <- dffinal
    }
    
    #Final return of the calculated p value table
    ######
    dffinal <- dffinal
    ######
  })
  
  annotationCalV <- eventReactive(input$submitAnnoteV, {
    testdata <- grpdataV$df
    #Parametric pairwise comparison
    #Calculation for Welch's t Test
    if (input$comptestAV == 'para' && input$comptestBV == 'pair') {
      colname <- as.factor(colnames(data()))
      dfNew <- data.frame()
      for (i in 1:nrow(testdata)) {
        # conditionx <- gsub("[[:punct:]]",'',testdata[i,1])
        # conditiony <- gsub("[[:punct:]]",'',testdata[i,2])
        x <- match(as.factor(testdata[i, 1]), colname)
        y <- match(as.factor(testdata[i, 2]), colname)
        testrow <- t.test(data()[, x], data()[, y])
        testrow <- tidy(testrow)
        dfNew <- rbind(dfNew, testrow)
      }
      colnames(dfNew) <- c(
        "Difference in Means",
        "Mean of Condition1",
        "Mean of Condition2",
        "t Statistics",
        "p_value",
        "Parameter",
        "Lower Level of 95% CI",
        "Upper Level of 95% CI",
        "Method",
        "Alternative"
      )
      dffinal <- cbind(testdata, dfNew)
      row.names(dffinal) <- NULL
      dffinal <- dffinal
    }
    #Non-parametric pairwise comparison
    #Calculation for Mann Whitney U test
    else if (input$comptestAV == 'nonpara' &&
             input$comptestBV == 'pair') {
      # colname <- gsub("[[:punct:]]",'',colnames(data()))
      colname <- as.factor(colnames(data()))
      dfNew <- data.frame()
      for (i in 1:nrow(testdata)) {
        # conditionx <- gsub("[[:punct:]]",'',testdata[i,1])
        # conditiony <- gsub("[[:punct:]]",'',testdata[i,2])
        x <- match(as.factor(testdata[i, 1]), colname)
        y <- match(as.factor(testdata[i, 2]), colname)
        testrow <- wmwTest(data()[, x], data()[, y])
        testrow <- tidy(testrow)
        dfNew <- rbind(dfNew, testrow)
      }
      colnames(dfNew) <- c(
        "Mann Whitney estimate",
        "Mann Whitney estimate",
        "p_value",
        "Tie factor",
        "Lower Level of 95% CI",
        "Upper Level of 95% CI",
        "Method",
        "Alternative"
      )
      dffinal <- cbind(testdata, dfNew)
      row.names(dffinal) <- NULL
      dffinal <- dffinal
    }
    #Non-parametric Group wise comparison
    #Calculation for PostHoc Dunn's test
    else if (input$comptestAV == 'nonpara' &&
             input$comptestBV == 'group') {
      dunntest <- dunn.test(
        x = na.omit(orderdata())$value,
        g = na.omit(orderdata())$variable,
        list = T,
        method = posthocinputV()
      )
      dunntest <- as.data.frame(dunntest)
      dunndf <- data.frame(p_value = dunntest$P.adjusted,
                           comparison = dunntest$comparisons)
      newdf <- data.frame()
      for (i in 1:nrow(dunndf)) {
        spp <- data.frame(unlist(strsplit(dunndf[i, 2], ' ')))
        spp <- t(spp)
        newdf <- rbind(newdf, spp)
      }
      row.names(newdf) <- NULL
      newdf <- data.frame(Condition1 = newdf[, 1], Condition2 = newdf[, 3])
      newdf <- cbind(newdf, dunndf$p_value)
      df <- data.frame()
      for (i in 1:nrow(testdata)) {
        for (j in (1:nrow(newdf))) {
          x <- match(testdata[i, ], newdf[j, ])
          if (is.na(x[1]) == FALSE && is.na(x[2]) == FALSE) {
            tempdf <- data.frame(newdf[j, ])
          } else{
            NULL
          }
        }
        df <- rbind(df, tempdf)
      }
      colnames(df) <- c('Condition1', 'Condtion2', 'p_value')
      dffinal <- df
      row.names(dffinal) <- NULL
      dffinal <- dffinal
    }
    #Parametric Group wise comparison
    #Calculation for PostHoc Tukey's HSD test
    else if (input$comptestAV == 'para' &&
             input$comptestBV == 'group') {
      model <- aov(na.omit(orderdata())$value ~ na.omit(orderdata())$variable)
      tuktest <- TukeyHSD(model, conf.level = as.numeric(posthocinputV()))
      tuktest <- as.data.frame.list(tuktest)
      tuktest$row_names <- row.names(tuktest)
      row.names(tuktest) <- NULL
      colnames(tuktest) <- c(
        "Difference of Mean",
        "Lower Level of CI",
        "Upper Level of CI",
        "Corrected p value",
        "Comparisons"
      )
      tukdf <- data.frame(p_value = tuktest$`Corrected p value`,
                          comparison = tuktest$Comparisons)
      newdf <- data.frame()
      for (i in 1:nrow(tukdf)) {
        spp <- data.frame(unlist(strsplit(tukdf[i, 2], '-')))
        spp <- t(spp)
        newdf <- rbind(newdf, spp)
      }
      row.names(newdf) <- NULL
      newdf <- data.frame(Condition1 = newdf[, 1], Condition2 = newdf[, 2])
      newdf <- cbind(newdf, tukdf$p_value)
      df <- data.frame()
      for (i in 1:nrow(testdata)) {
        for (j in (1:nrow(newdf))) {
          x <- match(testdata[i, ], newdf[j, ])
          if (is.na(x[1]) == FALSE && is.na(x[2]) == FALSE) {
            tempdf <- data.frame(newdf[j, ])
          } else{
            NULL
          }
          
        }
        df <- rbind(df, tempdf)
      }
      colnames(df) <- c('Condition1', 'Condtion2', 'p_value')
      dffinal <- df
      row.names(dffinal) <- NULL
      dffinal <- dffinal
    }
    
    #Final return of the calculated p value table
    ######
    dffinal <- dffinal
    ######
  })
  
  output$Annotetable <- renderTable({
    grpdata$df
  })
  output$AnnotetableV <- renderTable({
    grpdataV$df
  })
  
  observeEvent(input$TableReset, {
    grpdata$df <- data.frame()
  })
  observeEvent(input$TableResetV, {
    grpdataV$df <- data.frame()
  })
  
  ##Significance Annotation list Pairwise
  pairStar <- reactive({
    #for box plot
    testdata <- grpdata$df
    psigdata <- data.frame("pVal" = annotationCal()$p_value)
    newDF <- data.frame()
    for (i in 1:nrow(annotationCal())) {
      if (psigdata[i, 1] > 0.05) {
        sigstr <- c('ns')
        sigpval <- c(paste('p=', formatC(psigdata[i, 1]), sep = ''))
      } else if (psigdata[i, 1] < 0.05 && psigdata[i, 1] > 0.01) {
        sigstr <- c(intToUtf8(0x2731))
        # sigpval <- c('(p<0.05)')
        sigpval <- c(paste('p=', formatC(psigdata[i, 1]), sep = ''))
      } else if (psigdata[i, 1] < 0.01 && psigdata[i, 1] > 0.001) {
        sigstr <- c(paste(intToUtf8(0x2731), intToUtf8(0x2731), sep = ''))
        # sigpval <- c('(p<0.01)')
        sigpval <- c(paste('p=', formatC(psigdata[i, 1]), sep = ''))
      } else if (psigdata[i, 1] < 0.001 &&
                 psigdata[i, 1] > 0.0001) {
        sigstr <- c(paste(
          intToUtf8(0x2731),
          intToUtf8(0x2731),
          intToUtf8(0x2731),
          sep = ''
        ))
        # sigpval <- c('(p<0.001)')
        sigpval <- c(paste('p=', formatC(psigdata[i, 1]), sep = ''))
      } else {
        sigstr <- c(paste(
          intToUtf8(0x2731),
          intToUtf8(0x2731),
          intToUtf8(0x2731),
          intToUtf8(0x2731),
          sep = ''
        ))
        # sigpval <- c('(p<0.0001)')
        sigpval <- c(paste('p=', formatC(psigdata[i, 1]), sep = ''))
      }
      # newDF <- rbind(newDF,sigstr)
      if (input$pvalshow == 'ns') {
        if (sigstr > 0.05) {
          sigpval <- sigpval
        } else{
          sigpval <- c('')
        }
        tempdf <- append(sigstr, sigpval)
        newDF <- rbind(newDF, tempdf)
      } else if (input$pvalshow == 'all') {
        tempdf <- append(sigstr, sigpval)
        newDF <- rbind(newDF, tempdf)
      } else if (input$pvalshow == 'onlyP') {
        sigstr <- c('')
        tempdf <- append(sigstr, sigpval)
        newDF <- rbind(newDF, tempdf)
      } else{
        tempdf <- append(sigstr, '')
        newDF <- rbind(newDF, tempdf)
      }
    }
    FinalDF <- cbind(testdata, newDF)
    print(FinalDF)
  })
  pairStarV <- reactive({
    #for vio plot
    testdata <- grpdataV$df
    psigdataV <- data.frame("pVal" = annotationCalV()$p_value)
    newDF <- data.frame()
    for (i in 1:nrow(annotationCalV())) {
      if (psigdataV[i, 1] > 0.05) {
        sigstr <- c('ns')
        sigpval <- c(paste('p=', formatC(psigdataV[i, 1]), sep = ''))
      } else if (psigdataV[i, 1] < 0.05 && psigdataV[i, 1] > 0.01) {
        sigstr <- c(intToUtf8(0x2731))
        # sigpval <- c('(p<0.05)')
        sigpval <- c(paste('p=', formatC(psigdataV[i, 1]), sep = ''))
      } else if (psigdataV[i, 1] < 0.01 &&
                 psigdataV[i, 1] > 0.001) {
        sigstr <- c(paste(intToUtf8(0x2731), intToUtf8(0x2731), sep = ''))
        # sigpval <- c('(p<0.01)')
        sigpval <- c(paste('p=', formatC(psigdataV[i, 1]), sep = ''))
      } else if (psigdataV[i, 1] < 0.001 &&
                 psigdataV[i, 1] > 0.0001) {
        sigstr <- c(paste(
          intToUtf8(0x2731),
          intToUtf8(0x2731),
          intToUtf8(0x2731),
          sep = ''
        ))
        # sigpval <- c('(p<0.001)')
        sigpval <- c(paste('p=', formatC(psigdataV[i, 1]), sep = ''))
      } else {
        sigstr <- c(paste(
          intToUtf8(0x2731),
          intToUtf8(0x2731),
          intToUtf8(0x2731),
          intToUtf8(0x2731),
          sep = ''
        ))
        # sigpval <- c('(p<0.0001)')
        sigpval <- c(paste('p=', formatC(psigdataV[i, 1]), sep = ''))
      }
      # newDF <- rbind(newDF,sigstr)
      if (input$pvalshowV == 'ns') {
        if (sigstr > 0.05) {
          sigpval <- sigpval
        } else{
          sigpval <- c('')
        }
        tempdf <- append(sigstr, sigpval)
        newDF <- rbind(newDF, tempdf)
      } else if (input$pvalshowV == 'all') {
        tempdf <- append(sigstr, sigpval)
        newDF <- rbind(newDF, tempdf)
      } else if (input$pvalshowV == 'onlyP') {
        sigstr <- c('')
        tempdf <- append(sigstr, sigpval)
        newDF <- rbind(newDF, tempdf)
      } else{
        tempdf <- append(sigstr, '')
        newDF <- rbind(newDF, tempdf)
      }
    }
    FinalDF <- cbind(testdata, newDF)
    
  })
  
  pairLines <- reactive({
    #for box plot
    testdata <- grpdata$df
    #converting user selected pairs to column numbers
    colname <- as.factor(colnames(data()))
    df1 <- data.frame()
    for (i in 1:nrow(testdata)) {
      # conditionx <- gsub("[[:punct:]]", '', testdata[i, 1])
      # conditiony <- gsub("[[:punct:]]", '', testdata[i, 2])
      Column1 <- match(as.factor(testdata[i, 1]), colname)
      Column2 <- match(as.factor(testdata[i, 2]), colname)
      combined <- cbind(Column1, Column2)
      df1 <- rbind(df1, combined)
    }
    df1 <- df1 #Will be the X coordinates
    colDiff <- data.frame(ColDiff = abs(df1$Column1 - df1$Column2))#Getting the difference between the cols
    df1 <- cbind(df1, colDiff, pairStar())
    df1 <- df1[order(df1$ColDiff, decreasing = F), ]#Sorting them in increasing order
    maxPoint <- max(na.omit(orderdata())$value)
    maxPoint <- maxPoint + (maxPoint * 5 / 100)#First Line-Annotation Y-coords
    maxDownBracket <- maxPoint - (maxPoint * 10 / 100)#Downside line for the bracket
    TextUpCord <- maxPoint + (maxPoint * 5 / 100)# Y Coords for the text annotation
    ycordBase <- data.frame("Y-Cord Values" = rep(maxPoint, nrow(testdata)))
    df1 <- cbind(df1, ycordBase)#Giving a base Y cord value to all the pairs
    compareRowData <- data.frame()
    
    for (i in 1:nrow(testdata)) {
      for (j in (i + 1):nrow(testdata)) {
        if (j <= nrow(testdata)) {
          row1 <- c(df1[i, 1]:df1[i, 2])
          
          row2 <- c(df1[j, 1]:df1[j, 2])
          
          compareRow <- intersect(row1, row2)
          if (length(compareRow) > 0) {
            varYcord <- df1[j, 8] + ((input$annoteDist / 100) * maxPoint)
            
            df1[j, ] <- replace(df1[j, ], 8, varYcord)
            
          } else {
            NULL
          }
          
        } else {
          NULL
        }
      }
    }
    
    
    #Coords for the geom_segment
    anoteDF <-
      data.frame(
        x1 = c(df1[, 1]),
        x2 = c(df1[, 2]),
        y1 = c(df1[, 8]),
        y2 = c(df1[, 8]),
        text = c(df1[, 6]),
        pval = c(df1[, 7])
      )
    anoteDF <- anoteDF
  })
  
  pairLinesV <- reactive({
    #For vio plot
    testdata <- grpdataV$df
    #converting user selected pairs to column numbers
    colname <- as.factor(colnames(data()))
    df1 <- data.frame()
    for (i in 1:nrow(testdata)) {
      # conditionx <- gsub("[[:punct:]]", '', testdata[i, 1])
      # conditiony <- gsub("[[:punct:]]", '', testdata[i, 2])
      Column1 <- match(as.factor(testdata[i, 1]), colname)
      Column2 <- match(as.factor(testdata[i, 2]), colname)
      combined <- cbind(Column1, Column2)
      df1 <- rbind(df1, combined)
    }
    df1 <- df1 #Will be the X coordinates
    colDiff <- data.frame(ColDiff = abs(df1$Column1 - df1$Column2))#Getting the difference between the cols
    df1 <- cbind(df1, colDiff, pairStarV())
    df1 <- df1[order(df1$ColDiff, decreasing = F), ]#Sorting them in increasing order
    maxPoint <- max(na.omit(orderdata())$value)
    maxPoint <- maxPoint + (maxPoint * 5 / 100)#First Line-Annotation Y-coords
    maxDownBracket <- maxPoint - (maxPoint * 10 / 100)#Downside line for the bracket
    TextUpCord <- maxPoint + (maxPoint * 5 / 100)# Y Coords for the text annotation
    ycordBase <- data.frame("Y-Cord Values" = rep(maxPoint, nrow(testdata)))
    df1 <- cbind(df1, ycordBase)#Giving a base Y cord value to all the pairs
    compareRowData <- data.frame()
    
    for (i in 1:nrow(testdata)) {
      for (j in (i + 1):nrow(testdata)) {
        if (j <= nrow(testdata)) {
          row1 <- c(df1[i, 1]:df1[i, 2])
          
          row2 <- c(df1[j, 1]:df1[j, 2])
          
          compareRow <- intersect(row1, row2)
          if (length(compareRow) > 0) {
            varYcord <- df1[j, 8] + ((input$annoteDist / 100) * maxPoint)
            
            df1[j, ] <- replace(df1[j, ], 8, varYcord)
            
          } else {
            NULL
          }
          
        } else {
          NULL
        }
      }
    }
    
    
    #Coords for the geom_segment
    anoteDF <-
      data.frame(
        x1 = c(df1[, 1]),
        x2 = c(df1[, 2]),
        y1 = c(df1[, 8]),
        y2 = c(df1[, 8]),
        text = c(df1[, 6]),
        pval = c(df1[, 7])
      )
    anoteDF <- anoteDF
  })
  observeEvent(input$test, {
    output$testModBox <- renderTable(annotationCal())
  })
  observeEvent(input$testV, {
    output$testModBoxV <- renderTable(annotationCalV())
  })
  
  ##Re-using Settings
  colorsVio <- reactive({
    lapply(seq_along(data()), function(i) {
      input[[paste("colorsV", i, sep = '_')]]
    })
  })
  inputsV <- reactive({
    lapply(seq_along(data()), function(i) {
      paste("colorsV", i, sep = '_')
    })
  })
  parameterV <- reactive({
    lapply(seq_along(data()), function(i) {
      paste("Manual-Colour", i, sep = '_')
    })
  })
  
  ##Saving settings
  graphsettingsV <- reactive({
    setting <-
      data.frame(
        'Parameters' = c(
          'Plot Width',
          'Plot Height',
          'Y-axis title',
          'X-axis title',
          'X axis rotation',
          'Y axis log scale',
          'Y-axis Min limit',
          'Y-axis Max Limit',
          'Border Width',
          'Box Width',
          'FOnt family',
          'X-col Font',
          'X Title Font',
          'X Text Break',
          'Y-text Font',
          'Y Title Font',
          'Y Title Break',
          'Axes linewidth',
          'Plot BG Theme',
          'Theme Choice',
          'Preset Theme',
          'Border Colour',
          'Shader Value'
        ),
        'inputID' = c(
          'widthV',
          'heightV',
          'aytitleV',
          'axtitleV',
          'XrotateV',
          'logscaleV',
          'minYV',
          'maxYV',
          'borderWidthV',
          'boxWidthV',
          'fontV',
          'XfontcolV',
          'XfontszV',
          'XlinebreakV',
          'YfontcolV',
          'YfontszV',
          'YlinebreakV',
          'axislineV',
          'themeV',
          'choosethemeV',
          'viotheme',
          'viobordercol',
          'shadevalueV'
        ),
        'Data' = c(
          input$widthV,
          input$heightV,
          input$aytitleV,
          input$axtitleV,
          input$XrotateV,
          input$logscaleV,
          input$minYV,
          input$maxYV,
          input$borderWidthV,
          input$boxWidthV,
          input$fontV,
          input$XfontcolV,
          input$XfontszV,
          input$XlinebreakV,
          input$YfontcolV,
          input$YfontszV,
          input$YlinebreakV,
          input$axislineV,
          input$themeV,
          input$choosethemeV,
          input$viotheme,
          input$viobordercol,
          input$shadevalueV
        )
      )
    paletteSetting <-
      data.frame(
        'Parameters' = unlist(parameterV()),
        'inputID' = unlist(inputsV()),
        'Data' = unlist(colorsVio())
      )
    finaltable <- rbind(setting, paletteSetting)
    
  })
  #Reuse settings file
  
  observeEvent(input$reusesetV, {
    # Load inputs
    file <- input$usesettingV
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    uploaded_inputs <- read.csv(file$datapath)
    uploaded_inputs <- data.frame(uploaded_inputs)
    # Update each input
    for (i in 1:14) {
      updateTextInput(session, inputId = uploaded_inputs[i, 3], value = uploaded_inputs[i, 4])
    }
    ##Update colours from setting file
    output$colUpdateV <- renderUI({
      actionButton('updatecolV', 'Update Colours (Click 2X)', title = "Click twice to update colours")
    })
    
  })
  observeEvent(input$updatecolV, {
    file <- input$usesettingV
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    uploaded_inputs <- read.csv(file$datapath)
    uploaded_inputs <- data.frame(uploaded_inputs)
    # Update each input
    for (i in 15:nrow(uploaded_inputs)) {
      updateTextInput(session, inputId = uploaded_inputs[i, 3], value = uploaded_inputs[i, 4])
    }
  })
  
  ##Saving settings
  colorsBox <- reactive({
    lapply(seq_along(data()), function(i) {
      input[[paste("colors", i, sep = '_')]]
    })
  })
  colorsDP <- reactive({
    lapply(seq_along(data()), function(i) {
      input[[paste("colorsdp", i, sep = '_')]]
    })
  })
  inputs <- reactive({
    lapply(seq_along(data()), function(i) {
      paste("colors", i, sep = '_')
    })
  })
  DPinputs <- reactive({
    lapply(seq_along(data()), function(i) {
      paste("colorsdp", i, sep = '_')
    })
  })
  parameter <- reactive({
    lapply(seq_along(data()), function(i) {
      paste("Manual-Colour", i, sep = '_')
    })
  })
  DPparameter <- reactive({
    lapply(seq_along(data()), function(i) {
      paste("Manual-DP Colour", i, sep = '_')
    })
  })
  graphsettings <- reactive({
    setting <- data.frame(
      'Parameters' = c(
        'Plot Width',
        'Plot Height',
        'Y-axis title',
        'X-axis title',
        'X axis rotation',
        'Y axis log scale',
        'Y-axis Min limit',
        'Y-axis Max Limit',
        'Box Width',
        'Notches',
        'Outliers',
        'Line Width',
        'Scatter Width',
        'Point Width',
        'Point Shape',
        'Font Family',
        'X-col Font',
        'X Title Font',
        'X Text Break',
        'Y-text Font',
        'Y Title Font',
        'Y Title break',
        'Axes linewidth',
        'Plot BG Theme',
        'Choose Theme Generator',
        'Preset Theme',
        'Gradient-Col1',
        'Gradient-Col2',
        'Choose DP theme',
        'Choose Box Border theme',
        'Shader Amount',
        'DP Gradient-Col1',
        'DP Gradient Col2'
      ),
      'inputID' = c(
        'width',
        'height',
        'aytitle',
        'axtitle',
        'Xrotate',
        'logscale',
        'minY',
        'maxY',
        'boxwidth',
        'notch',
        'outlier',
        'linewidth',
        'scatter',
        'pointsize',
        'pointshape',
        'font',
        'Xfontcol',
        'Xfontsz',
        'Xlinebreak',
        'Yfontcol',
        'Yfontsz',
        'Ylinebreak',
        'axisline',
        'theme',
        'choosetheme',
        'boxtheme',
        'grad1',
        'grad2',
        'dpcolor',
        'boxbordercol',
        'shadevalue',
        'dpgrad1',
        'dpgrad2'
      ),
      'Data' = c(
        input$width,
        input$height,
        input$aytitle,
        input$axtitle,
        input$Xrotate,
        input$logscale,
        input$minY,
        input$maxY,
        input$boxwidth,
        input$notch,
        input$outlier,
        input$linewidth,
        input$scatter,
        input$pointsize,
        input$pointshape,
        input$font,
        input$Xfontcol,
        input$Xfontsz,
        input$Xlinebreak,
        input$Yfontcol,
        input$Yfontsz,
        input$Ylinebreak,
        input$axisline,
        input$theme,
        input$choosetheme,
        input$boxtheme,
        input$grad1,
        input$grad2,
        input$dpcolor,
        input$boxbordercol,
        input$shadevalue,
        input$dpgrad1,
        input$dpgrad2
      )
    )
    paletteSetting <- data.frame(
      'Parameters' = unlist(parameter()),
      'inputID' = unlist(inputs()),
      'Data' = unlist(colorsBox())
    )
    DPpaletteSetting <- data.frame(
      'Parameters' = unlist(DPparameter()),
      'inputID' = unlist(DPinputs()),
      'Data' = unlist(colorsDP())
    )
    finaltable <- rbind(setting, paletteSetting)
    finaltable <- rbind(finaltable, DPpaletteSetting)
    
  })
  #Reuse settings file
  
  observeEvent(input$reuseset, {
    #important
    # Load inputs
    file <- input$usesetting
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    uploaded_inputs <- read.csv(file$datapath)
    uploaded_inputs <- data.frame(uploaded_inputs)
    # Update each input
    for (i in 1:20) {
      updateTextInput(session, inputId = uploaded_inputs[i, 3], value = uploaded_inputs[i, 4])
    }
    ##Update colours from setting file
    output$colUpdate <- renderUI({
      actionButton('updatecol', 'Update Colours (Click 2X)', title = "Click twice to update colours")
    })
  })
  observeEvent(input$updatecol, {
    file <- input$usesetting
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    uploaded_inputs <- read.csv(file$datapath)
    uploaded_inputs <- data.frame(uploaded_inputs)
    # Update each input
    for (i in 21:nrow(uploaded_inputs)) {
      updateTextInput(session, inputId = uploaded_inputs[i, 3], value = uploaded_inputs[i, 4])
    }
  })
  
  ## JS- To initiate copy to clipboard for the plot
  # observeEvent(input[["MyBPlot"]], {
  #   shinyjs::runjs(HTML(js))
  # }, ignoreNULL = FALSE)
  # 
  # observeEvent(input[["MyVPlot"]], {
  #   shinyjs::runjs(HTML(jsV))
  # }, ignoreNULL = FALSE)
})

shinyApp(ui = ui, server = server)