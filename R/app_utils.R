### Source Code for SEN'sabale Plotting App ###
### MIT License - see LICENSE file for details
### Copyright (c) 2026 Sumit Sen

### Utility Codes ###

options(encoding = "UTF-8")
##CSS Custom Styles
css <- "
  body, html { margin:0; padding:0;}
  #DTtip {position:fixed; bottom:50px; left:400px;}
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
  .truncated_title .form-group label {display:block; max-width:90px; white-space: nowrap;
  overflow:hidden; text-overflow:ellipsis;}
  .truncated_title_point .form-group label {max-width:170px; white-space: nowrap;
  overflow:hidden; text-overflow:ellipsis;}
  .vscomp-wrapper.show-as-popup .vscomp-dropbox-container{z-index:9999!important;}
  .waiter-overlay{z-index:999!important;}
  .btn-check{width:0!important;}
  #savesetting{opacity:0; width:0px; height:0px;}
  #graphFinal{ display: flex; align-items:center; justify-content:flex-end; flex-direction:column; margin:auto;}
  #graphFinal img {border:1px solid lightgrey; box-shadow:1px 1px 10px #cfcfcf88; border-radius:10px; padding:10px; background: #FFF;}
  #reuseset{position:relative; margin-top:-15px;}
  .groupBtn .pretty input:checked ~ .state.p-danger label:after, .pretty.p-toggle .state.p-danger label:after{width:250px; padding:10px 25px 5px 0px;
  border-radius: 5px; height:35px; text-align:center; content:'Ungrouped Data'; font-weight:500;
  font-size: 15px !important; background: white !important; box-shadow:1px 1px 3px #acacac;}
  .groupBtn .pretty input:checked ~ .state.p-success label:after, .pretty.p-toggle .state.p-success label:after{width:250px; padding:10px 30px 5px 0px;
  border-radius: 5px; height:35px; text-align: center; content:'Grouped Data'; font-weight:500;
  font-size:15px !important; background: #3459e6 !important; color: white !important;  box-shadow:1px 1px 3px #9a9a9a !important;}
  .dropdown-menu.show, .dropdown-menu.in{display: flex; flex-direction: column;gap: 10px;}
  
  .groupBtn .pretty input{width:250px; height:35px;}
  .groupBtn{display:flex; align-item: center; justify-content:center; flex-direction:row;}
  .popover{animation: popFlash 0.8s ease-out infinite;font-weight:600!important;}
  @keyframes popFlash {
        0%, 100% { box-shadow: 5px 5px 27px 0px rgba(53,88,230,0.14),
        -5px -5px 27px 0px rgba(53,88,230,0.14); }
        25%, 75% { box-shadow: 7px 7px 27px 0px rgba(53,88,230,0.24),
        -7px -7px 27px 0px rgba(53,88,230,0.24); }
  }
  #demo-text{width: auto; height:50px; display:flex;flex-direction:row; justify-content:center; align-items:center; 
  margin:auto; padding:10px; position:relative;z-index:999; font-weight:bold;}

  #demo-text::after{content:''; position:absolute; display:block;background:white;
  height:20px; width:50px; margin-top:10px;margin-left:150px; z-index:10; animation:1s short infinite ease-out;}
  @keyframes short{
  0%{transform:translateX(0px); transform-origin:left;}
  25%{transform:translateX(10px);transform-origin:left;}
  50%{transform:translateX(20px);transform-origin:left;}
  75%{transform:translateX(30px);transform-origin:left;}
  100%{transform:translateX(0px);transform-origin:left;}
  }
  #ellipse{font-size:15px; font-weight:bold}

"
# JavaScript callback to send new column sequence to R
callback_js <- JS(
  "table.on('column-reorder', function(e, settings, details){",
  "  Shiny.setInputValue('current_column_order', details);",
  "});"
)
#Preset theme background color
bgColor <- c(
  "background: linear-gradient(90deg, #EEE1EF 0%, #554994 100%); color: white; font-weight: bold; text-shadow:1px 1px 5px #333;",
  "background: linear-gradient(90deg, #B5D1AE 0%, #122740 100%); color: white; font-weight: bold; text-shadow:1px 1px 5px #333;",
  "background: linear-gradient(90deg, #9ADA81 0%, #33B061 50%, #054239 100%); color: white; font-weight: bold; text-shadow:1px 1px 5px #333;",
  "background: linear-gradient(90deg, #FDE4DE 0%, #F56093 100%); color: white; font-weight: bold; text-shadow:1px 1px 5px #333;",
  "background: linear-gradient(90deg, #FFAE01 0%, #C70E00 100%); color: white; font-weight: bold; text-shadow:1px 1px 5px #333;",
  "background: linear-gradient(90deg, #1BFFFF 0%, #2E3192 100%); color: white; font-weight: bold; text-shadow:1px 1px 5px #333;",
  "background: linear-gradient(90deg, #FDD700 0%, #8C3617 100%); color: white; font-weight: bold; text-shadow:1px 1px 5px #333;",
  "background: linear-gradient(90deg, #BAE1FF 0%, #FFB3BA 100%); color: white; font-weight: bold; text-shadow:1px 1px 5px #333;",
  "background: linear-gradient(90deg, #F4E867 0%, #DA4B82 50%, #387494 100%); color: white; font-weight: bold; text-shadow:1px 1px 5px #333;",
  "background: linear-gradient(90deg, #2066A8 0%, #EDEDED 50%, #AE282C 100%); color: white; font-weight: bold; text-shadow:1px 1px 5px #333;",
  "background: linear-gradient(90deg, #F5EADA 0%, #768267 50%, #304659 100%); color: white; font-weight: bold; text-shadow:1px 1px 5px #333;",
  "background: linear-gradient(90deg, #C0C0C2 0%, #373737 100%); color: white; font-weight: bold; text-shadow:1px 1px 5px #333;"
)
#Plot type icons
plotUG <- c("Violin Plot" = 'violin',
            "Raincloud Plot" = 'viopoint',
            "Box-Whisker Plot" = 'box',
            "Jitter Plot"='jitter',
            "Bar Plot" = 'bar' )
plotUG_img <- c(
  sprintf("<img src='www/image/vio.png' width='50px' style='margin-right: 10px;'> %s", names(plotUG)[1]),
  sprintf("<img src='www/image/rain.png' width='50px' style='margin-right: 10px;'> %s", names(plotUG)[2]),
  sprintf("<img src='www/image/box.png' width='50px' style='margin-right: 10px;'> %s", names(plotUG)[3]),
  sprintf("<img src='www/image/jitter.png' width='50px' style='margin-right: 10px;'> %s", names(plotUG)[4]),
  sprintf("<img src='www/image/bar.png' width='50px' style='margin-right: 10px;'> %s", names(plotUG)[5]) 
)
plotG <- c("Violin Plot" = 'violin',
           "Box-Whisker Plot" = 'box')
plotG_img <- c(
  sprintf("<img src='www/image/vio_grp.png' width='50px' style='margin-right: 10px;'> %s", names(plotG)[1]),
  sprintf("<img src='www/image/box_grp.png' width='50px' style='margin-right: 10px;'> %s", names(plotG)[2])
  
)
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

starVicon <- c(
  "<i class='fa-solid fa-arrows-up-to-line'></i>" = 'bottom',
  "<i class='fa-solid fa-arrows-down-to-line'></i>" = 'top'
)
starHicon <- c(
  "<i class='fa-solid fa-align-left'></i>"='left',
  "<i class='fa-solid fa-align-center'></i>"='center',
  "<i class='fa-solid fa-align-right'></i>"='right'
)
#suppressing warnings for numerics conversion for empty element
safe_as_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}
#Creating a function to assign asterisk to P value significance 
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
  colnam <- DescTools::CombPairs(union(colnames(df$p.value),rownames(df$p.value)))
  
  finDF <- data.frame(paste(colnam$X1,'-',colnam$X2),dataS,dataP)
  colnames(finDF) <- c('Comparisons', 'T Statistic', 'Adjusted P Value')
  return(finDF)
}

#Reading demo files from data folder function
get_package_file <- function(...) {
  path <- system.file("app/data", ..., package = "Sensabled")
  if (path == "") {
    path <- file.path("inst/app/data", ...) 
  }
  return(path)
}

set.seed(123)