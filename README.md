
# SEN'sable Plotting v1.2.0

<!-- badges: start -->
<!-- badges: end -->


<img width="200" height="200" alt="app_logo_color" src="https://github.com/user-attachments/assets/ef095771-5a98-40c4-a87e-48f17ec62712" />


**SEN’sable Plotting** is a lightweight, open-source Shiny app for visualizing and statistically analyzing discrete or categorical data. It is designed as a free, user-friendly alternative to paid software like GraphPad Prism.

Built with students and early-career researchers in mind, it offers an intuitive, no-code interface to create **publication-ready plots and statistical reports** without any knowledge of R code.

-------------------------------------------------------------------------------------------------------------------------------------------------

<b>&#10024; Why This App</b>
<br><br>
R is a powerful language for statistics and visualization, backed by base functions and peer-reviewed packages. However, its learning curve can be a barrier. SEN’sable Plotting removes that barrier by providing a point-and-click experience while leveraging R's robust capabilities under the hood.<br><br>
**Features:**
1. Easy and intuitive UI
2. Multiple options to customize plots
3. Save plots in high-quality raster images or SVG vectors
4. Use built-in statistical analysis tool and generate reports
5. Annotate P-value significance directly on the plot

-------------------------------------------------------------------------------------------------------------------------------------------------

## Installation

You can install the development version of Sensabled from [GitHub](https://github.com/) with:

``` r
install.packages("pak")
pak::pak("sumitsen616/Sensabled")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
Sensabled::run_app()
```
<br><br>
Or <a href="https://sumitsen-sensabled.share.connect.posit.cloud" target="_blank">click</a> to access this app online without installing R or Rstudio
<br>
---------------------------------------------------------------------------------------------------------------------------------
**&#128195;Quick Usage Guide**
<br><br>
1. **Upload data**: Import an Excel file (supports multiple sheets) or paste a data table directly and upload. Currently supports Box-whisker, Violin, Raincloud, Jitter, and Bar plots in single data mode or Box-whisker and violin in grouped data mode.
   
<img width="1920" height="1080" alt="image" src="https://github.com/user-attachments/assets/dc919cd9-dd36-4fd7-aaea-afd970b5d552" />

2. **Customize & Save**: Adjust shapes, themes, fonts, colors, labels via collapsible panels. Download high-resolution plots (PNG, TIFF, SVG, etc., selectable DPI).
   
<img width="1920" height="1080" alt="image" src="https://github.com/user-attachments/assets/489a796d-7d51-475f-9679-26896c331e6f" />

3. **Run statistics**: Auto-detect test type (two-sample/multi-sample, parametric/non-parametric) or choose manually. Enable post-hoc multiple comparisons,  submit, and generate report. Export stat report in publication-ready format. Add significance annotation directly from the graph tab.
   
<img width="1920" height="1080" alt="image" src="https://github.com/user-attachments/assets/b20ea5f1-1d83-4a48-915b-454de3a8cea3" />

4. **Reusable Settings:** Save selected settings for later use or import a setting (Excel) file to reuse previous settings to reproduce plots.
<br><br>
------------------------------------------------------------------------------------------------------------------------------------------------------
**&#128679;Important Disclaimer**<br><br>
Statistical results are automated for convenience, but users should always verify test assumptions, selections, and outputs using additional tools or expert consultation. This app is not a substitute for professional statistical advice.
<br><br>
**&#129309;Get Involved**<br><br>
SEN’sable Plotting is licensed under the **MIT License** (permissive open-source).
<ul>
<li>Download source code from <a href="https://github.com/sumitsen616/Sensabled" target="_blank">
            https://github.com/sumitsen616/Sensabled</a></li>
<li>Report bugs, request features, or contribute from
                 <a href="https://github.com/sumitsen616/Sensabled/issues" target="_blank">https://github.com/sumitsen616/Sensabled/issues</a></li>
<li><a href="https://github.com/sumitsen616/Sensabled/tags" target="_blank">Release Notes</a></li>
</ul>

Feedback is very welcome. I actively maintain this tool and appreciate your input to make it better!<br>
**© Sumit Sen (2026)**