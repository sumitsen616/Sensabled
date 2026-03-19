# SEN'sable Plotting v1.0.0-beta
**SEN’sable Plotting** is a lightweight, open-source Shiny app for visualizing and statistically analyzing discrete or categorical data—designed as a free, user-friendly alternative to paid software like GraphPad Prism.

Built with biologists, ecologists, students, and early-career researchers in mind, it offers an intuitive, no-code interface to create **publication-ready plots and statistical reports** without any knowledge of R code.

**Why this app?**
R is a powerful language for statistics and visualization, backed by base functions and peer-reviewed packages. However, its learning curve can be a barrier. SEN’sable Plotting removes that barrier by providing a point-and-click experience while leveraging R's robust capabilities under the hood.

**Core Packages**
<ul>
<li><b>Framework:</b> Shiny (with shinyBS, shinyjs, shinywidgets, shinycssloaders)</li>
<li><b>Data handling:</b> openxlsx, DT, tidyverse (dplyr, tidyr, stringr, scales), broom</li>
<li><b>Plotting:</b> ggplot2 + extensions (ggbeeswarm, ggdist, ggnewscale, ggtext, qqplotr)</li>
<li><b>Themes & UI:</b> colorspace, colourpicker, bslib, bsplus, waiter, patchwork, extrafont, fontawesome</li>
<li><b>Statistics:</b> rstatix, DescTools, lme4, emmeans, PMCMRplus, car, ARTool (plus base stats)</li>
<li><b>Other:</b> svglite, rJava</li>
  </ul>
All packages are open-source and freely available—full session info and dependencies are in the repo for reproducibility.

**Quick Usage Guide**
1. **Upload data** (File Upload tab): Import Excel (multi-sheet supported) or paste directly → select sheet and upload.
2. **Choose plot type** (Plot Type dropdown): Single (Box-jitter, Violin, Raincloud, Jitter, Bar) or grouped (Box-jitter and Violin) plots. Enable 'Grouped Data' if needed.
3. **Customize & view** (Graph tab): Adjust shapes, themes, fonts, colors, labels via collapsible panels. Download high-resolution plots (PNG, TIFF, SVG, etc., selectable DPI).
4. **Run statistics** (Statistics tab): Auto-detect test type (two-sample/multi-sample, parametric/non-parametric) or choose manually. Enable post-hoc comparisons if required → submit and generate report.
5. **Download & annotate:** Export stat report (Excel). Add customizable annotations (p-values, brackets, asterisks) directly to plots via the Graph tab.
6. **Post-hoc details:** Select comparison type (control vs. rest or pairwise) and columns → run analysis.
7. **Reusable Settings:** Save selected settings for later use or import a setting (Excel) file to reuse previous settings to reproduce plots.
<br><br>

**Important Disclaimer**
Statistical results are automated for convenience, but users should always verify test assumptions, selections, and outputs using additional tools or expert consultation. This app is not a substitute for professional statistical advice.

**Get Involved**
SEN’sable Plotting is licensed under the **MIT License** (permissive open-source).

<ul>
<li><b>Source code:</b> <a href="https://github.com/sumitsen616/Sensabled">
            https://github.com/sumitsen616/Sensabled</a></li>
<li><b>Report bugs, request features, or contribute:</b> <a href="https://github.com/sumitsen616/Sensabled/issues">https://github.com/sumitsen616/Sensabled/issues</a></li>
</ul>

Feedback is very welcome. I actively maintain this tool and appreciate your input to make it better!<br>
**© Sumit Sen (2026)**
