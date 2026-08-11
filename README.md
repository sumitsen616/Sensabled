# SEN'sable Plotting v1.0.0
**SEN’sable Plotting** is a lightweight, open-source Shiny app for visualizing and statistically analyzing discrete or categorical data—designed as a free, user-friendly alternative to paid software like GraphPad Prism.

Built with biologists, ecologists, students, and early-career researchers in mind, it offers an intuitive, no-code interface to create **publication-ready plots and statistical reports** without any knowledge of R code.

<a href="https://sumitsen-sensabled.share.connect.posit.cloud" target="_blank">Click to Access the App</a>

**Why this app?**
R is a powerful language for statistics and visualization, backed by base functions and peer-reviewed packages. However, its learning curve can be a barrier. SEN’sable Plotting removes that barrier by providing a point-and-click experience while leveraging R's robust capabilities under the hood.

**Core Packages**
<ul>
<li><b>Framework:</b> Shiny (with <code>shinyBS</code>, <code>shinyjs</code>, <code>shinywidgets</code>, <code>shinycssloaders</code>)</li>
<li><b>Data handling:</b> <code>openxlsx</code>, <code>DT</code>, <code>tidyverse</code> (<code>dplyr</code>, <code>tidyr</code>, <code>stringr</code>, <code>scales</code>), <code>broom</code></li>
<li><b>Plotting:</b> <code>ggplot2</code> + extensions (<code>ggbeeswarm</code>, <code>ggdist</code>, <code>ggnewscale</code>, <code>ggtext</code>, <code>qqplotr</code>)</li>
<li><b>Themes & UI:</b> <code>colorspace</code>, <code>colourpicker</code>, <code>bslib</code>, <code>bsplus</code>, <code>waiter</code>, <code>patchwork</code>, <code>extrafont</code>, <code>fontawesome</code></li>
<li><b>Statistics:</b> <code>rstatix</code>, <code>DescTools</code>, <code>lme4</code>, <code>emmeans</code>, <code>PMCMRplus</code>, <code>car</code>, <code>ARTool</code> (plus base stats)</li>
<li><b>Other:</b> <code>svglite</code>, <code>rJava</code></li>
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
<li>Source code: <a href="https://github.com/sumitsen616/Sensabled" target="_blank">
            https://github.com/sumitsen616/Sensabled</a></li>
                 <li>Report bugs, request features, or contribute:
                 <a href="https://github.com/sumitsen616/Sensabled/issues" target="_blank">https://github.com/sumitsen616/Sensabled/issues</a></li>
                 <li><a href="https://github.com/sumitsen616/Sensabled/tags" target="_blank">Release Notes</a></li>
</ul>

Feedback is very welcome. I actively maintain this tool and appreciate your input to make it better!<br>
**© Sumit Sen (2026)**
