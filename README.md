#Gunnels Data Visualization Dashboard

## Overview

This project is an interactive data visualization dashboard built using **R Shiny** to analyze environmental factors affecting gunnel fish presence.
It explores relationships between:
* Substrate types
* Wave exposure
* Tidal zones
* Habitat features (cobble, pool, water)
* Amphipod density


## Features
* Interactive dashboard with filters
* Time-series analysis
* Heatmaps and correlation plots
* Drill-down functionality
* Optimized performance with reactive programming

## Dataset

* File:**Gunnels.csv**
* Contains 1500+ ecological observations
* Includes variables like:
  * Gunnel presence (target)
  * Substrate type
  * Distance from low tide
  * Slope, wave exposure, etc.

## Technologies Used
* R
* Shiny
* shinydashboard
* ggplot2
* plotly
* dplyr
* DT


## How to Run

1. Install required packages:
install.packages(c(
  "shiny", "shinydashboard", "dplyr", "ggplot2",
  "DT", "tidyr", "readr", "plotly", "shinythemes", "shinyWidgets"
))

2. Run the app:
shiny::runApp("gunnels-dashboard.R")


## Key Insights

* Gunnels prefer rocky substrates (especially cobble)
* Detection decreases with distance from low tide
* Presence strongly linked with habitat features like water & cobble



## Author

VIDEESHA CATTAMANCHI


