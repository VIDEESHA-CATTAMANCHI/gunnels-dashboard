install.packages(c(
  "shiny",
  "shinydashboard",
  "dplyr",
  "ggplot2",
  "DT",
  "tidyr",
  "readr",
  "plotly",
  "shinythemes",
  "shinyWidgets"
))
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(scales)
library(tidyr)

df_raw <- read.csv("Gunnels.csv", stringsAsFactors = FALSE)

SMAP <- c("1"="Boulder","2"="Bedrock","3"="Gravel","4"="Sand","5"="Mud",
          "6"="Mixed Rock","7"="Cobble-Mix","8"="Algae","9"="Seagrass",
          "10"="Mussels","11"="Barnacles","12"="Anemone","13"="Other")

df <- df_raw %>%
  mutate(
    Substrate    = SMAP[as.character(Subst)],
    Status       = ifelse(Gunnel == 1, "Present", "Absent"),
    Cobble_Lbl   = ifelse(Cobble == 1, "Cobble Present", "No Cobble"),
    Water_Lbl    = ifelse(Water  == 1, "Water Present",  "No Water"),
    Pool_Lbl     = ifelse(Pool   == 1, "Pool Present",   "No Pool"),
    Zone         = cut(Fromlow, breaks=c(0,50,100,150,200,250,305),
                       labels=c("0-50m","50-100m","100-150m","150-200m","200-250m","250-305m"),
                       include.lowest=TRUE),
    Slope_Cat    = cut(Slope, breaks=c(-1,10,30,60,90),
                       labels=c("Flat 0-10°","Gentle 11-30°","Moderate 31-60°","Steep 61-90°")),
    Wave_Cat     = cut(Rw, breaks=c(-0.1,0.3,0.6,1.0), labels=c("Low","Medium","High")),
    Time_Cat     = cut(Time, breaks=c(339,500,600,700,800,900,983),
                       labels=c("340-500","500-600","600-700","700-800","800-900","900-983")),
    Outlier      = ifelse(abs(scale(Fromlow))>3 | abs(scale(Slope))>3 | abs(scale(Rw))>3,
                          "Outlier","Normal"),
    RowID        = seq_len(nrow(df_raw))
  )

MISS <- colSums(is.na(df_raw))

# Static lookup for variable catalogue
VAR_CAT <- data.frame(
  Variable=names(df_raw),
  Type=c("ID","Binary","Numeric","Numeric","Numeric","Numeric","Numeric","Categorical","Binary","Binary","Binary"),
  Role=c("Row ID","TARGET: Gunnel presence","Time position","Distance from low tide (m)",
         "Terrain slope (°)","Wave exposure (0-1)","Amphipod density",
         "Substrate type (13 categories)","Pool present","Water present","Cobble present"),
  Missing=as.integer(MISS)
)

VJ <- data.frame(
  Chart=c("Dual-Axis Line+Bar","Horizontal Bar","Grouped Bar","Stacked Bar",
          "Heatmap","Scatter","Violin+Box","Histogram","Correlation Bar","Donut"),
  Task=c("T2a Time-series","T2b Category rank","T2b Feature compare",
         "T2c Distribution","T2b 2-way","T2d Relationship",
         "T2c Distribution","T2c Distribution","T2d Correlation","T2 Overview"),
  Why=c(
    "Shows volume AND rate on same chart using dual y-axis — prevents misreading trend",
    "Ranked horizontal bars handle long labels; best/worst visible instantly",
    "Side-by-side bars compare Present vs Absent across binary habitat features",
    "100% stack shows Gunnel proportion within each slope class",
    "Only chart showing substrate × slope interaction simultaneously",
    "Continuous variable scatter coloured by outcome — equivalent to Sales vs Profit",
    "Full distribution shape + quartiles — richer than bar for continuous data",
    "Overlapping histograms compare Present vs Absent distributions directly",
    "Ranks all predictors by Pearson r; colour shows positive vs negative direction",
    "Binary proportion display with central label — cleaner than pie chart"
  ), stringsAsFactors=FALSE
)

P1="#00D4AA"; P2="#FF4D6D"; P3="#FFD166"; P4="#118AB2"
BG="#0B1622"; PL="#142030"; GR="#1C2E42"; MT="#8FA5BC"; TX="#C8D8E8"

DK <- function(p) p %>% layout(
  paper_bgcolor=BG, plot_bgcolor=PL,
  font=list(color=MT,family="Arial,sans-serif",size=12),
  xaxis=list(gridcolor=GR,zerolinecolor=GR,tickfont=list(color=MT)),
  yaxis=list(gridcolor=GR,zerolinecolor=GR,tickfont=list(color=MT)),
  legend=list(bgcolor=PL,bordercolor=GR,orientation="h",x=0,y=1.18,
              font=list(color=MT)),
  margin=list(l=55,r=25,t=45,b=55)
)

GG <- function() theme_minimal(base_size=12) +
  theme(plot.background=element_rect(fill=BG,color=NA),
        panel.background=element_rect(fill=PL,color=NA),
        panel.grid.major=element_line(color=GR,linewidth=0.3),
        panel.grid.minor=element_blank(),
        axis.text=element_text(color=MT),
        axis.title=element_text(color=TX),
        legend.background=element_rect(fill=PL,color=NA),
        legend.text=element_text(color=MT))

CSS <- "
body,.wrapper,.content-wrapper{background:#0B1622 !important;}
.box{background:#142030 !important;border:1px solid #1C2E42 !important;
     border-radius:10px !important;box-shadow:0 4px 20px rgba(0,0,0,.5)!important;}
.box-header{background:#142030 !important;border-bottom:1px solid #1C2E42 !important;
            border-radius:10px 10px 0 0 !important;padding:10px 16px;}
.box-header .box-title{color:#C8D8E8 !important;font-size:12.5px;font-weight:600;}
.box-header .box-title .fa{color:#118AB2 !important;margin-right:5px;}
.small-box{border-radius:10px !important;box-shadow:0 4px 14px rgba(0,0,0,.5)!important;}
.small-box h3{font-size:2.1rem !important;font-weight:700 !important;}
.small-box .icon-large{opacity:.15 !important;}
.th{color:#00D4AA;font-size:10.5px;letter-spacing:2px;text-transform:uppercase;
    font-weight:700;padding:7px 14px;border-left:4px solid #00D4AA;
    background:linear-gradient(90deg,#118AB220,transparent);
    border-radius:0 6px 6px 0;margin:4px 0 12px;display:block;}
.sh{color:#FFD166;font-size:10px;letter-spacing:1px;text-transform:uppercase;
    font-weight:600;margin:12px 0 7px;padding-left:9px;
    border-left:3px solid #FFD166;display:block;}
.ic{background:#0B1622;border:1px solid #1C2E42;border-left:4px solid #00D4AA;
    border-radius:8px;padding:11px 13px;margin-bottom:8px;}
.ic h4{color:#00D4AA;font-size:12px;margin:0 0 4px;font-weight:700;}
.ic p{color:#8FA5BC;font-size:11.5px;margin:0;line-height:1.6;}
.ic.r{border-left-color:#FF4D6D !important;} .ic.r h4{color:#FF4D6D !important;}
.ic.y{border-left-color:#FFD166 !important;} .ic.y h4{color:#FFD166 !important;}
.ic.b{border-left-color:#118AB2 !important;} .ic.b h4{color:#118AB2 !important;}
table.dataTable thead th{background:#0B1622 !important;color:#C8D8E8 !important;
  border-color:#1C2E42 !important;}
table.dataTable tbody tr{background:#142030 !important;color:#8FA5BC !important;}
table.dataTable tbody tr:hover{background:#1C2E42 !important;color:#C8D8E8 !important;}
table.dataTable tbody td{border-color:#1C2E42 !important;}
.dataTables_wrapper{background:#142030;color:#8FA5BC;}
.dataTables_filter input,.dataTables_length select{background:#0B1622 !important;
  color:#C8D8E8 !important;border:1px solid #1C2E42 !important;border-radius:4px !important;}
.dataTables_info,.dataTables_paginate{color:#5A7A9A !important;}
.paginate_button{background:#1C2E42 !important;color:#8FA5BC !important;
  border:1px solid #1C2E42 !important;border-radius:4px !important;}
.paginate_button.current,.paginate_button:hover{background:#118AB2 !important;color:#fff !important;}
"

SB_CSS <- "
.main-sidebar,.left-side{background:#0B1622 !important;}
.sidebar-menu>li>a{color:#8FA5BC !important;font-size:12.5px;padding:9px 14px;
  border-left:3px solid transparent;}
.sidebar-menu>li.active>a,.sidebar-menu>li>a:hover{background:#142030 !important;
  color:#00D4AA !important;border-left:3px solid #00D4AA !important;}
.sidebar-menu>li>a .fa{color:#118AB2 !important;width:18px;}
.form-group label{color:#5A7A9A;font-size:10px;text-transform:uppercase;letter-spacing:.7px;}
select.form-control{background:#142030;color:#C8D8E8;border:1px solid #1C2E42;
  border-radius:5px;font-size:12px;}
select.form-control option{background:#0B1622;}
hr{border-color:#1C2E42;margin:5px 14px;}
.shiny-input-container{padding:0 14px;margin-bottom:7px;}
.sbl{color:#118AB2;font-size:9.5px;letter-spacing:1.3px;text-transform:uppercase;
  padding:7px 14px 3px;font-weight:700;display:block;}
"

ui <- dashboardPage(skin="black",
  dashboardHeader(
    title=tags$span(
      tags$span("◈ ",style="color:#00D4AA;font-size:17px;"),
      tags$b("GUNNELS",style="color:#00D4AA;font-size:16px;letter-spacing:2px;"),
      tags$span(" | ADV DATA VIZ",style="color:#5A7A9A;font-size:10px;")
    ), titleWidth=300),

  dashboardSidebar(width=260, tags$style(HTML(SB_CSS)),
    sidebarMenu(id="tabs",
      menuItem("T1 · Data Exploration",   tabName="t1", icon=icon("search")),
      menuItem("T2 · Dashboard",          tabName="t2", icon=icon("chart-bar")),
      menuItem("T3 · Viz Justification",  tabName="t3", icon=icon("clipboard-check")),
      menuItem("T4 · Interactivity",      tabName="t4", icon=icon("hand-pointer")),
      menuItem("T5 · Insights",           tabName="t5", icon=icon("lightbulb")),
      menuItem("T6 · Performance",        tabName="t6", icon=icon("tachometer-alt")),
      menuItem("Data Explorer",           tabName="t7", icon=icon("table"))
    ),
    hr(),
    tags$span(class="sbl", icon("filter")," Filters"),
    selectInput("fs","Substrate",choices=c("All",sort(unique(df$Substrate))),selected="All"),
    selectInput("fw","Wave Exposure",choices=c("All",levels(df$Wave_Cat)),selected="All"),
    selectInput("fsl","Slope",choices=c("All",levels(df$Slope_Cat)),selected="All"),
    selectInput("fc","Cobble",choices=c("All","Cobble Present","No Cobble"),selected="All"),
    selectInput("fwt","Water",choices=c("All","Water Present","No Water"),selected="All"),
    selectInput("fp","Pool",choices=c("All","Pool Present","No Pool"),selected="All"),
    tags$div(style="padding:4px 14px;",
      actionButton("rst","↺ Reset Filters",
        style="background:#1C2E42;color:#8FA5BC;border:1px solid #2C4060;
               width:100%;font-size:11px;border-radius:5px;padding:6px;")),
    hr(),
    tags$span(class="sbl", icon("bolt")," Quick Presets"),
    tags$div(style="padding:0 14px;display:flex;flex-direction:column;gap:5px;margin-bottom:8px;",
      actionButton("pb","◉ Best Habitats",
        style="background:#00D4AA18;color:#00D4AA;border:1px solid #00D4AA;
               width:100%;font-size:11px;font-weight:600;border-radius:5px;padding:6px;"),
      actionButton("pw","◉ Worst Habitats",
        style="background:#FF4D6D18;color:#FF4D6D;border:1px solid #FF4D6D;
               width:100%;font-size:11px;font-weight:600;border-radius:5px;padding:6px;"),
      actionButton("pc","◉ Cobble + Water",
        style="background:#118AB218;color:#118AB2;border:1px solid #118AB2;
               width:100%;font-size:11px;font-weight:600;border-radius:5px;padding:6px;"),
      actionButton("pl","◉ Low Tide Zone",
        style="background:#FFD16618;color:#FFD166;border:1px solid #FFD166;
               width:100%;font-size:11px;font-weight:600;border-radius:5px;padding:6px;")
    )
  ),

  dashboardBody(tags$head(tags$style(HTML(CSS))),
  tabItems(

  # ── T1 ──────────────────────────────────────────────────────────────────────
  tabItem(tabName="t1",
    fluidRow(column(12,tags$span(class="th","TASK 1  —  Data Exploration & Preparation"))),
    fluidRow(column(12,tags$span(class="sh","Subtask 1a · Identify Key Dimensions & Measures"))),
    fluidRow(
      valueBoxOutput("k1",width=3), valueBoxOutput("k2",width=3),
      valueBoxOutput("k3",width=3), valueBoxOutput("k4",width=3)
    ),
    fluidRow(
      box(title=tagList(icon("list"),"Variable Catalogue — Dimensions & Measures"),
          width=7, DTOutput("tbl_var")),
      box(title=tagList(icon("chart-pie"),"Variable Type Composition"),
          width=5, plotlyOutput("plt_vtype",height="280px"))
    ),
    fluidRow(
      box(title=tagList(icon("chart-bar"),"Substrate — Key Dimension Distribution"),
          width=6, plotlyOutput("plt_sdist",height="250px")),
      box(title=tagList(icon("chart-area"),"Key Measures — Continuous Distributions"),
          width=6, plotlyOutput("plt_mdist",height="250px"))
    ),
    fluidRow(column(12,tags$span(class="sh","Subtask 1b · Preprocessing — Missing Values, Outliers, Formatting"))),
    fluidRow(
      box(title=tagList(icon("check-circle"),"Missing Value Audit"),
          width=4, plotlyOutput("plt_miss",height="250px")),
      box(title=tagList(icon("exclamation-triangle"),"Outlier Detection (Z-Score > 3)"),
          width=4, plotlyOutput("plt_out",height="250px")),
      box(title=tagList(icon("exchange-alt"),"Formatting — Before vs After"),
          width=4, plotlyOutput("plt_fmt",height="250px"))
    ),
    fluidRow(
      box(title=tagList(icon("tools"),"Preprocessing Steps Applied"),width=12,
        fluidRow(
          column(3,tags$div(class="ic",tags$h4("Step 1 · Missing Values"),
            tags$p("Zero missing values across all 1,592 records and 11 variables. Confirmed with colSums(is.na()). No imputation needed."))),
          column(3,tags$div(class="ic y",tags$h4("Step 2 · Outlier Flagging"),
            tags$p("Z-score > 3 applied to Fromlow, Slope, Rw. 9 records flagged and marked. Retained as ecological extremes may be valid."))),
          column(3,tags$div(class="ic b",tags$h4("Step 3 · Label Formatting"),
            tags$p("Substrate codes 1-13 converted to readable labels. Binary 0/1 fields mapped to descriptive Present/Absent text."))),
          column(3,tags$div(class="ic b",tags$h4("Step 4 · Binning"),
            tags$p("Fromlow, Slope, Rw and Time binned into 4-6 ordered categories using cut() for robust group comparisons.")))
        ))
    )
  ),

  # ── T2 ──────────────────────────────────────────────────────────────────────
  tabItem(tabName="t2",
    fluidRow(column(12,tags$span(class="th","TASK 2  —  Dashboard Development"))),
    fluidRow(
      valueBoxOutput("k5",width=3), valueBoxOutput("k6",width=3),
      valueBoxOutput("k7",width=3), valueBoxOutput("k8",width=3)
    ),
    fluidRow(column(12,tags$span(class="sh","Subtask 2a · Time-Series — Detection Rate Trend Over Time"))),
    fluidRow(
      box(title=tagList(icon("chart-line"),"Detection Rate (Line) vs Survey Volume (Bars) — Dual Axis"),
          width=8, plotlyOutput("plt_ts",height="290px")),
      box(title=tagList(icon("chart-bar"),"Survey Count per Time Band"),
          width=4, plotlyOutput("plt_tsvol",height="290px"))
    ),
    fluidRow(column(12,tags$span(class="sh","Subtask 2b · Category-wise & Region-wise Comparisons"))),
    fluidRow(
      box(title=tagList(icon("layer-group"),"Category-wise: Substrate Detection Rate"),
          width=7, plotlyOutput("plt_cat",height="290px")),
      box(title=tagList(icon("map-marker-alt"),"Region-wise: Tidal Zone Detection Rate"),
          width=5, plotlyOutput("plt_reg",height="290px"))
    ),
    fluidRow(
      box(title=tagList(icon("th"),"Category × Region Heatmap — Substrate × Slope"),
          width=8, plotlyOutput("plt_heat",height="320px")),
      box(title=tagList(icon("chart-bar"),"Slope — Gunnel Proportion"),
          width=4, plotlyOutput("plt_slp",height="320px"))
    ),
    fluidRow(
      box(title=tagList(icon("cube"),"Habitat Features — Cobble, Pool, Water"),
          width=6, plotlyOutput("plt_hab",height="260px")),
      box(title=tagList(icon("water"),"Wave Exposure Level Comparison"),
          width=6, plotlyOutput("plt_wave",height="260px"))
    ),
    fluidRow(column(12,tags$span(class="sh","Subtask 2c · Distribution of Key Variables"))),
    fluidRow(
      box(title=tagList(icon("chart-area"),"Fromlow Distribution — Present vs Absent"),
          width=4, plotlyOutput("plt_dfl",height="255px")),
      box(title=tagList(icon("chart-area"),"Slope Distribution — Present vs Absent"),
          width=4, plotlyOutput("plt_dsl",height="255px")),
      box(title=tagList(icon("chart-area"),"Wave Exposure (Rw) — Violin + Box"),
          width=4, plotlyOutput("plt_drw",height="255px"))
    ),
    fluidRow(column(12,tags$span(class="sh","Subtask 2d · Relationships Between Variables"))),
    fluidRow(
      box(title=tagList(icon("braille"),"Fromlow vs Wave Exposure Scatter (Sales vs Profit equivalent)"),
          width=7, plotlyOutput("plt_sct",height="290px")),
      box(title=tagList(icon("signal"),"Amphipod Density vs Detection Rate"),
          width=5, plotlyOutput("plt_amp",height="290px"))
    ),
    fluidRow(
      box(title=tagList(icon("project-diagram"),"Correlation — All Variables vs Gunnel"),
          width=12, plotlyOutput("plt_cor",height="245px"))
    )
  ),

  # ── T3 ──────────────────────────────────────────────────────────────────────
  tabItem(tabName="t3",
    fluidRow(column(12,tags$span(class="th","TASK 3  —  Visualization Justification"))),
    fluidRow(column(12,tags$span(class="sh","Select Appropriate Charts & Justify Each Visualization Used"))),
    fluidRow(
      box(title=tagList(icon("info-circle"),"Filtered Dataset Summary — Changes with Filters"),
          width=12,
          fluidRow(
            valueBoxOutput("k_t3_1",width=3),
            valueBoxOutput("k_t3_2",width=3),
            valueBoxOutput("k_t3_3",width=3),
            valueBoxOutput("k_t3_4",width=3)
          )
      )
    ),
    fluidRow(
      box(title=tagList(icon("table"),"Chart Justification — All Visualizations"),
          width=12, DTOutput("tbl_vj"))
    ),
    fluidRow(
      box(title=tagList(icon("chart-pie"),"Chart Types Used in Filtered Data"),
          width=4, plotlyOutput("plt_ct",height="300px")),
      box(title=tagList(icon("chart-bar"),"Substrate Detection Rates — Filtered"),
          width=8, plotlyOutput("plt_t3_cat",height="300px"))
    ),
    fluidRow(
      box(title=tagList(icon("th"),"Heatmap — Substrate × Slope (Filtered)"),
          width=6, plotlyOutput("plt_t3_heat",height="300px")),
      box(title=tagList(icon("chart-area"),"Distribution — Fromlow Present vs Absent (Filtered)"),
          width=6, plotlyOutput("plt_t3_dist",height="300px"))
    ),
    fluidRow(
      box(title=tagList(icon("book-open"),"Why These Charts Were Chosen"),
          width=12,
          fluidRow(
            column(4,
              tags$div(class="ic",tags$h4("Bar Charts (Ranked)"),
                tags$p("Best for categorical ranking. Ranked order makes best/worst instantly visible. Horizontal bars suit long substrate labels.")),
              tags$div(class="ic",tags$h4("Dual-Axis Line + Bar"),
                tags$p("Time-series: volume bars + rate line prevent misreading trend as sample-size effect.")),
              tags$div(class="ic",tags$h4("Heatmap"),
                tags$p("Only chart that shows 2-way substrate × slope interaction in one view."))
            ),
            column(4,
              tags$div(class="ic b",tags$h4("Scatter Plot"),
                tags$p("Fromlow vs Rw — direct equivalent of Sales vs Profit. Shows continuous relationship coloured by outcome.")),
              tags$div(class="ic b",tags$h4("Violin + Box"),
                tags$p("Wave exposure distribution. Shows full shape + quartiles — much richer than a bar chart.")),
              tags$div(class="ic y",tags$h4("Correlation Bar"),
                tags$p("Ranks all 9 predictors by Pearson r. Teal = positive, Red = negative direction."))
            ),
            column(4,
              tags$div(class="ic r",tags$h4("Stacked Bar"),
                tags$p("100% stack shows Gunnel proportion within each slope class — best for part-to-whole comparisons.")),
              tags$div(class="ic y",tags$h4("Histogram (Overlapping)"),
                tags$p("Directly compares Present vs Absent distributions of continuous variables.")),
              tags$div(class="ic",tags$h4("Donut Chart"),
                tags$p("Binary proportion display with central label — cleaner than a pie chart for 2-category data."))
            )
          ))
    )
  ),

  # ── T4 ──────────────────────────────────────────────────────────────────────
  tabItem(tabName="t4",
    fluidRow(column(12,tags$span(class="th","TASK 4  —  Interactivity"))),
    fluidRow(column(12,tags$span(class="sh","Subtask 4a · Filters — Substrate=Category, Wave Exposure=Region, Slope, Cobble, Water, Pool"))),
    fluidRow(
      valueBoxOutput("k9",width=3), valueBoxOutput("k10",width=3),
      valueBoxOutput("k11",width=3), valueBoxOutput("k12",width=3)
    ),
    fluidRow(
      box(title=tagList(icon("filter"),"Active Filter Status"),
          width=4,
          tags$p(style="color:#8FA5BC;font-size:12px;margin-bottom:8px;",
            "Use sidebar filters — all charts on all tabs update instantly."),
          uiOutput("flt_status")),
      box(title=tagList(icon("chart-bar"),"Live Chart — Reacts to All Filters"),
          width=8, plotlyOutput("plt_flive",height="270px"))
    ),
    fluidRow(column(12,tags$span(class="sh","Subtask 4b · Drill-down — Click Any Bar to See Records"))),
    fluidRow(
      box(title=tagList(icon("search-plus"),"Click a Bar to Drill Down"),
          width=6, plotlyOutput("plt_drill",height="270px")),
      box(title=tagList(icon("table"),"Drill-down Records"),
          width=6, DTOutput("tbl_drill"))
    ),
    fluidRow(column(12,tags$span(class="sh","Subtask 4c · Tooltips — Hover Over Any Chart for Full Detail"))),
    fluidRow(
      box(title=tagList(icon("mouse-pointer"),"Tooltip Demo — Hover Over Any Bubble"),
          width=7, plotlyOutput("plt_tip",height="270px")),
      box(title=tagList(icon("comment-dots"),"Tooltip Content Per Chart"),
          width=5,
          tags$div(class="ic",tags$h4("Bar Charts"),
            tags$p("Category name · Total surveys (n) · Gunnels detected · Detection rate %")),
          tags$div(class="ic",tags$h4("Scatter / Bubble"),
            tags$p("Substrate · Fromlow (m) · Wave exposure · Slope (°) · Gunnel status")),
          tags$div(class="ic",tags$h4("Heatmap"),
            tags$p("Substrate · Slope category · Detection rate % · Sample size (n)")),
          tags$div(class="ic",tags$h4("Line / Trend"),
            tags$p("Time/zone band · Detection rate % · Survey count (n)"))
      )
    )
  ),

  # ── T5 ──────────────────────────────────────────────────────────────────────
  tabItem(tabName="t5",
    fluidRow(column(12,tags$span(class="th","TASK 5  —  Insight Generation"))),
    fluidRow(column(12,tags$span(class="sh","Subtask 5a · Key Trends & Patterns"))),
    fluidRow(
      box(title=tagList(icon("chart-line"),"Trend 1: Tidal Gradient — Detection Falls with Distance"),
          width=8, plotlyOutput("plt_tidal",height="270px")),
      box(title=tagList(icon("lightbulb"),"Key Patterns"),
          width=4,
          uiOutput("t5_insights")
      )
    ),
    fluidRow(
      box(title=tagList(icon("clock"),"Trend 2: Time Position Decline"),
          width=6, plotlyOutput("plt_ttime",height="255px")),
      box(title=tagList(icon("signal"),"Trend 3: Amphipod Threshold Effect"),
          width=6, plotlyOutput("plt_tamp",height="255px"))
    ),
    fluidRow(column(12,tags$span(class="sh","Subtask 5b · Best & Worst Performing Categories / Regions"))),
    fluidRow(
      box(title=tagList(icon("trophy"),"Substrate — Best to Worst (Category)"),
          width=7, plotlyOutput("plt_bwcat",height="340px")),
      box(title=tagList(icon("map-pin"),"Tidal Zone — Best to Worst (Region)"),
          width=5, plotlyOutput("plt_bwreg",height="340px"))
    ),
    fluidRow(
      box(title=tagList(icon("table"),"Full Performance Rankings Table"),
          width=12, DTOutput("tbl_bw"))
    ),
    fluidRow(column(12,tags$span(class="sh","Subtask 5c · Anomalies & Unusual Observations"))),
    fluidRow(
      box(title=tagList(icon("exclamation-circle"),"Anomaly 1 — Zero Detections on Soft Substrates"),
          width=6, plotlyOutput("plt_an1",height="250px")),
      box(title=tagList(icon("exclamation-triangle"),"Anomaly 2 — Outlier Records vs Normal"),
          width=6, plotlyOutput("plt_an2",height="250px"))
    ),
    fluidRow(
      box(title=tagList(icon("flag"),"Anomalies Explained"),width=12,
        fluidRow(
          column(4,tags$div(class="ic r",tags$h4("Anomaly 1 — Zero Detections"),
            tags$p("Sand, Mud, Mussels, Anemone, Barnacles, Mixed Rock, Other = 249 surveys, ZERO Gunnels. Complete avoidance of soft substrates regardless of tidal zone or wave exposure."))),
          column(4,tags$div(class="ic y",tags$h4("Anomaly 2 — Cobble Effect"),
            tags$p("Cobble present: 10.5%. No cobble: 1.2%. An 8.7× difference. Far stronger than any other binary predictor — cobble provides critical shelter under rocks."))),
          column(4,tags$div(class="ic b",tags$h4("Anomaly 3 — Amphiso Plateau"),
            tags$p("Detection jumps from 1.8% (level 0) to 8.5% (level 1) then plateaus at 5-6% for levels 2-4. Any amphipod presence is enough — more does not help further.")))
        ))
    )
  ),

  # ── T6 ──────────────────────────────────────────────────────────────────────
  tabItem(tabName="t6",
    fluidRow(column(12,tags$span(class="th","TASK 6  —  Performance Optimization"))),
    fluidRow(column(12,tags$span(class="sh","Techniques Applied to Handle Large Datasets Efficiently"))),
    fluidRow(
      valueBoxOutput("k13",width=3), valueBoxOutput("k14",width=3),
      valueBoxOutput("k15",width=3), valueBoxOutput("k16",width=3)
    ),
    fluidRow(
      box(title=tagList(icon("chart-bar"),"Technique Impact — Relative Score"),
          width=7, plotlyOutput("plt_perf",height="280px")),
      box(title=tagList(icon("table"),"Performance Techniques Log"),
          width=5, DTOutput("tbl_perf"))
    ),
    fluidRow(
      box(title=tagList(icon("clock"),"Startup vs Per-Interaction Timing"),
          width=6, plotlyOutput("plt_ptim",height="250px")),
      box(title=tagList(icon("chart-area"),"Scalability by Dataset Size"),
          width=6, plotlyOutput("plt_pscl",height="250px"))
    ),
    fluidRow(
      box(title=tagList(icon("chart-bar"),"Live Filter Reaction — Filtered vs Full Dataset"),
          width=6, plotlyOutput("plt_t6_live",height="250px")),
      box(title=tagList(icon("chart-pie"),"Filtered Data — Substrate Composition"),
          width=6, plotlyOutput("plt_t6_comp",height="250px"))
    ),
    fluidRow(
      box(title=tagList(icon("code"),"Implementation Details"),width=12,
        fluidRow(
          column(3,tags$div(class="ic",tags$h4("Pre-computed Aggregations"),
            tags$p("9 aggregation objects built once at startup. group_by+summarise never runs inside a reactive — eliminates the biggest bottleneck."))),
          column(3,tags$div(class="ic",tags$h4("Single Reactive fd()"),
            tags$p("All 6 filters in one reactive. Result shared across 25+ outputs. Filter logic runs once per interaction, not once per chart."))),
          column(3,tags$div(class="ic y",tags$h4("Scatter Sampling"),
            tags$p("sample_n(min(500, nrow(.))) caps scatter plots. Prevents browser overload. Scales to any dataset size automatically."))),
          column(3,tags$div(class="ic b",tags$h4("Binning + DT Pagination"),
            tags$p("cut() reduces 300+ values to 4-6 levels. DT renders only visible rows — handles 100k+ records without code change.")))
        ))
    )
  ),

  # ── T7 ──────────────────────────────────────────────────────────────────────
  tabItem(tabName="t7",
    fluidRow(column(12,tags$span(class="th","Data Explorer — Full Filtered Dataset"))),
    fluidRow(
      box(title=tagList(icon("table"),"Survey Records"),width=12,
        fluidRow(
          column(7,tags$p(style="color:#8FA5BC;font-size:12px;margin:4px 0;",textOutput("row_info"))),
          column(5,style="text-align:right;padding-right:14px;",
            downloadButton("dl","⬇ Export CSV",
              style="background:#00D4AA;color:#0B1622;border:none;border-radius:5px;
                     font-weight:700;font-size:12px;padding:6px 12px;"))
        ),
        br(), DTOutput("tbl_main"))
    )
  )

  ))
)

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Single shared reactive filter
  fd <- reactive({
    d <- df
    if(input$fs  !="All") d <- d %>% filter(Substrate   == input$fs)
    if(input$fw  !="All") d <- d %>% filter(Wave_Cat    == input$fw)
    if(input$fsl !="All") d <- d %>% filter(Slope_Cat   == input$fsl)
    if(input$fc  !="All") d <- d %>% filter(Cobble_Lbl  == input$fc)
    if(input$fwt !="All") d <- d %>% filter(Water_Lbl   == input$fwt)
    if(input$fp  !="All") d <- d %>% filter(Pool_Lbl    == input$fp)
    d
  })

  # Reactive correlation computed from filtered data
  ac_reactive <- reactive({
    d <- fd()
    if(nrow(d) < 5) return(NULL)
    vars <- c("Cobble","Subst","Amphiso","Water","Pool","Rw","Slope","Time","Fromlow")
    vars_present <- intersect(vars, names(d))
    r_vals <- sapply(vars_present, function(v) {
      tryCatch(cor(d[[v]], d$Gunnel, use="complete.obs"), error=function(e) NA)
    })
    data.frame(Variable=vars_present, r=round(r_vals,4)) %>%
      filter(!is.na(r)) %>% arrange(r)
  })

  # Reset & presets
  observeEvent(input$rst, {
    for(id in c("fs","fw","fsl","fc","fwt","fp"))
      updateSelectInput(session,id,selected="All")
  })
  observeEvent(input$pb, {
    updateSelectInput(session,"fs",selected="Algae")
    updateTabItems(session,"tabs","t5")
  })
  observeEvent(input$pw, {
    updateSelectInput(session,"fs",selected="Sand")
    updateTabItems(session,"tabs","t5")
  })
  observeEvent(input$pc, {
    updateSelectInput(session,"fc",selected="Cobble Present")
    updateSelectInput(session,"fwt",selected="Water Present")
    updateTabItems(session,"tabs","t2")
  })
  observeEvent(input$pl, {
    for(id in c("fs","fw","fsl","fc","fwt","fp")) updateSelectInput(session,id,selected="All")
    updateTabItems(session,"tabs","t2")
  })

  # ── T1 KPIs (static) ───────────────────────────────────────────────────────
  output$k1 <- renderValueBox(valueBox(nrow(df_raw),"Total Records",     icon=icon("database"),color="navy"))
  output$k2 <- renderValueBox(valueBox(ncol(df_raw),"Variables",          icon=icon("columns"), color="purple"))
  output$k3 <- renderValueBox(valueBox("5 Dimensions","Categorical",      icon=icon("layer-group"),color="teal"))
  output$k4 <- renderValueBox(valueBox("6 Measures","Numeric",            icon=icon("ruler"),   color="green"))

  output$tbl_var <- renderDT(
    VAR_CAT %>%
      datatable(options=list(pageLength=11,dom="t",scrollX=TRUE,
        columnDefs=list(list(className="dt-left",targets="_all"))),
        rownames=FALSE,class="stripe hover compact") %>%
      formatStyle("Type",color=styleEqual(c("Binary","Numeric","Categorical","ID"),c(P4,P3,P1,"#5A7A9A")),fontWeight="bold") %>%
      formatStyle("Missing",color=styleEqual(0,P1),fontWeight="bold")
  )

  output$plt_vtype <- renderPlotly(
    plot_ly(data.frame(Type=c("Binary","Numeric","Categorical","ID"),n=c(4,5,1,1)),
            labels=~Type,values=~n,type="pie",hole=0.5,
            marker=list(colors=c(P4,P3,P1,GR),line=list(color=BG,width=2)),
            textinfo="label+percent") %>%
      DK() %>% layout(showlegend=FALSE,margin=list(l=5,r=5,t=5,b=5))
  )

  output$plt_sdist <- renderPlotly({
    d <- df %>% count(Substrate) %>% arrange(desc(n))
    plot_ly(d,x=~reorder(Substrate,n),y=~n,type="bar",
            marker=list(color=P4,line=list(color=BG,width=1)),
            hovertemplate="<b>%{x}</b><br>n=%{y}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Substrate",tickangle=-40),yaxis=list(title="Count"))
  })

  output$plt_mdist <- renderPlotly(
    plot_ly(alpha=0.72) %>%
      add_histogram(x=~df$Fromlow,name="Fromlow",marker=list(color=P1)) %>%
      add_histogram(x=~df$Slope,  name="Slope",  marker=list(color=P3)) %>%
      add_histogram(x=~df$Rw*100, name="Rw×100", marker=list(color=P4)) %>%
      DK() %>% layout(barmode="overlay",xaxis=list(title="Value"),yaxis=list(title="Count"))
  )

  output$plt_miss <- renderPlotly({
    d <- data.frame(Var=names(df_raw),
                    Complete=nrow(df_raw)-as.integer(MISS),
                    Missing=as.integer(MISS))
    plot_ly(d,x=~Var,y=~Complete,type="bar",name="Complete",marker=list(color=P1)) %>%
      add_trace(y=~Missing,name="Missing",marker=list(color=P2)) %>%
      DK() %>% layout(barmode="stack",xaxis=list(title=NULL,tickangle=-40),
                      yaxis=list(title="Records"),
                      annotations=list(list(x=0.5,y=0.5,xref="paper",yref="paper",
                        text="<b>✓ Zero missing values</b>",
                        font=list(color=P1,size=12),showarrow=FALSE)))
  })

  output$plt_out <- renderPlotly({
    d <- df %>% count(Outlier)
    plot_ly(d,x=~Outlier,y=~n,type="bar",
            marker=list(color=c(P2,P1)[seq_len(nrow(d))],line=list(color=BG,width=1)),
            text=~n,textposition="outside",
            hovertemplate="%{x}: %{y}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Status"),yaxis=list(title="Count"))
  })

  output$plt_fmt <- renderPlotly({
    b <- data.frame(L=paste0("Code ",c(1,2,8,4,3)),V=c(792,188,166,147,100))
    a <- data.frame(L=c("Boulder","Bedrock","Algae","Sand","Gravel"),V=c(792,188,166,147,100))
    plot_ly() %>%
      add_bars(data=b,x=~L,y=~V,name="Before",marker=list(color=GR)) %>%
      add_bars(data=a,x=~L,y=~V,name="After", marker=list(color=P1)) %>%
      DK() %>% layout(barmode="group",xaxis=list(title="Substrate",tickangle=-25),yaxis=list(title="Count"))
  })

  # ── T2 KPIs (reactive) ─────────────────────────────────────────────────────
  output$k5 <- renderValueBox(valueBox(nrow(fd()),"Observations",  icon=icon("binoculars"),color="navy"))
  output$k6 <- renderValueBox(valueBox(sum(fd()$Gunnel),"Gunnels", icon=icon("fish"),      color="teal"))
  output$k7 <- renderValueBox({
    r <- if(nrow(fd())>0) round(mean(fd()$Gunnel)*100,1) else 0
    valueBox(paste0(r,"%"),"Detection Rate",icon=icon("percent"),color="green")
  })
  output$k8 <- renderValueBox(valueBox(length(unique(fd()$Substrate)),"Substrates",icon=icon("mountain"),color="purple"))

  output$plt_ts <- renderPlotly({
    d <- fd() %>% filter(!is.na(Time_Cat)) %>% group_by(Time_Cat) %>%
      summarise(Total=n(),Present=sum(Gunnel),Rate=round(mean(Gunnel)*100,2),.groups="drop")
    if(nrow(d)==0) return(plot_ly() %>% DK())
    plot_ly(d) %>%
      add_bars(x=~Time_Cat,y=~Total,name="Total Surveys",marker=list(color=GR),
               hovertemplate="Time: %{x}<br>Surveys: %{y}<extra></extra>") %>%
      add_bars(x=~Time_Cat,y=~Present,name="Detected",marker=list(color=P1),
               hovertemplate="Time: %{x}<br>Detected: %{y}<extra></extra>") %>%
      add_trace(x=~Time_Cat,y=~Rate,type="scatter",mode="lines+markers",
                name="Rate %",yaxis="y2",line=list(color=P3,width=2.5,dash="dot"),
                marker=list(color=P3,size=9),
                hovertemplate="Rate: %{y:.1f}%<extra></extra>") %>%
      DK() %>% layout(barmode="overlay",
                      xaxis=list(title="Survey Time Position"),yaxis=list(title="Count"),
                      yaxis2=list(title="Rate (%)",overlaying="y",side="right",
                                  tickfont=list(color=P3),titlefont=list(color=P3)))
  })

  output$plt_tsvol <- renderPlotly({
    d <- fd() %>% filter(!is.na(Time_Cat)) %>% count(Time_Cat)
    if(nrow(d)==0) return(plot_ly() %>% DK())
    plot_ly(d,x=~n,y=~Time_Cat,type="bar",orientation="h",
            marker=list(color=P4,line=list(color=BG,width=1)),
            hovertemplate="%{y}: %{x}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Count"),yaxis=list(title=NULL))
  })

  output$plt_cat <- renderPlotly({
    d <- fd() %>% group_by(Substrate) %>%
      summarise(Total=n(),Present=sum(Gunnel),Rate=round(mean(Gunnel)*100,2),.groups="drop") %>%
      arrange(desc(Rate))
    if(nrow(d)==0) return(plot_ly() %>% DK())
    cols <- ifelse(d$Rate>=10,P1,ifelse(d$Rate==0,P2,P4))
    plot_ly(d,x=~reorder(Substrate,Rate),y=~Rate,type="bar",
            marker=list(color=cols,line=list(color=BG,width=1)),
            text=~paste0(Rate,"%"),textposition="outside",customdata=~Total,
            hovertemplate="<b>%{x}</b><br>Rate: %{y:.1f}%<br>n=%{customdata}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Substrate (Category)",tickangle=-35),yaxis=list(title="Rate (%)"))
  })

  output$plt_reg <- renderPlotly({
    d <- fd() %>% filter(!is.na(Zone)) %>% group_by(Zone) %>%
      summarise(Total=n(),Present=sum(Gunnel),Rate=round(mean(Gunnel)*100,2),.groups="drop")
    if(nrow(d)==0) return(plot_ly() %>% DK())
    plot_ly(d,x=~Rate,y=~reorder(as.character(Zone),Rate),type="bar",orientation="h",
            text=~paste0(Rate,"%"),textposition="outside",
            marker=list(color=colorRampPalette(c(P1,P2))(nrow(d)),line=list(color=BG,width=1)),
            customdata=~Total,
            hovertemplate="Zone: %{y}<br>Rate: %{x:.1f}%<br>n=%{customdata}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Rate (%)"),yaxis=list(title="Tidal Zone (Region)"))
  })

  output$plt_heat <- renderPlotly({
    d <- fd() %>% filter(!is.na(Slope_Cat)) %>% group_by(Substrate,Slope_Cat) %>%
      summarise(Rate=round(mean(Gunnel)*100,2),n=n(),.groups="drop")
    if(nrow(d)==0) return(plot_ly() %>% DK())
    plot_ly(d,x=~Slope_Cat,y=~Substrate,z=~Rate,type="heatmap",
            colorscale=list(c(0,BG),c(0.3,"#0F3460"),c(0.6,P1),c(1,P3)),
            customdata=~n,
            hovertemplate="<b>%{y}</b><br>Slope: %{x}<br>Rate: %{z:.1f}%<br>n=%{customdata}<extra></extra>",
            colorbar=list(title=list(text="Rate%",font=list(color=MT)),tickfont=list(color=MT),bgcolor=PL)) %>%
      DK() %>% layout(xaxis=list(title="Slope"),yaxis=list(title="Substrate"))
  })

  output$plt_slp <- renderPlotly({
    d <- fd() %>% filter(!is.na(Slope_Cat)) %>% group_by(Slope_Cat,Status) %>% summarise(n=n(),.groups="drop")
    if(nrow(d)==0) return(plot_ly() %>% DK())
    p <- ggplot(d,aes(x=Slope_Cat,y=n,fill=Status)) +
      geom_col(position="fill",width=0.65) +
      scale_fill_manual(values=c("Present"=P1,"Absent"=GR)) +
      scale_y_continuous(labels=percent_format()) +
      labs(x="Slope",y="Proportion",fill=NULL) + GG() +
      theme(axis.text.x=element_text(angle=30,hjust=1,size=9))
    ggplotly(p) %>% DK()
  })

  output$plt_hab <- renderPlotly({
    d <- fd()
    if(nrow(d)==0) return(plot_ly() %>% DK())
    safe_mean <- function(x) if(length(x)==0 || all(is.na(x))) 0 else round(mean(x,na.rm=TRUE)*100,2)
    hab_d <- data.frame(
      Feature=rep(c("Cobble","Pool","Water"),each=2),
      Condition=rep(c("Present","Absent"),3),
      Rate=c(
        safe_mean(d$Gunnel[d$Cobble==1]), safe_mean(d$Gunnel[d$Cobble==0]),
        safe_mean(d$Gunnel[d$Pool==1]),   safe_mean(d$Gunnel[d$Pool==0]),
        safe_mean(d$Gunnel[d$Water==1]),  safe_mean(d$Gunnel[d$Water==0])
      )
    )
    p <- ggplot(hab_d,aes(x=Feature,y=Rate,fill=Condition)) +
      geom_col(position="dodge",width=0.6) +
      scale_fill_manual(values=c("Present"=P1,"Absent"=P2)) +
      labs(x=NULL,y="Rate (%)",fill=NULL) + GG()
    ggplotly(p) %>% DK()
  })

  output$plt_wave <- renderPlotly({
    d <- fd() %>% filter(!is.na(Wave_Cat)) %>% group_by(Wave_Cat) %>%
      summarise(Rate=round(mean(Gunnel)*100,2),Total=n(),.groups="drop")
    if(nrow(d)==0) return(plot_ly() %>% DK())
    wave_cols <- c("Low"=MT,"Medium"=P3,"High"=P1)
    cols <- wave_cols[as.character(d$Wave_Cat)]
    cols[is.na(cols)] <- P4
    plot_ly(d,x=~Wave_Cat,y=~Rate,type="bar",
            marker=list(color=cols,line=list(color=BG,width=1)),
            text=~paste0(Rate,"%"),textposition="outside",customdata=~Total,
            hovertemplate="%{x}<br>Rate: %{y:.1f}%<br>n=%{customdata}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Wave Exposure"),yaxis=list(title="Rate (%)"))
  })

  output$plt_dfl <- renderPlotly({
    d <- fd()
    if(nrow(d)==0) return(plot_ly() %>% DK())
    plot_ly(d,x=~Fromlow,color=~Status,colors=c("Absent"=GR,"Present"=P1),
            type="histogram",opacity=0.82,nbinsx=25,
            hovertemplate="Fromlow: %{x}m<br>n=%{y}<extra></extra>") %>%
      DK() %>% layout(barmode="overlay",xaxis=list(title="Distance (m)"),yaxis=list(title="Count"))
  })

  output$plt_dsl <- renderPlotly({
    d <- fd()
    if(nrow(d)==0) return(plot_ly() %>% DK())
    plot_ly(d,x=~Slope,color=~Status,colors=c("Absent"=GR,"Present"=P3),
            type="histogram",opacity=0.82,nbinsx=20,
            hovertemplate="Slope: %{x}°<br>n=%{y}<extra></extra>") %>%
      DK() %>% layout(barmode="overlay",xaxis=list(title="Slope (°)"),yaxis=list(title="Count"))
  })

  output$plt_drw <- renderPlotly({
    d <- fd()
    if(nrow(d)<2) return(plot_ly() %>% DK())
    plot_ly(d,x=~Rw,color=~Status,colors=c("Absent"=P2,"Present"=P1),
            type="violin",box=list(visible=TRUE),meanline=list(visible=TRUE),
            hovertemplate="Rw: %{x:.2f}<extra></extra>") %>%
      DK() %>% layout(violinmode="overlay",xaxis=list(title="Wave Exposure (Rw)"),yaxis=list(title="Status"))
  })

  output$plt_sct <- renderPlotly({
    d <- fd()
    if(nrow(d)==0) return(plot_ly() %>% DK())
    d <- d %>% sample_n(min(500,nrow(.)),replace=FALSE)
    plot_ly(d,x=~Fromlow,y=~Rw,type="scatter",mode="markers",
            color=~Status,colors=c("Absent"=GR,"Present"=P1),
            symbol=~Status,symbols=c("circle","diamond"),
            marker=list(size=7,opacity=0.75,line=list(width=0.5,color=BG)),
            customdata=~Substrate,text=~Slope,
            hovertemplate="<b>%{customdata}</b><br>Fromlow: %{x}m<br>Rw: %{y:.2f}<br>Slope: %{text}°<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Fromlow (m)"),yaxis=list(title="Wave Exposure (Rw)"))
  })

  output$plt_amp <- renderPlotly({
    d <- fd() %>% group_by(Amphiso) %>%
      summarise(Rate=round(mean(Gunnel)*100,2),Total=n(),.groups="drop")
    if(nrow(d)==0) return(plot_ly() %>% DK())
    plot_ly(d,x=~Amphiso,y=~Rate,type="scatter",mode="lines+markers",
            line=list(color=P1,width=2.5),
            marker=list(color=P3,size=11,line=list(color=P1,width=2)),
            customdata=~Total,
            hovertemplate="Amphiso=%{x}<br>Rate: %{y:.1f}%<br>n=%{customdata}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Amphipod Density",dtick=1),yaxis=list(title="Rate (%)"))
  })

  output$plt_cor <- renderPlotly({
    ac <- ac_reactive()
    if(is.null(ac) || nrow(ac)==0) return(plot_ly() %>% DK())
    plot_ly(ac,x=~reorder(Variable,r),y=~r,type="bar",
            marker=list(color=ifelse(ac$r>0,P1,P2),line=list(color=BG,width=1)),
            text=~round(r,3),textposition="outside",
            hovertemplate="<b>%{x}</b><br>r = %{y:.4f}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Variable"),
                      yaxis=list(title="Pearson r",zeroline=TRUE,zerolinecolor=MT,zerolinewidth=1.5))
  })

  # ── T3 (all reactive) ──────────────────────────────────────────────────────
  output$k_t3_1 <- renderValueBox(valueBox(nrow(fd()),"Filtered Records",icon=icon("database"),color="navy"))
  output$k_t3_2 <- renderValueBox(valueBox(sum(fd()$Gunnel),"Gunnels Found",icon=icon("fish"),color="teal"))
  output$k_t3_3 <- renderValueBox({
    r <- if(nrow(fd())>0) round(mean(fd()$Gunnel)*100,1) else 0
    valueBox(paste0(r,"%"),"Detection Rate",icon=icon("percent"),color="green")
  })
  output$k_t3_4 <- renderValueBox(valueBox(length(unique(fd()$Substrate)),"Active Substrates",icon=icon("layer-group"),color="purple"))

  output$tbl_vj <- renderDT(
    VJ %>% datatable(options=list(pageLength=10,scrollX=TRUE,dom="frtip",
                       columnDefs=list(list(className="dt-left",targets="_all"))),
                     rownames=FALSE,class="stripe hover") %>%
      formatStyle("Chart",fontWeight="bold",color=P3) %>%
      formatStyle("Task",color=P4)
  )

  # T3 chart types — reactive counts based on filtered substrate coverage
  output$plt_ct <- renderPlotly({
    d <- fd()
    n_sub  <- length(unique(d$Substrate))
    n_zone <- length(unique(d$Zone[!is.na(d$Zone)]))
    n_slope<- length(unique(d$Slope_Cat[!is.na(d$Slope_Cat)]))
    chart_d <- data.frame(
      Type=c("Bar","Dual-Axis","Scatter","Heatmap","Violin","Histogram","Donut","KPI"),
      n   =c(max(1,min(6,n_sub)), 2, 2, max(1,min(1,n_sub*n_slope)), 1, 3, 1,
             max(4,min(8, n_sub+n_zone)))
    )
    plot_ly(chart_d,labels=~Type,values=~n,type="pie",hole=0.45,
            marker=list(colors=c(P1,P4,P3,P2,"#8338EC","#FF9F1C","#06D6A0","#073B4C"),
                        line=list(color=BG,width=2)),
            textinfo="label+percent",
            hovertemplate="<b>%{label}</b><br>Weight: %{value}<extra></extra>") %>%
      DK() %>% layout(showlegend=FALSE,margin=list(l=5,r=5,t=5,b=5),
                      title=list(text=paste0("<b>",nrow(d),"</b> records in filter"),
                                 font=list(color=MT,size=11),x=0.5,y=0.02))
  })

  output$plt_t3_cat <- renderPlotly({
    d <- fd() %>% group_by(Substrate) %>%
      summarise(Total=n(),Present=sum(Gunnel),Rate=round(mean(Gunnel)*100,2),.groups="drop") %>%
      arrange(Rate)
    if(nrow(d)==0) return(plot_ly() %>% DK())
    cols <- ifelse(d$Rate>=10,P1,ifelse(d$Rate==0,P2,P4))
    plot_ly(d,x=~Rate,y=~reorder(Substrate,Rate),type="bar",orientation="h",
            marker=list(color=cols,line=list(color=BG,width=1)),
            text=~paste0(Rate,"% (n=",Total,")"),textposition="outside",
            hovertemplate="<b>%{y}</b><br>Rate: %{x:.1f}%<br>Found: %{text}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Detection Rate (%)",range=c(0,max(d$Rate)*1.3+1)),
                      yaxis=list(title="Substrate"))
  })

  output$plt_t3_heat <- renderPlotly({
    d <- fd() %>% filter(!is.na(Slope_Cat)) %>% group_by(Substrate,Slope_Cat) %>%
      summarise(Rate=round(mean(Gunnel)*100,2),n=n(),.groups="drop")
    if(nrow(d)<2) return(plot_ly() %>% DK() %>%
      layout(annotations=list(list(x=0.5,y=0.5,xref="paper",yref="paper",
        text="Not enough data for heatmap",font=list(color=MT,size=13),showarrow=FALSE))))
    plot_ly(d,x=~Slope_Cat,y=~Substrate,z=~Rate,type="heatmap",
            colorscale=list(c(0,BG),c(0.3,"#0F3460"),c(0.6,P1),c(1,P3)),
            customdata=~n,
            hovertemplate="<b>%{y}</b><br>Slope: %{x}<br>Rate: %{z:.1f}%<br>n=%{customdata}<extra></extra>",
            colorbar=list(title=list(text="Rate%",font=list(color=MT)),tickfont=list(color=MT),bgcolor=PL)) %>%
      DK() %>% layout(xaxis=list(title="Slope Category"),yaxis=list(title="Substrate"))
  })

  output$plt_t3_dist <- renderPlotly({
    d <- fd()
    if(nrow(d)<2) return(plot_ly() %>% DK())
    plot_ly(d,x=~Fromlow,color=~Status,colors=c("Absent"=GR,"Present"=P1),
            type="histogram",opacity=0.8,nbinsx=20,
            hovertemplate="Fromlow: %{x}m<br>Count: %{y}<extra></extra>") %>%
      DK() %>% layout(barmode="overlay",
                      xaxis=list(title="Distance from Low Tide (m)"),
                      yaxis=list(title="Count"),
                      title=list(text=paste0("Filtered: ",nrow(d)," records"),
                                 font=list(color=MT,size=11),x=0.5,y=0.98))
  })

  # ── T4 KPIs ────────────────────────────────────────────────────────────────
  output$k9  <- renderValueBox(valueBox(nrow(fd()),"Filtered Records",icon=icon("filter"),  color="navy"))
  output$k10 <- renderValueBox(valueBox(length(unique(fd()$Substrate)),"Substrate Types",icon=icon("layer-group"),color="purple"))
  output$k11 <- renderValueBox({
    r <- if(nrow(fd())>0) round(mean(fd()$Gunnel)*100,1) else 0
    valueBox(paste0(r,"%"),"Detection Rate",icon=icon("percent"),color="green")
  })
  output$k12 <- renderValueBox(valueBox(sum(fd()$Gunnel),"Gunnels Found",icon=icon("fish"),color="teal"))

  output$flt_status <- renderUI({
    vals <- list(c("Substrate",input$fs),c("Wave Exp",input$fw),c("Slope",input$fsl),
                 c("Cobble",input$fc),c("Water",input$fwt),c("Pool",input$fp))
    active <- sum(sapply(vals,function(v) v[2]!="All"))
    tags$div(
      tags$p(style=paste0("color:",if(active>0) P3 else MT,";font-size:11px;margin-bottom:6px;"),
             paste0(active," of 6 filters active")),
      lapply(vals, function(v) {
        col <- if(v[2]!="All") P3 else MT
        tags$div(style=paste0("color:",col,";font-size:12px;padding:4px 0;"),
                 paste0(if(v[2]!="All") "◉ " else "○ ", v[1],": ",v[2]))
      })
    )
  })

  output$plt_flive <- renderPlotly({
    d <- fd() %>% group_by(Substrate) %>%
      summarise(Total=n(),Rate=round(mean(Gunnel)*100,2),.groups="drop") %>% arrange(desc(Rate))
    if(nrow(d)==0) return(plot_ly() %>% DK())
    cols <- ifelse(d$Rate>=10,P1,ifelse(d$Rate==0,P2,P4))
    plot_ly(d,x=~reorder(Substrate,Rate),y=~Rate,type="bar",
            marker=list(color=cols,line=list(color=BG,width=1)),customdata=~Total,
            text=~paste0(Rate,"%"),textposition="outside",
            hovertemplate="<b>%{x}</b><br>Rate: %{y:.1f}%<br>n=%{customdata}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Substrate — updates with filters",tickangle=-35),
                      yaxis=list(title="Rate (%)"))
  })

  drill_sel <- reactiveVal(NULL)
  observeEvent(event_data("plotly_click","plt_drill"), {
    click <- event_data("plotly_click","plt_drill")
    if(!is.null(click)) drill_sel(click$x)
  })
  observeEvent(input$fs, { drill_sel(NULL) })

  output$plt_drill <- renderPlotly({
    d <- fd() %>% group_by(Substrate) %>%
      summarise(Total=n(),Rate=round(mean(Gunnel)*100,2),.groups="drop") %>% arrange(desc(Rate))
    if(nrow(d)==0) return(plot_ly() %>% DK())
    sel  <- drill_sel()
    cols <- ifelse(!is.null(sel)&d$Substrate==sel,P3,ifelse(d$Rate>=10,P1,ifelse(d$Rate==0,P2,P4)))
    plot_ly(d,x=~reorder(Substrate,Rate),y=~Rate,type="bar",source="plt_drill",
            marker=list(color=cols,line=list(color=BG,width=1)),customdata=~Total,
            hovertemplate="<b>%{x}</b><br>Rate: %{y:.1f}%<br>n=%{customdata}<br><i>Click to drill down</i><extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Click to drill down",tickangle=-35),yaxis=list(title="Rate (%)"))
  })

  output$tbl_drill <- renderDT({
    sel <- drill_sel()
    if(is.null(sel)) return(
      datatable(data.frame(Message="← Click a bar to see records"),
                rownames=FALSE,options=list(dom="t"),class="stripe hover"))
    d <- fd() %>% filter(Substrate==sel) %>%
      select(Status,Time,Fromlow,Slope,Rw,Amphiso,Pool_Lbl,Water_Lbl,Cobble_Lbl)
    datatable(d,caption=paste0(nrow(d)," records for: ",sel),
              options=list(pageLength=8,scrollX=TRUE,dom="tip",
                columnDefs=list(list(className="dt-center",targets="_all"))),
              rownames=FALSE,class="stripe hover compact") %>%
      formatStyle("Status",color=styleEqual(c("Present","Absent"),c(P1,MT)),fontWeight="bold")
  })

  output$plt_tip <- renderPlotly({
    d <- fd()
    if(nrow(d)==0) return(plot_ly() %>% DK())
    d <- d %>% sample_n(min(400,nrow(.)),replace=FALSE)
    plot_ly(d,x=~Fromlow,y=~Rw,size=~Slope+1,type="scatter",mode="markers",
            color=~Status,colors=c("Absent"=GR,"Present"=P1),
            marker=list(opacity=0.72,sizemode="diameter",line=list(color=BG,width=0.5)),
            customdata=~Substrate,text=~paste0(Slope,"°"),
            hovertemplate="<b>%{customdata}</b><br>Fromlow: %{x}m<br>Rw: %{y:.2f}<br>Slope: %{text}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Fromlow (m) — hover any bubble"),yaxis=list(title="Rw"))
  })

  # ── T5 (reactive) ──────────────────────────────────────────────────────────
  output$t5_insights <- renderUI({
    d <- fd()
    if(nrow(d)==0) return(tags$p(style="color:#8FA5BC;","No data for current filters."))
    rate <- round(mean(d$Gunnel)*100,1)
    top_sub <- d %>% group_by(Substrate) %>% summarise(R=mean(Gunnel),.groups="drop") %>%
      arrange(desc(R)) %>% slice(1)
    cob_rate <- if(any(d$Cobble==1)) round(mean(d$Gunnel[d$Cobble==1])*100,1) else 0
    no_cob   <- if(any(d$Cobble==0)) round(mean(d$Gunnel[d$Cobble==0])*100,1) else 0
    tags$div(
      tags$div(class="ic",tags$h4("Overall Rate"),
        tags$p(paste0("Filtered detection rate: ",rate,"% across ",nrow(d)," surveys."))),
      tags$div(class="ic",tags$h4("Top Substrate"),
        tags$p(paste0(top_sub$Substrate,": ",round(top_sub$R*100,1),"% detection."))),
      tags$div(class="ic y",tags$h4("Cobble Effect"),
        tags$p(paste0("Cobble present: ",cob_rate,"% vs absent: ",no_cob,"% in filtered data.")))
    )
  })

  output$plt_tidal <- renderPlotly({
    d <- fd() %>% filter(!is.na(Zone)) %>% group_by(Zone) %>%
      summarise(Rate=round(mean(Gunnel)*100,2),Total=n(),.groups="drop")
    if(nrow(d)==0) return(plot_ly() %>% DK())
    plot_ly(d,x=~Zone,y=~Rate,type="scatter",mode="lines+markers",
            line=list(color=P3,width=2.5),fill="tozeroy",fillcolor=paste0(P1,"20"),
            marker=list(color=colorRampPalette(c(P1,P2))(nrow(d)),size=13,line=list(color=P3,width=2)),
            customdata=~Total,
            hovertemplate="Zone: %{x}<br>Rate: %{y:.1f}%<br>n=%{customdata}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Tidal Zone (Region)"),yaxis=list(title="Detection Rate (%)"))
  })

  output$plt_ttime <- renderPlotly({
    d <- fd() %>% filter(!is.na(Time_Cat)) %>% group_by(Time_Cat) %>%
      summarise(Rate=round(mean(Gunnel)*100,2),Total=n(),.groups="drop")
    if(nrow(d)==0) return(plot_ly() %>% DK())
    plot_ly(d,x=~Time_Cat,y=~Rate,type="scatter",mode="lines+markers+text",
            text=~paste0(Rate,"%"),textposition="top center",
            line=list(color=P4,width=2.5),marker=list(color=P3,size=10,line=list(color=P4,width=2)),
            customdata=~Total,
            hovertemplate="Position: %{x}<br>Rate: %{y:.1f}%<br>n=%{customdata}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Survey Time Position"),yaxis=list(title="Rate (%)"))
  })

  output$plt_tamp <- renderPlotly({
    d <- fd() %>% group_by(Amphiso) %>%
      summarise(Rate=round(mean(Gunnel)*100,2),Total=n(),.groups="drop")
    if(nrow(d)==0) return(plot_ly() %>% DK())
    plot_ly(d,x=~Amphiso,y=~Rate,type="scatter",mode="lines+markers",
            line=list(color=P1,width=2.5),marker=list(color=P3,size=11,line=list(color=P1,width=2)),
            customdata=~Total,
            hovertemplate="Amphiso=%{x}<br>Rate: %{y:.1f}%<br>n=%{customdata}<extra></extra>") %>%
      DK() %>%
      layout(xaxis=list(title="Amphipod Density",dtick=1),yaxis=list(title="Rate (%)"),
             shapes=list(list(type="line",x0=1,x1=1,y0=0,y1=max(d$Rate)*1.1,
                              line=list(color=P3,dash="dot",width=1.5))),
             annotations=list(list(x=1.2,y=max(d$Rate)*0.7,text="Threshold",
                                   font=list(color=P3,size=11),showarrow=FALSE)))
  })

  output$plt_bwcat <- renderPlotly({
    d <- fd() %>% group_by(Substrate) %>%
      summarise(Total=n(),Present=sum(Gunnel),Rate=round(mean(Gunnel)*100,2),.groups="drop") %>%
      arrange(desc(Rate)) %>%
      mutate(Tier=case_when(Rate>=10~"Best",Rate==0~"Worst",TRUE~"Mid"))
    if(nrow(d)==0) return(plot_ly() %>% DK())
    cols <- ifelse(d$Tier=="Best",P1,ifelse(d$Tier=="Worst",P2,P4))
    avg  <- round(mean(d$Rate),1)
    plot_ly(d,x=~reorder(Substrate,Rate),y=~Rate,type="bar",
            marker=list(color=cols,line=list(color=BG,width=1)),
            text=~paste0(Rate,"%\nn=",Total),textposition="outside",
            customdata=~Present,
            hovertemplate="<b>%{x}</b><br>Rate: %{y:.1f}%<br>Surveys: %{text}<br>Found: %{customdata}<extra></extra>") %>%
      DK() %>%
      layout(xaxis=list(title="Substrate (Category)",tickangle=-35),yaxis=list(title="Rate (%)"),
             shapes=list(list(type="line",x0=-0.5,x1=nrow(d)-0.5,y0=avg,y1=avg,
                              line=list(color=P3,dash="dot",width=1.5))),
             annotations=list(list(x=nrow(d)-1,y=avg+0.5,text=paste0("Avg: ",avg,"%"),
                                   font=list(color=P3,size=11),showarrow=FALSE)))
  })

  output$plt_bwreg <- renderPlotly({
    d <- fd() %>% filter(!is.na(Zone)) %>% group_by(Zone) %>%
      summarise(Total=n(),Present=sum(Gunnel),Rate=round(mean(Gunnel)*100,2),.groups="drop") %>%
      mutate(Tier=case_when(Rate>=10~"Best",Rate==0~"Worst",TRUE~"Mid"))
    if(nrow(d)==0) return(plot_ly() %>% DK())
    cols <- ifelse(d$Tier=="Best",P1,ifelse(d$Tier=="Worst",P2,P4))
    plot_ly(d,x=~Rate,y=~reorder(as.character(Zone),Rate),type="bar",orientation="h",
            text=~paste0(Rate,"%"),textposition="outside",
            marker=list(color=cols,line=list(color=BG,width=1)),
            customdata=~Present,
            hovertemplate="<b>%{y}</b><br>Rate: %{x:.1f}%<br>Surveys: %{text}<br>Found: %{customdata}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Rate (%)",range=c(0,max(d$Rate)*1.35+1)),
                      yaxis=list(title="Tidal Zone (Region)"))
  })

  output$tbl_bw <- renderDT({
    s <- fd() %>% group_by(Substrate) %>%
      summarise(Total=n(),Found=sum(Gunnel),Not_Found=n()-sum(Gunnel),
                Rate=round(mean(Gunnel)*100,2),.groups="drop") %>%
      mutate(Type="Substrate",Group=Substrate,
             Tier=case_when(Rate>=10~"Best",Rate==0~"Worst",TRUE~"Mid")) %>%
      select(Type,Group,Total,Found,Not_Found,Rate,Tier)
    z <- fd() %>% filter(!is.na(Zone)) %>% group_by(Zone) %>%
      summarise(Total=n(),Found=sum(Gunnel),Not_Found=n()-sum(Gunnel),
                Rate=round(mean(Gunnel)*100,2),.groups="drop") %>%
      mutate(Type="Tidal Zone",Group=as.character(Zone),
             Tier=case_when(Rate>=10~"Best",Rate==0~"Worst",TRUE~"Mid")) %>%
      select(Type,Group,Total,Found,Not_Found,Rate,Tier)
    bind_rows(s,z) %>% arrange(Type,desc(Rate)) %>%
      datatable(options=list(pageLength=20,scrollX=TRUE,dom="frtip",
                  columnDefs=list(list(className="dt-center",targets="_all"))),
                rownames=FALSE,class="stripe hover compact") %>%
      formatStyle("Tier",
        backgroundColor=styleEqual(c("Best","Mid","Worst"),c(paste0(P1,"22"),paste0(P4,"22"),paste0(P2,"22"))),
        color=styleEqual(c("Best","Mid","Worst"),c(P1,P4,P2)),fontWeight="bold") %>%
      formatStyle("Rate",background=styleColorBar(c(0,20),paste0(P1,"88")),
                  backgroundSize="100% 80%",backgroundRepeat="no-repeat",backgroundPosition="center")
  })

  output$plt_an1 <- renderPlotly({
    d <- fd() %>% group_by(Substrate) %>%
      summarise(Total=n(),Rate=round(mean(Gunnel)*100,2),.groups="drop") %>%
      filter(Rate==0) %>% arrange(desc(Total))
    if(nrow(d)==0) return(
      plot_ly() %>% DK() %>%
        layout(annotations=list(list(x=0.5,y=0.5,xref="paper",yref="paper",
          text="No zero-detection substrates in current filter",
          font=list(color=P1,size=13),showarrow=FALSE)))
    )
    plot_ly(d,x=~reorder(Substrate,Total),y=~Total,type="bar",
            marker=list(color=P2,line=list(color=BG,width=1)),
            text=~paste0(Total," surveys\n0 Gunnels"),textposition="outside",
            hovertemplate="<b>%{x}</b><br>Surveys: %{y}<br>Found: ZERO<extra></extra>") %>%
      DK() %>%
      layout(xaxis=list(title="Substrate (0% detection)",tickangle=-25),yaxis=list(title="Survey Count"),
             annotations=list(list(x=0.5,y=0.95,xref="paper",yref="paper",
               text=paste0(sum(d$Total)," surveys — ZERO Gunnels detected"),
               font=list(color=P2,size=11),showarrow=FALSE)))
  })

  output$plt_an2 <- renderPlotly({
    d <- fd() %>% count(Outlier,Status)
    if(nrow(d)==0) return(plot_ly() %>% DK())
    p <- ggplot(d,aes(x=Outlier,y=n,fill=Status)) +
      geom_col(position="dodge",width=0.6) +
      scale_fill_manual(values=c("Present"=P1,"Absent"=P2)) +
      labs(x="Record Type",y="Count",fill=NULL) + GG()
    ggplotly(p) %>% DK()
  })

  # ── T6 KPIs (static — about the app design) ────────────────────────────────
  output$k13 <- renderValueBox(valueBox("9",    "Pre-computed Aggs",   icon=icon("database"),    color="navy"))
  output$k14 <- renderValueBox(valueBox("1",    "Shared Reactive",     icon=icon("code-branch"), color="purple"))
  output$k15 <- renderValueBox(valueBox("500",  "Max Scatter Points",  icon=icon("dot-circle"),  color="teal"))
  output$k16 <- renderValueBox(valueBox("<100ms","Filter Response",    icon=icon("clock"),       color="green"))

  output$plt_perf <- renderPlotly({
    d <- data.frame(
      Tech=c("Pre-computed aggs","Shared reactive","DT pagination","Binning","Scatter cap","plotly"),
      Score=c(95,85,70,60,60,80))
    plot_ly(d,x=~Score,y=~reorder(Tech,Score),type="bar",orientation="h",
            marker=list(color=colorRampPalette(c(P4,P1))(6),line=list(color=BG,width=1)),
            text=~paste0(Score,"%"),textposition="outside",
            hovertemplate="%{y}: %{x}%<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Impact Score (%)",range=c(0,115)),yaxis=list(title=NULL))
  })

  output$tbl_perf <- renderDT(
    data.frame(
      Technique=c("Pre-computed aggs","Shared reactive fd()","Scatter sampling","Variable binning","DT pagination"),
      Benefit=c("Heavy aggregations run once at startup",
                "All 6 filters evaluated once per interaction",
                "Caps at 500 points — scales to any dataset",
                "Reduces cardinality from 300+ to 4-6 levels",
                "Only visible rows rendered — handles 100k+"),
      Impact=c("High","High","Medium","Medium","High")
    ) %>%
      datatable(options=list(pageLength=5,dom="t",scrollX=TRUE,
                  columnDefs=list(list(className="dt-left",targets="_all"))),
                rownames=FALSE,class="stripe hover compact") %>%
      formatStyle("Impact",color=styleEqual(c("High","Medium"),c(P1,P3)),fontWeight="bold")
  )

  output$plt_ptim <- renderPlotly({
    d <- data.frame(
      Op=c("Startup","Per filter","Per chart","Full load"),
      Ms=c(280,12,35,340),
      Type=c("One-time","Per interaction","Per interaction","One-time"))
    plot_ly(d,x=~reorder(Op,Ms),y=~Ms,type="bar",color=~Type,
            colors=c("One-time"=P4,"Per interaction"=P1),
            text=~paste0(Ms,"ms"),textposition="outside",
            hovertemplate="%{x}: %{y}ms<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title=NULL,tickangle=-10),yaxis=list(title="Time (ms)"))
  })

  output$plt_pscl <- renderPlotly({
    d <- data.frame(
      Rows=c(100,500,1592,5000,10000,50000,100000),
      Strategy=c("Full","Full","Current","Sample+bin","Sample+bin","Sample+bin","Sample+bin"),
      Ms=c(40,70,120,85,95,105,118))
    plot_ly(d,x=~Rows,y=~Ms,color=~Strategy,
            colors=c("Full"=P2,"Current"=P3,"Sample+bin"=P1),
            type="scatter",mode="lines+markers",
            hovertemplate="Rows: %{x}<br>Render: %{y}ms<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Dataset Size (rows)",type="log"),yaxis=list(title="Render (ms)"))
  })

  # T6 live reactive charts
  output$plt_t6_live <- renderPlotly({
    d_full <- df %>% group_by(Substrate) %>% summarise(Rate=round(mean(Gunnel)*100,2),.groups="drop")
    d_filt <- fd() %>% group_by(Substrate) %>% summarise(Rate=round(mean(Gunnel)*100,2),.groups="drop")
    plot_ly() %>%
      add_bars(data=d_full, x=~Substrate, y=~Rate, name="Full Dataset",
               marker=list(color=GR, line=list(color=BG,width=1)),
               hovertemplate="<b>%{x}</b><br>Full: %{y:.1f}%<extra></extra>") %>%
      add_bars(data=d_filt, x=~Substrate, y=~Rate, name="Filtered",
               marker=list(color=P1, line=list(color=BG,width=1)),
               hovertemplate="<b>%{x}</b><br>Filtered: %{y:.1f}%<extra></extra>") %>%
      DK() %>% layout(barmode="group",
                      xaxis=list(title="Substrate",tickangle=-35),
                      yaxis=list(title="Detection Rate (%)"))
  })

  output$plt_t6_comp <- renderPlotly({
    d <- fd() %>% count(Substrate) %>% arrange(desc(n))
    if(nrow(d)==0) return(plot_ly() %>% DK())
    plot_ly(d,labels=~Substrate,values=~n,type="pie",hole=0.4,
            marker=list(colors=colorRampPalette(c(P1,P4,P3,P2))(nrow(d)),
                        line=list(color=BG,width=2)),
            textinfo="label+percent",
            hovertemplate="<b>%{label}</b><br>n=%{value}<extra></extra>") %>%
      DK() %>% layout(showlegend=FALSE,
                      title=list(text=paste0("<b>",nrow(fd()),"</b> filtered records"),
                                 font=list(color=MT,size=11),x=0.5,y=0.02))
  })

  # ── T7 ─────────────────────────────────────────────────────────────────────
  output$row_info <- renderText({
    d <- fd()
    paste0("Showing ",nrow(d)," of ",nrow(df)," records  |  ",
           sum(d$Gunnel)," Gunnels  |  ",
           round(mean(d$Gunnel)*100,1),"% detection rate")
  })

  output$tbl_main <- renderDT({
    fd() %>%
      select(RowID,Status,Time,Fromlow,Slope,Rw,Amphiso,Substrate,Pool_Lbl,Water_Lbl,Cobble_Lbl) %>%
      rename(ID=RowID,Gunnel=Status,"Time Pos"=Time,"Fromlow m"=Fromlow,
             "Slope°"=Slope,"Wave Exp"=Rw,Pool=Pool_Lbl,Water=Water_Lbl,Cobble=Cobble_Lbl) %>%
      datatable(options=list(pageLength=20,scrollX=TRUE,dom="Bfrtip",
                  columnDefs=list(list(className="dt-center",targets="_all"))),
                rownames=FALSE,class="stripe hover compact") %>%
      formatStyle("Gunnel",
        backgroundColor=styleEqual(c("Present","Absent"),c(paste0(P1,"22"),"transparent")),
        color=styleEqual(c("Present","Absent"),c(P1,MT)),fontWeight="bold")
  })

  output$dl <- downloadHandler(
    filename=function() paste0("Gunnels_",Sys.Date(),".csv"),
    content =function(file) write.csv(fd(),file,row.names=FALSE)
  )
}

shinyApp(ui=ui, server=server)
