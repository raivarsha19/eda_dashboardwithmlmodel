# ============================================================
# 🎨 EDA PRO DASHBOARD — Full-Featured Shiny App
# ============================================================
# Install required packages (run once):
# install.packages(c("shiny","shinydashboard","shinydashboardPlus",
#   "shinyWidgets","DT","ggplot2","plotly","dplyr","tidyr","readr",
#   "corrplot","GGally","skimr","janitor","RColorBrewer","viridis",
#   "scales","zip","tools","httr","stringr","moments","ggcorrplot",
#   "ggthemes","bslib","shinycssloaders","forecast","lubridate"))
# ============================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(readr)
library(corrplot)
library(skimr)
library(janitor)
library(RColorBrewer)
library(viridis)
library(scales)
library(zip)
library(tools)
library(httr)
library(stringr)
library(moments)
library(ggcorrplot)
library(ggthemes)
library(shinycssloaders)

# ─── COLOUR PALETTE ────────────────────────────────────────
PALETTE <- c("#FF6B6B","#FFD93D","#6BCB77","#4D96FF",
             "#C77DFF","#FF9F45","#2EC4B6","#E63946",
             "#F4A261","#264653")

GRAD_CSS <- "
  body, .wrapper { background: #0F0F1A !important; }
  .skin-blue .main-header .navbar,
  .skin-blue .main-header .logo { background: #12122A !important; border-bottom: 2px solid #4D96FF; }
  .skin-blue .main-sidebar { background: #12122A !important; }
  .skin-blue .sidebar a { color: #c9d1e3 !important; }
  .skin-blue .sidebar-menu > li.active > a,
  .skin-blue .sidebar-menu > li:hover > a { background: #1E1E3F !important; border-left: 4px solid #4D96FF !important; color: #fff !important; }
  .content-wrapper, .right-side { background: #0F0F1A !important; }
  .box { background: #1A1A2E !important; border-top: 3px solid #4D96FF; color: #e0e6f0 !important; border-radius: 12px !important; box-shadow: 0 4px 24px #0008; }
  .box .box-header { color: #e0e6f0 !important; }
  .box .box-title { color: #4D96FF !important; font-weight: 700; letter-spacing: 1px; }
  .small-box { border-radius: 12px !important; box-shadow: 0 4px 20px #0006 !important; }
  .small-box h3 { font-size: 2rem; }
  .nav-tabs > li.active > a { background: #1A1A2E !important; color: #4D96FF !important; border-top: 3px solid #4D96FF !important; }
  .nav-tabs > li > a { background: #12122A !important; color: #c9d1e3 !important; }
  .tab-content { background: #1A1A2E !important; padding: 15px; border-radius: 0 0 12px 12px; }
  .selectize-input, .form-control { background: #12122A !important; color: #e0e6f0 !important; border: 1px solid #2a2a5a !important; }
  .selectize-dropdown { background: #1A1A2E !important; color: #e0e6f0 !important; }
  .dataTables_wrapper { color: #e0e6f0 !important; }
  table.dataTable { background: #1A1A2E !important; color: #e0e6f0 !important; }
  table.dataTable thead th { background: #12122A !important; color: #4D96FF !important; }
  .progress-bar { background: linear-gradient(90deg,#4D96FF,#C77DFF) !important; }
  label { color: #c9d1e3 !important; }
  h1,h2,h3,h4,h5 { color: #e0e6f0 !important; }
  .insight-card { background: #1E1E3F; border-left: 4px solid #6BCB77; padding: 12px 16px; border-radius: 8px; margin-bottom: 10px; color: #e0e6f0; }
  .warn-card   { background: #1E1E3F; border-left: 4px solid #FFD93D; padding: 12px 16px; border-radius: 8px; margin-bottom: 10px; color: #e0e6f0; }
  .danger-card { background: #1E1E3F; border-left: 4px solid #FF6B6B; padding: 12px 16px; border-radius: 8px; margin-bottom: 10px; color: #e0e6f0; }
  .upload-area { border: 2px dashed #4D96FF !important; border-radius: 12px; padding: 20px; background: #12122A !important; }
  .badge-pill { display: inline-block; padding: 3px 10px; border-radius: 20px; font-size: 11px; font-weight: 700; margin: 2px; }
  .btn-primary { background: linear-gradient(135deg,#4D96FF,#C77DFF) !important; border: none !important; border-radius: 8px !important; font-weight: 600; }
  .btn-success { background: linear-gradient(135deg,#6BCB77,#2EC4B6) !important; border: none !important; border-radius: 8px !important; font-weight: 600; }
  .section-title { font-size: 13px; text-transform: uppercase; letter-spacing: 2px; color: #4D96FF; font-weight: 700; margin-bottom: 10px; }
  ::-webkit-scrollbar { width: 6px; } ::-webkit-scrollbar-track { background: #12122A; } ::-webkit-scrollbar-thumb { background: #4D96FF; border-radius: 3px; }
"

dark_theme <- theme(
  plot.background   = element_rect(fill = "#1A1A2E", color = NA),
  panel.background  = element_rect(fill = "#12122A", color = NA),
  panel.grid.major  = element_line(color = "#2a2a5a", size = 0.4),
  panel.grid.minor  = element_blank(),
  axis.text         = element_text(color = "#c9d1e3", size = 10),
  axis.title        = element_text(color = "#4D96FF", size = 11, face = "bold"),
  plot.title        = element_text(color = "#e0e6f0", size = 14, face = "bold", hjust = 0.5),
  plot.subtitle     = element_text(color = "#c9d1e3", size = 10, hjust = 0.5),
  legend.background = element_rect(fill = "#1A1A2E"),
  legend.text       = element_text(color = "#c9d1e3"),
  legend.title      = element_text(color = "#4D96FF", face = "bold"),
  strip.background  = element_rect(fill = "#12122A"),
  strip.text        = element_text(color = "#4D96FF", face = "bold")
)

# ─── UI ────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://img.icons8.com/fluency/32/combo-chart.png",
               style = "vertical-align:middle;margin-right:8px;"),
      tags$span("EDA PRO", style = "font-weight:800;letter-spacing:2px;color:#4D96FF;font-size:20px;")
    ),
    titleWidth = 240
  ),

  dashboardSidebar(
    width = 240,
    tags$div(style = "padding:15px 10px 5px;",
             tags$p(class = "section-title", "📂 Data Source")),
    sidebarMenu(
      id = "tabs",
      menuItem("🏠 Home",           tabName = "home",       icon = icon("home")),
      menuItem("📊 Overview",        tabName = "overview",   icon = icon("table")),
      menuItem("📈 Distributions",   tabName = "dist",       icon = icon("chart-bar")),
      menuItem("🔗 Correlations",    tabName = "corr",       icon = icon("project-diagram")),
      menuItem("🤖 Smart Charts",    tabName = "smart",      icon = icon("magic")),
      menuItem("💡 Business Insights",tabName = "biz",       icon = icon("lightbulb")),
      menuItem("🔍 Data Explorer",   tabName = "explorer",   icon = icon("search")),
      menuItem("🤖 Prediction Model", tabName = "predict",    icon = icon("robot"))
    ),
    tags$hr(style = "border-color:#2a2a5a;"),
    tags$div(style = "padding:0 10px;",
             tags$p(class = "section-title", "⚙ Controls"),
             uiOutput("col_selector_ui"),
             uiOutput("color_palette_ui")
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML(GRAD_CSS))),

    tabItems(
      # ── HOME ──────────────────────────────────────────────
      tabItem("home",
        fluidRow(
          box(width = 12, title = "📂 Load Your Dataset", solidHeader = TRUE,
            fluidRow(
              column(4,
                tags$div(class = "upload-area",
                  fileInput("csv_file", "Upload CSV / ZIP containing CSV",
                            accept = c(".csv",".zip"), buttonLabel = "Browse"),
                  tags$small(style="color:#c9d1e3;","Supports .csv and .zip (CSV inside)")
                )
              ),
              column(4,
                tags$br(),
                textInput("url_input", "Or paste a CSV URL", placeholder = "https://...data.csv"),
                actionButton("load_url", "🌐 Load from URL", class = "btn-primary", style="margin-top:5px;")
              ),
              column(4,
                tags$br(),
                tags$div(class = "insight-card",
                  tags$b("💡 Quick Start"),
                  tags$ul(style="padding-left:18px;margin:5px 0;",
                    tags$li("Upload any CSV file"),
                    tags$li("Upload a ZIP with CSV inside"),
                    tags$li("Paste a public CSV URL"),
                    tags$li("Navigate tabs for analysis")
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          valueBoxOutput("vbox_rows",   width = 3),
          valueBoxOutput("vbox_cols",   width = 3),
          valueBoxOutput("vbox_num",    width = 3),
          valueBoxOutput("vbox_miss",   width = 3)
        ),
        fluidRow(
          box(width = 6, title = "📋 Data Preview (first 10 rows)", solidHeader = TRUE,
            withSpinner(DTOutput("preview_table"), color = "#4D96FF")),
          box(width = 6, title = "🧬 Column Info", solidHeader = TRUE,
            withSpinner(DTOutput("col_info_table"), color = "#4D96FF"))
        )
      ),

      # ── OVERVIEW ──────────────────────────────────────────
      tabItem("overview",
        fluidRow(
          box(width = 12, title = "📊 Statistical Summary", solidHeader = TRUE,
            withSpinner(DTOutput("summary_table"), color = "#4D96FF"))
        ),
        fluidRow(
          box(width = 6, title = "❓ Missing Values Heatmap", solidHeader = TRUE,
            withSpinner(plotlyOutput("missing_plot", height = "380px"), color = "#4D96FF")),
          box(width = 6, title = "🏷 Data Types Breakdown", solidHeader = TRUE,
            withSpinner(plotlyOutput("dtype_plot",   height = "380px"), color = "#4D96FF"))
        )
      ),

      # ── DISTRIBUTIONS ─────────────────────────────────────
      tabItem("dist",
        fluidRow(
          box(width = 3, title = "⚙ Options", solidHeader = TRUE,
            uiOutput("dist_col_ui"),
            selectInput("dist_type", "Chart Type",
                        choices = c("Histogram","Density","Box Plot","Violin","QQ Plot"), selected = "Histogram"),
            sliderInput("hist_bins", "Histogram Bins", 5, 100, 30),
            checkboxInput("show_normal", "Overlay Normal Curve", FALSE)
          ),
          box(width = 9, title = "📈 Distribution View", solidHeader = TRUE,
            withSpinner(plotlyOutput("dist_plot", height = "420px"), color = "#4D96FF"))
        ),
        fluidRow(
          box(width = 12, title = "🗂 All Numeric Distributions (Grid)", solidHeader = TRUE,
            withSpinner(plotlyOutput("dist_grid", height = "520px"), color = "#4D96FF"))
        )
      ),

      # ── CORRELATIONS ──────────────────────────────────────
      tabItem("corr",
        fluidRow(
          box(width = 3, title = "⚙ Options", solidHeader = TRUE,
            selectInput("corr_method", "Method", choices = c("pearson","spearman","kendall")),
            selectInput("corr_style",  "Style",  choices = c("Heatmap","Circle","Number")),
            sliderInput("corr_thresh", "Highlight |r| ≥", 0, 1, 0.5, step = 0.05)
          ),
          box(width = 9, title = "🔗 Correlation Matrix", solidHeader = TRUE,
            withSpinner(plotlyOutput("corr_plot", height = "500px"), color = "#4D96FF"))
        ),
        fluidRow(
          box(width = 12, title = "📉 Top Correlated Pairs", solidHeader = TRUE,
            withSpinner(DTOutput("corr_pairs"), color = "#4D96FF"))
        )
      ),

      # ── SMART CHARTS ──────────────────────────────────────
      tabItem("smart",
        fluidRow(
          box(width = 12, title = "🤖 AI-Recommended Charts for Your Dataset", solidHeader = TRUE,
            tags$div(class = "warn-card",
              "🔍 Charts below are auto-selected based on data types, cardinality & distributions."
            ),
            withSpinner(uiOutput("smart_charts_ui"), color = "#4D96FF")
          )
        )
      ),

      # ── BUSINESS INSIGHTS ─────────────────────────────────
      tabItem("biz",
        fluidRow(
          box(width = 12, title = "💡 Automated Business Insights", solidHeader = TRUE,
            withSpinner(uiOutput("biz_insights_ui"), color = "#4D96FF"))
        ),
        fluidRow(
          box(width = 6, title = "📌 Outlier Detection", solidHeader = TRUE,
            withSpinner(plotlyOutput("outlier_plot", height = "400px"), color = "#4D96FF")),
          box(width = 6, title = "📊 Top Categories by Volume", solidHeader = TRUE,
            withSpinner(plotlyOutput("cat_bar_plot", height = "400px"), color = "#4D96FF"))
        )
      ),

      # ── DATA EXPLORER ─────────────────────────────────────
      tabItem("explorer",
        fluidRow(
          box(width = 4, title = "⚙ Scatter Config", solidHeader = TRUE,
            uiOutput("scatter_x_ui"),
            uiOutput("scatter_y_ui"),
            uiOutput("scatter_col_ui"),
            uiOutput("scatter_size_ui"),
            checkboxInput("scatter_smooth","Add Trend Line", TRUE)
          ),
          box(width = 8, title = "🔍 Interactive Scatter", solidHeader = TRUE,
            withSpinner(plotlyOutput("scatter_plot", height = "420px"), color = "#4D96FF"))
        ),
        fluidRow(
          box(width = 12, title = "📋 Filtered Data Table", solidHeader = TRUE,
            withSpinner(DTOutput("full_table"), color = "#4D96FF"))
        )
      ),

      # ── PREDICTION MODEL ─────────────────────────────────
      tabItem("predict",
        fluidRow(
          box(width = 4, title = "🤖 Passenger Satisfaction Prediction", solidHeader = TRUE,
            tags$div(class = "warn-card",
              tags$b("Model Type: "), "Classification using Logistic Regression"
            ),
            numericInput("boarding", "Online Boarding Rating", value = 3, min = 1, max = 5),
            numericInput("comfort", "Seat Comfort Rating", value = 3, min = 1, max = 5),
            numericInput("entertainment", "Inflight Entertainment Rating", value = 3, min = 1, max = 5),
            numericInput("cleanliness", "Cleanliness Rating", value = 3, min = 1, max = 5),
            numericInput("delay", "Departure Delay in Minutes", value = 10, min = 0),
            actionButton("predict_btn", "Predict Satisfaction", class = "btn-success")
          ),
          box(width = 8, title = "📊 Prediction Result", solidHeader = TRUE,
            tags$br(),
            h2(textOutput("prediction_result")),
            tags$br(),
            tags$div(class = "insight-card",
              tags$b("How it works: "),
              "The model uses selected service ratings and departure delay to predict whether a passenger is likely to be satisfied."
            ),
            tags$br(),
            h4("Model Accuracy"),
            verbatimTextOutput("model_accuracy"),
            tags$br(),
            h4("Required Columns Check"),
            verbatimTextOutput("required_cols_check")
          )
        )
      )
    )
  )
)

# ─── SERVER ────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Reactive: load data ───────────────────────────────────
  raw_data <- reactiveVal(NULL)

  observeEvent(input$csv_file, {
    req(input$csv_file)
    path <- input$csv_file$datapath
    ext  <- tolower(file_ext(input$csv_file$name))

    df <- tryCatch({
      if (ext == "zip") {
        tmp <- tempdir()
        utils::unzip(path, exdir = tmp)
        csvs <- list.files(tmp, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
        if (length(csvs) == 0) stop("No CSV found in ZIP")
        read_csv(csvs[1], show_col_types = FALSE)
      } else {
        read_csv(path, show_col_types = FALSE)
      }
    }, error = function(e) { showNotification(paste("Error:", e$message), type="error"); NULL })

    if (!is.null(df)) {
      df <- clean_names(df)
      raw_data(df)
      showNotification("✅ Dataset loaded successfully!", type = "message")
      updateTabItems(session, "tabs", "overview")
    }
  })

  observeEvent(input$load_url, {
    req(input$url_input)
    url <- trimws(input$url_input)
    df <- tryCatch({
      tmp <- tempfile(fileext = ".csv")
      GET(url, write_disk(tmp, overwrite = TRUE))
      read_csv(tmp, show_col_types = FALSE)
    }, error = function(e) { showNotification(paste("URL Error:", e$message), type="error"); NULL })
    if (!is.null(df)) {
      df <- clean_names(df)
      raw_data(df)
      showNotification("✅ Dataset loaded from URL!", type = "message")
      updateTabItems(session, "tabs", "overview")
    }
  })

  # ── Helper columns ────────────────────────────────────────
  num_cols <- reactive({
    req(raw_data())
    names(raw_data())[sapply(raw_data(), is.numeric)]
  })
  cat_cols <- reactive({
    req(raw_data())
    names(raw_data())[sapply(raw_data(), function(x) is.character(x)|is.factor(x))]
  })
  all_cols <- reactive({ req(raw_data()); names(raw_data()) })

  # ── Value Boxes ───────────────────────────────────────────
  output$vbox_rows <- renderValueBox({
    n <- if (!is.null(raw_data())) nrow(raw_data()) else 0
    valueBox(format(n, big.mark=","), "Total Rows", icon=icon("database"),
             color="blue")
  })
  output$vbox_cols <- renderValueBox({
    n <- if (!is.null(raw_data())) ncol(raw_data()) else 0
    valueBox(n, "Columns", icon=icon("columns"), color="purple")
  })
  output$vbox_num <- renderValueBox({
    n <- if (!is.null(raw_data())) length(num_cols()) else 0
    valueBox(n, "Numeric Cols", icon=icon("hashtag"), color="green")
  })
  output$vbox_miss <- renderValueBox({
    pct <- if (!is.null(raw_data())) {
      m <- sum(is.na(raw_data()))
      t <- prod(dim(raw_data()))
      paste0(round(100*m/t,1),"%")
    } else "0%"
    valueBox(pct, "Missing Data", icon=icon("exclamation-triangle"), color="red")
  })

  # ── Preview ───────────────────────────────────────────────
  output$preview_table <- renderDT({
    req(raw_data())
    datatable(head(raw_data(),10), options=list(scrollX=TRUE,dom="t",pageLength=10),
              style="bootstrap", class="compact")
  })

  output$col_info_table <- renderDT({
    req(raw_data())
    df <- raw_data()
    info <- data.frame(
      Column   = names(df),
      Type     = sapply(df, function(x) class(x)[1]),
      Unique   = sapply(df, function(x) length(unique(x))),
      Missing  = sapply(df, function(x) sum(is.na(x))),
      Miss_Pct = paste0(round(100*sapply(df,function(x)mean(is.na(x))),1),"%")
    )
    datatable(info, options=list(scrollX=TRUE,dom="t",pageLength=20),
              style="bootstrap", class="compact")
  })

  # ── Summary ───────────────────────────────────────────────
  output$summary_table <- renderDT({
    req(raw_data(), length(num_cols())>0)
    df <- raw_data()[, num_cols(), drop=FALSE]
    sm <- do.call(rbind, lapply(names(df), function(col){
      x <- df[[col]][!is.na(df[[col]])]
      data.frame(
        Column = col, N = length(x),
        Mean   = round(mean(x),3),
        Median = round(median(x),3),
        SD     = round(sd(x),3),
        Min    = round(min(x),3),
        Max    = round(max(x),3),
        Skewness = round(skewness(x),3),
        Kurtosis = round(kurtosis(x),3),
        Outliers = sum(x < (quantile(x,.25)-1.5*IQR(x)) |
                       x > (quantile(x,.75)+1.5*IQR(x)))
      )
    }))
    datatable(sm, options=list(scrollX=TRUE,pageLength=15), style="bootstrap", class="compact")
  })

  # ── Missing plot ──────────────────────────────────────────
  output$missing_plot <- renderPlotly({
    req(raw_data())
    df   <- raw_data()
    miss <- data.frame(
      Column  = names(df),
      Missing = sapply(df, function(x) 100*mean(is.na(x)))
    ) %>% arrange(desc(Missing))
    p <- ggplot(miss, aes(x=reorder(Column,Missing), y=Missing,
                           fill=Missing, text=paste0(Column,": ",round(Missing,1),"%"))) +
      geom_col(width=0.7) +
      scale_fill_gradient(low="#4D96FF", high="#FF6B6B") +
      coord_flip() +
      labs(title="Missing % per Column", x="", y="Missing (%)") +
      dark_theme + theme(legend.position="none")
    ggplotly(p, tooltip="text") %>%
      layout(paper_bgcolor="#1A1A2E", plot_bgcolor="#12122A",
             font=list(color="#e0e6f0"))
  })

  output$dtype_plot <- renderPlotly({
    req(raw_data())
    df <- raw_data()
    types <- table(sapply(df, function(x) class(x)[1]))
    pie_df <- data.frame(Type=names(types), Count=as.numeric(types))
    plot_ly(pie_df, labels=~Type, values=~Count, type="pie",
            marker=list(colors=PALETTE[1:nrow(pie_df)]),
            textinfo="label+percent") %>%
      layout(title=list(text="Column Data Types",font=list(color="#e0e6f0")),
             paper_bgcolor="#1A1A2E", plot_bgcolor="#1A1A2E",
             font=list(color="#e0e6f0"), legend=list(font=list(color="#e0e6f0")))
  })

  # ── Distribution UI ───────────────────────────────────────
  output$dist_col_ui <- renderUI({
    req(num_cols())
    selectInput("dist_col","Select Column", choices=num_cols(), selected=num_cols()[1])
  })

  output$dist_plot <- renderPlotly({
    req(raw_data(), input$dist_col)
    df  <- raw_data()
    col <- input$dist_col
    x   <- df[[col]][!is.na(df[[col]])]

    p <- switch(input$dist_type,
      "Histogram" = {
        gg <- ggplot(data.frame(x=x), aes(x)) +
          geom_histogram(bins=input$hist_bins, fill="#4D96FF", color="#12122A", alpha=0.85)
        if (input$show_normal) {
          gg <- gg + stat_function(fun=function(t) dnorm(t,mean(x),sd(x))*length(x)*(max(x)-min(x))/input$hist_bins,
                                   color="#FFD93D", size=1.2)
        }
        gg + labs(title=paste("Histogram of", col), x=col, y="Count") + dark_theme
      },
      "Density"  = ggplot(data.frame(x=x),aes(x)) +
        geom_density(fill="#C77DFF",alpha=0.6,color="#e0e6f0") +
        labs(title=paste("Density of",col),x=col,y="Density") + dark_theme,
      "Box Plot" = ggplot(data.frame(x=x),aes(y=x,x="")) +
        geom_boxplot(fill="#6BCB77",color="#e0e6f0",outlier.color="#FF6B6B",outlier.size=2) +
        labs(title=paste("Box Plot of",col),x="",y=col) + dark_theme,
      "Violin"   = ggplot(data.frame(x=x),aes(y=x,x="")) +
        geom_violin(fill="#FF9F45",alpha=0.7,color="#e0e6f0") +
        geom_boxplot(width=0.1,fill="#1A1A2E",color="#e0e6f0") +
        labs(title=paste("Violin of",col),x="",y=col) + dark_theme,
      "QQ Plot"  = {
        qq <- qqnorm(x, plot.it=FALSE)
        ggplot(data.frame(sample=qq$x,theoretical=qq$y),aes(x=theoretical,y=sample)) +
          geom_point(color="#4D96FF",alpha=0.6,size=1.5) +
          geom_smooth(method="lm",color="#FFD93D",se=FALSE) +
          labs(title=paste("QQ Plot of",col),x="Theoretical",y="Sample") + dark_theme
      }
    )
    ggplotly(p) %>%
      layout(paper_bgcolor="#1A1A2E", plot_bgcolor="#12122A", font=list(color="#e0e6f0"))
  })

  output$dist_grid <- renderPlotly({
    req(raw_data(), length(num_cols())>0)
    df   <- raw_data()
    cols <- head(num_cols(), 12)
    plots <- list()
    for (col in cols) {
      x <- df[[col]][!is.na(df[[col]])]
      plots[[col]] <- plot_ly(x=x, type="histogram", name=col,
                               marker=list(color=sample(PALETTE,1)),
                               showlegend=FALSE) %>%
        layout(xaxis=list(title=col,color="#c9d1e3"),
               yaxis=list(title="",color="#c9d1e3"),
               paper_bgcolor="#1A1A2E", plot_bgcolor="#12122A")
    }
    nc <- min(3, length(cols))
    nr <- ceiling(length(cols)/nc)
    subplot(plots, nrows=nr, shareX=FALSE, shareY=FALSE, titleX=TRUE) %>%
      layout(title=list(text="All Numeric Distributions",font=list(color="#e0e6f0")),
             paper_bgcolor="#1A1A2E", plot_bgcolor="#12122A",
             font=list(color="#e0e6f0"))
  })

  # ── Correlations ──────────────────────────────────────────
  output$corr_plot <- renderPlotly({
    req(raw_data(), length(num_cols())>=2)
    df  <- raw_data()[, num_cols(), drop=FALSE]
    cm  <- cor(df, use="pairwise.complete.obs", method=input$corr_method)
    cols <- colorRampPalette(c("#FF6B6B","#1A1A2E","#4D96FF"))(100)
    p <- ggcorrplot(cm, method=if(input$corr_style=="Circle")"circle" else "square",
                    type="lower", lab=input$corr_style=="Number",
                    colors=c("#FF6B6B","#1A1A2E","#4D96FF"),
                    outline.color="#12122A", title="Correlation Matrix") +
      dark_theme
    ggplotly(p) %>%
      layout(paper_bgcolor="#1A1A2E", plot_bgcolor="#12122A",
             font=list(color="#e0e6f0"))
  })

  output$corr_pairs <- renderDT({
    req(raw_data(), length(num_cols())>=2)
    df <- raw_data()[, num_cols(), drop=FALSE]
    cm <- cor(df, use="pairwise.complete.obs", method=input$corr_method)
    pairs <- which(lower.tri(cm), arr.ind=TRUE)
    pair_df <- data.frame(
      Var1 = rownames(cm)[pairs[,1]],
      Var2 = colnames(cm)[pairs[,2]],
      Correlation = round(cm[pairs],4)
    ) %>% arrange(desc(abs(Correlation))) %>%
      filter(abs(Correlation) >= input$corr_thresh)
    datatable(pair_df, options=list(pageLength=10,scrollX=TRUE),
              style="bootstrap", class="compact")
  })

  # ── Smart Charts ──────────────────────────────────────────
  output$smart_charts_ui <- renderUI({
    req(raw_data())
    df <- raw_data()
    nc <- num_cols(); cc <- cat_cols()

    plots <- list()

    # 1) If date-like column exists → time series
    date_cols <- names(df)[sapply(df, function(x) inherits(x,"Date")|inherits(x,"POSIXct")|
                                    (is.character(x)&&any(grepl("\\d{4}",x[1:5]))))]
    if (length(date_cols)>0 && length(nc)>0) {
      plots[["time"]] <- box(width=12, title="📅 Time Trend", solidHeader=TRUE,
        renderPlotly({
          d <- df; d[[date_cols[1]]] <- as.Date(d[[date_cols[1]]], tryFormats=c("%Y-%m-%d","%d/%m/%Y","%m/%d/%Y"))
          d <- d[!is.na(d[[date_cols[1]]]),]
          d <- d %>% arrange(.data[[date_cols[1]]])
          p <- ggplot(d, aes(x=.data[[date_cols[1]]], y=.data[[nc[1]]])) +
            geom_line(color="#4D96FF",size=1) +
            geom_area(fill="#4D96FF",alpha=0.2) +
            labs(title=paste(nc[1],"over Time"),x=date_cols[1],y=nc[1]) + dark_theme
          ggplotly(p) %>% layout(paper_bgcolor="#1A1A2E",plot_bgcolor="#12122A",font=list(color="#e0e6f0"))
        })
      )
    }

    # 2) Categorical bar
    if (length(cc)>0 && length(nc)>0) {
      best_cat <- cc[which.min(sapply(cc,function(c)length(unique(df[[c]]))))]
      if (length(unique(df[[best_cat]])) <= 20) {
        plots[["cat_bar"]] <- box(width=6, title=paste("🏷", best_cat, "vs", nc[1]), solidHeader=TRUE,
          renderPlotly({
            agg <- df %>% group_by(.data[[best_cat]]) %>%
              summarise(val=mean(.data[[nc[1]]],na.rm=TRUE),.groups="drop") %>%
              arrange(desc(val))
            p <- ggplot(agg, aes(x=reorder(.data[[best_cat]],val),y=val,fill=val,
                                  text=paste0(.data[[best_cat]],": ",round(val,2)))) +
              geom_col(width=0.7) +
              scale_fill_gradientn(colors=c("#4D96FF","#C77DFF","#FF6B6B")) +
              coord_flip() +
              labs(title=paste("Avg",nc[1],"by",best_cat),x="",y=paste("Avg",nc[1])) +
              dark_theme + theme(legend.position="none")
            ggplotly(p,tooltip="text") %>%
              layout(paper_bgcolor="#1A1A2E",plot_bgcolor="#12122A",font=list(color="#e0e6f0"))
          })
        )
      }
    }

    # 3) Scatter (top 2 numeric)
    if (length(nc)>=2) {
      plots[["scatter"]] <- box(width=6, title=paste("⚡",nc[1],"vs",nc[2]), solidHeader=TRUE,
        renderPlotly({
          p <- ggplot(df, aes(x=.data[[nc[1]]],y=.data[[nc[2]]],
                               text=paste0(nc[1],": ",.data[[nc[1]]],"<br>",nc[2],": ",.data[[nc[2]]]))) +
            geom_point(color="#FFD93D",alpha=0.6,size=1.8) +
            geom_smooth(method="lm",color="#FF6B6B",se=TRUE,fill="#FF6B6B",alpha=0.15) +
            labs(title=paste(nc[1],"vs",nc[2])) + dark_theme
          ggplotly(p,tooltip="text") %>%
            layout(paper_bgcolor="#1A1A2E",plot_bgcolor="#12122A",font=list(color="#e0e6f0"))
        })
      )
    }

    # 4) Pie for low-cardinality cat
    if (length(cc)>0) {
      pie_col <- cc[which.min(sapply(cc,function(c)length(unique(df[[c]]))))]
      if (length(unique(df[[pie_col]])) <= 8) {
        plots[["pie"]] <- box(width=6, title=paste("🥧 Distribution:", pie_col), solidHeader=TRUE,
          renderPlotly({
            counts <- table(df[[pie_col]])
            pie_df <- data.frame(Cat=names(counts),Count=as.numeric(counts))
            plot_ly(pie_df,labels=~Cat,values=~Count,type="pie",
                    marker=list(colors=PALETTE[1:nrow(pie_df)]),
                    textinfo="label+percent") %>%
              layout(paper_bgcolor="#1A1A2E",font=list(color="#e0e6f0"),
                     legend=list(font=list(color="#e0e6f0")))
          })
        )
      }
    }

    # 5) Heatmap (numeric cols)
    if (length(nc)>=3) {
      top_nc <- head(nc, 8)
      plots[["heatmap"]] <- box(width=6, title="🌡 Numeric Heatmap (sample)", solidHeader=TRUE,
        renderPlotly({
          samp <- df %>% slice_sample(n=min(50,nrow(df))) %>% select(all_of(top_nc))
          samp_mat <- as.matrix(samp)
          plot_ly(z=samp_mat,type="heatmap",colorscale="Viridis",
                  x=top_nc,showscale=TRUE) %>%
            layout(title=list(text="Sample Heatmap",font=list(color="#e0e6f0")),
                   paper_bgcolor="#1A1A2E",font=list(color="#e0e6f0"),
                   xaxis=list(color="#c9d1e3"),yaxis=list(color="#c9d1e3"))
        })
      )
    }

    do.call(fluidRow, plots)
  })

  # ── Business Insights ─────────────────────────────────────
  output$biz_insights_ui <- renderUI({
    req(raw_data())
    df <- raw_data(); nc <- num_cols(); cc <- cat_cols()
    insights <- list()

    # Row count
    insights[[1]] <- tags$div(class="insight-card",
      tags$b("📐 Dataset Scale: "),
      sprintf("The dataset has %s rows and %s columns — a %s dataset.",
              format(nrow(df),big.mark=","), ncol(df),
              if(nrow(df)>100000)"large-scale" else if(nrow(df)>10000)"medium" else "small"))

    # Missing
    miss_pct <- 100*sum(is.na(df))/prod(dim(df))
    cls <- if(miss_pct>20)"danger-card" else if(miss_pct>5)"warn-card" else "insight-card"
    insights[[2]] <- tags$div(class=cls,
      tags$b("❓ Data Completeness: "),
      sprintf("%.1f%% of values are missing. %s", miss_pct,
              if(miss_pct>20)"⚠ Critical — imputation or removal needed before modeling."
              else if(miss_pct>5)"Consider imputation strategies."
              else "✅ Excellent data completeness."))

    # Skewness
    if (length(nc)>0) {
      skews <- sapply(nc, function(c){ x<-df[[c]][!is.na(df[[c]])]; skewness(x) })
      high_skew <- names(skews[abs(skews)>1])
      if (length(high_skew)>0)
        insights[[3]] <- tags$div(class="warn-card",
          tags$b("📐 Skewed Distributions: "),
          paste0(paste(head(high_skew,5),collapse=", "),
                 " show high skewness. Log/sqrt transform may improve model performance."))
    }

    # Outliers
    if (length(nc)>0) {
      out_counts <- sapply(nc, function(c){
        x<-df[[c]][!is.na(df[[c]])]; q<-quantile(x,c(.25,.75)); iqr<-IQR(x)
        sum(x<(q[1]-1.5*iqr)|x>(q[2]+1.5*iqr))
      })
      top_out <- sort(out_counts,decreasing=TRUE)[1]
      insights[[4]] <- tags$div(class=if(top_out>nrow(df)*0.05)"danger-card" else "insight-card",
        tags$b("🚨 Outliers: "),
        sprintf("Column '%s' has %d outliers (%.1f%% of rows). %s",
                names(top_out), top_out, 100*top_out/nrow(df),
                if(top_out>nrow(df)*0.05)"Investigate before modeling." else "Within acceptable range."))
    }

    # Cardinality
    if (length(cc)>0) {
      cards <- sapply(cc, function(c) length(unique(df[[c]])))
      high_card <- names(cards[cards>50])
      if (length(high_card)>0)
        insights[[5]] <- tags$div(class="warn-card",
          tags$b("🏷 High Cardinality: "),
          paste0(paste(head(high_card,4),collapse=", "),
                 " have >50 unique values. Consider grouping/encoding."))
    }

    # Correlation alert
    if (length(nc)>=2) {
      cm  <- cor(df[,nc,drop=FALSE], use="pairwise.complete.obs")
      ut  <- upper.tri(cm)
      max_cor <- max(abs(cm[ut]))
      idx <- which(abs(cm)==max_cor & ut, arr.ind=TRUE)[1,]
      insights[[6]] <- tags$div(class=if(max_cor>0.9)"danger-card" else "insight-card",
        tags$b("🔗 Correlation: "),
        sprintf("Strongest pair: %s & %s (r = %.2f). %s",
                rownames(cm)[idx[1]], colnames(cm)[idx[2]], max_cor,
                if(max_cor>0.9)"⚠ Possible multicollinearity — check for redundancy."
                else if(max_cor>0.7)"Strong relationship — useful for predictive modeling."
                else "Moderate correlations."))
    }

    # Class balance (binary cat)
    binary_cats <- cc[sapply(cc, function(c) length(unique(na.omit(df[[c]])))==2)]
    if (length(binary_cats)>0) {
      bc <- binary_cats[1]
      tbl <- table(df[[bc]]); mn <- min(tbl)/max(tbl)
      cls2 <- if(mn<0.2)"danger-card" else "insight-card"
      insights[[7]] <- tags$div(class=cls2,
        tags$b(paste0("⚖ Class Balance (", bc, "): ")),
        sprintf("Ratio %.2f. %s", mn,
                if(mn<0.2)"Significant imbalance — consider SMOTE/weighting for classification."
                else "✅ Reasonably balanced classes."))
    }

    # Recommendation
    insights[[8]] <- tags$div(class="insight-card",
      tags$b("🎯 Modeling Recommendation: "),
      if(length(binary_cats)>0) paste0("Binary target detected ('",binary_cats[1],"'). Consider: Logistic Regression, Random Forest, XGBoost.")
      else if(length(nc)>0) "Continuous target likely. Consider: Linear Regression, Gradient Boosting, Neural Nets."
      else "Primarily categorical data. Consider: Clustering, Association Rules, Chi-square tests.")

    tagList(insights)
  })

  # ── Outlier Plot ──────────────────────────────────────────
  output$outlier_plot <- renderPlotly({
    req(raw_data(), length(num_cols())>0)
    df <- raw_data()
    nc <- head(num_cols(), 8)
    long <- df %>% select(all_of(nc)) %>%
      pivot_longer(everything(), names_to="Variable", values_to="Value") %>%
      filter(!is.na(Value))
    p <- ggplot(long, aes(x=Variable, y=Value, fill=Variable)) +
      geom_boxplot(outlier.color="#FF6B6B",outlier.size=1.5,alpha=0.8) +
      scale_fill_manual(values=PALETTE[1:length(nc)]) +
      coord_flip() +
      labs(title="Outlier Detection (Boxplots)",x="",y="Value") +
      dark_theme + theme(legend.position="none")
    ggplotly(p) %>%
      layout(paper_bgcolor="#1A1A2E",plot_bgcolor="#12122A",font=list(color="#e0e6f0"))
  })

  output$cat_bar_plot <- renderPlotly({
    req(raw_data())
    cc <- cat_cols()
    if (length(cc)==0) return(NULL)
    df <- raw_data()
    best <- cc[which.min(sapply(cc, function(c){ u<-length(unique(df[[c]])); if(u>30)999 else abs(u-10) }))]
    counts <- df %>% count(.data[[best]]) %>% arrange(desc(n)) %>% head(15)
    p <- ggplot(counts,aes(x=reorder(.data[[best]],n),y=n,fill=n,
                            text=paste0(.data[[best]],": ",n))) +
      geom_col(width=0.7) +
      scale_fill_gradientn(colors=c("#4D96FF","#C77DFF","#FF6B6B")) +
      coord_flip() +
      labs(title=paste("Top Categories:", best),x="",y="Count") +
      dark_theme + theme(legend.position="none")
    ggplotly(p,tooltip="text") %>%
      layout(paper_bgcolor="#1A1A2E",plot_bgcolor="#12122A",font=list(color="#e0e6f0"))
  })

  # ── Explorer ──────────────────────────────────────────────
  output$scatter_x_ui   <- renderUI({ req(num_cols()); selectInput("sx","X Axis",   choices=num_cols(), selected=num_cols()[1]) })
  output$scatter_y_ui   <- renderUI({ req(num_cols()); selectInput("sy","Y Axis",   choices=num_cols(), selected=num_cols()[min(2,length(num_cols()))]) })
  output$scatter_col_ui <- renderUI({ req(all_cols()); selectInput("sc","Color By", choices=c("None",all_cols()), selected="None") })
  output$scatter_size_ui<- renderUI({ req(num_cols()); selectInput("ss","Size By",  choices=c("None",num_cols()), selected="None") })

  output$scatter_plot <- renderPlotly({
    req(raw_data(), input$sx, input$sy)
    df <- raw_data()
    p <- ggplot(df, aes(x=.data[[input$sx]], y=.data[[input$sy]]))
    if (input$sc != "None") p <- p + aes(color=as.factor(.data[[input$sc]]))
    if (input$ss != "None") p <- p + aes(size=.data[[input$ss]])
    p <- p + geom_point(alpha=0.65)
    if (input$scatter_smooth) p <- p + geom_smooth(method="lm",color="#FFD93D",se=FALSE)
    p <- p + scale_color_manual(values=PALETTE) +
      labs(title=paste(input$sx,"vs",input$sy)) + dark_theme
    ggplotly(p) %>%
      layout(paper_bgcolor="#1A1A2E",plot_bgcolor="#12122A",font=list(color="#e0e6f0"))
  })

  output$full_table <- renderDT({
    req(raw_data())
    datatable(raw_data(), filter="top",
              options=list(scrollX=TRUE,pageLength=15),
              style="bootstrap", class="compact")
  })

  # ── Prediction Model: Logistic Regression Classification ───
  required_model_cols <- c(
    "satisfaction",
    "online_boarding",
    "seat_comfort",
    "inflight_entertainment",
    "cleanliness",
    "departure_delay_in_minutes"
  )

  output$required_cols_check <- renderPrint({
    req(raw_data())
    missing_cols <- setdiff(required_model_cols, names(raw_data()))

    if (length(missing_cols) == 0) {
      cat("✅ All required columns are available. Model can run.")
    } else {
      cat("❌ Missing columns:\n")
      print(missing_cols)
      cat("\nAvailable columns in your dataset are:\n")
      print(names(raw_data()))
    }
  })

  model_fit <- reactive({
    req(raw_data())

    df <- raw_data()
    missing_cols <- setdiff(required_model_cols, names(df))
    validate(
      need(length(missing_cols) == 0,
           paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    )

    model_df <- df %>%
      select(all_of(required_model_cols)) %>%
      drop_na()

    model_df$satisfaction <- as.factor(model_df$satisfaction)

    validate(
      need(length(levels(model_df$satisfaction)) == 2,
           "Satisfaction column must have exactly 2 classes for logistic regression.")
    )

    glm(
      satisfaction ~ online_boarding + seat_comfort + inflight_entertainment +
        cleanliness + departure_delay_in_minutes,
      data = model_df,
      family = "binomial"
    )
  })

  output$model_accuracy <- renderPrint({
    req(raw_data())

    df <- raw_data()
    missing_cols <- setdiff(required_model_cols, names(df))
    if (length(missing_cols) > 0) {
      cat("Model accuracy cannot be calculated because some required columns are missing.")
      return()
    }

    model_df <- df %>%
      select(all_of(required_model_cols)) %>%
      drop_na()

    model_df$satisfaction <- as.factor(model_df$satisfaction)

    if (length(levels(model_df$satisfaction)) != 2) {
      cat("Accuracy cannot be calculated because satisfaction must have exactly 2 classes.")
      return()
    }

    set.seed(123)
    train_index <- sample(seq_len(nrow(model_df)), size = 0.8 * nrow(model_df))
    train_data <- model_df[train_index, ]
    test_data  <- model_df[-train_index, ]

    temp_model <- glm(
      satisfaction ~ online_boarding + seat_comfort + inflight_entertainment +
        cleanliness + departure_delay_in_minutes,
      data = train_data,
      family = "binomial"
    )

    prob <- predict(temp_model, newdata = test_data, type = "response")
    target_levels <- levels(train_data$satisfaction)

    # In logistic regression, probability belongs to the second factor level.
    predicted_class <- ifelse(prob > 0.5, target_levels[2], target_levels[1])

    accuracy <- mean(predicted_class == test_data$satisfaction)

    cat("Accuracy:", round(accuracy * 100, 2), "%\n\n")
    cat("Confusion Matrix:\n")
    print(table(Actual = test_data$satisfaction, Predicted = predicted_class))
  })

  output$prediction_result <- renderText({
    req(input$predict_btn)

    new_data <- data.frame(
      online_boarding = input$boarding,
      seat_comfort = input$comfort,
      inflight_entertainment = input$entertainment,
      cleanliness = input$cleanliness,
      departure_delay_in_minutes = input$delay
    )

    prob <- predict(model_fit(), newdata = new_data, type = "response")
    target_levels <- levels(model_fit()$model$satisfaction)

    predicted_class <- ifelse(prob > 0.5, target_levels[2], target_levels[1])

    paste0(
      "Prediction: ", toupper(predicted_class),
      " | Probability: ", round(as.numeric(prob) * 100, 2), "%"
    )
  })
}

shinyApp(ui, server)