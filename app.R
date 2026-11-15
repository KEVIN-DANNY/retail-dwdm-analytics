# ==============================================================================
# PB&J RETAIL INTELLIGENCE & OLAP DECISION SUPPORT SYSTEM
# Advanced Data Warehousing & Data Mining (DWDM) Interactive Platform
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(plotly)
  library(ggplot2)
  library(dplyr)
  library(DBI)
  library(RSQLite)
  library(arules)
  library(arulesViz)
  library(visNetwork)
  library(randomForest)
  library(shinyWidgets)
  library(shinycssloaders)
  library(scales)
})

# ------------------------------------------------------------------------------
# 1. INITIALIZE DATA & MODEL ASSETS
# ------------------------------------------------------------------------------
cat("[INFO] Loading data assets and pre-trained models...\n")

# Load existing environment objects from .RData if present
if (file.exists(".RData")) {
  load(".RData")
}

# Connect to the SQLite Data Warehouse
db_path <- "retail_dw.db"
if (!file.exists(db_path)) {
  warning("retail_dw.db not found in current directory. Make sure it exists.")
}

# Ensure pre-computed rule base is available
if (!exists("rules_by_lift") && exists("trans")) {
  cat("[INFO] Mining initial Apriori rules...\n")
  rules_init <- apriori(trans, 
                        parameter = list(supp = 0.015, conf = 0.6, minlen = 2, maxlen = 4),
                        control = list(verbose = FALSE))
  rules_by_lift <- sort(rules_init, by = "lift", decreasing = TRUE)
}

# Pre-mine an expanded rule pool for broader product recommendation coverage (fast: ~0.06s)
expanded_rules_pool <- NULL
if (exists("trans")) {
  tryCatch({
    expanded_rules_pool <- apriori(trans, 
                                   parameter = list(supp = 0.007, conf = 0.35, minlen = 2, maxlen = 4),
                                   control = list(verbose = FALSE))
  }, error = function(e) {
    expanded_rules_pool <- rules_by_lift
  })
} else {
  expanded_rules_pool <- rules_by_lift
}

# Extract product list for recommendation catalog
rec_catalog_items <- c()
if (!is.null(expanded_rules_pool)) {
  lhs_items <- unique(unlist(as(lhs(expanded_rules_pool), "list")))
  rhs_items <- unique(unlist(as(rhs(expanded_rules_pool), "list")))
  rec_catalog_items <- sort(unique(c(lhs_items, rhs_items)))
}
if (length(rec_catalog_items) == 0 && exists("item_counts")) {
  rec_catalog_items <- sort(item_counts$Description)
}

# Pre-calculate 2D PCA for K-Means Clustering
pca_plot_data <- NULL
if (exists("cluster_scaled") && exists("cluster_filtered") && exists("k3_model")) {
  pca_res <- prcomp(cluster_scaled)
  pca_plot_data <- data.frame(
    PC1 = as.numeric(pca_res$x[, 1]),
    PC2 = as.numeric(pca_res$x[, 2]),
    Cluster = paste("Cluster", cluster_filtered$cluster),
    Spend = as.numeric(cluster_filtered$total_spend),
    Items = as.numeric(cluster_filtered$total_items),
    SKUs = as.numeric(cluster_filtered$unique_skus),
    AvgPrice = as.numeric(cluster_filtered$avg_item_price),
    stringsAsFactors = FALSE
  )
  # Pre-sample 2,000 points for fluid rendering
  set.seed(42)
  if (nrow(pca_plot_data) > 2000) {
    pca_plot_data <- pca_plot_data[sample(nrow(pca_plot_data), 2000), ]
  }
}

# Helper to execute SQLite queries safely
query_dw <- function(sql) {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con))
  dbGetQuery(con, sql)
}

# ------------------------------------------------------------------------------
# 2. CUSTOM CSS & MODERN SAAS THEME
# ------------------------------------------------------------------------------
custom_theme_css <- "
  @import url('https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400;500;600;700&display=swap');
  
  body, .main-sidebar, .main-header .logo, h1, h2, h3, h4, h5, h6, .box-title {
    font-family: 'Plus Jakarta Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif !important;
  }
  
  .content-wrapper, .right-side {
    background-color: #f8fafc;
  }
  
  /* Modern Top Header */
  .main-header .navbar {
    background: linear-gradient(135deg, #1e1b4b 0%, #312e81 50%, #4338ca 100%) !important;
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.08);
  }
  .main-header .logo {
    background-color: #1e1b4b !important;
    font-weight: 700;
    letter-spacing: 0.5px;
    border-bottom: 0 solid transparent;
  }
  .main-header .logo:hover {
    background-color: #1e1b4b !important;
  }
  
  /* Sidebar Styling */
  .main-sidebar {
    background-color: #0f172a !important;
    box-shadow: 2px 0 10px rgba(0,0,0,0.05);
  }
  .sidebar-menu > li.header {
    color: #64748b;
    font-size: 11px;
    text-transform: uppercase;
    letter-spacing: 1.2px;
    background: transparent;
    padding: 14px 20px 6px 20px;
  }
  .sidebar-menu > li > a {
    color: #94a3b8;
    font-weight: 500;
    padding: 12px 18px;
    border-left: 3px solid transparent;
    transition: all 0.2s ease;
  }
  .sidebar-menu > li:hover > a, .sidebar-menu > li.active > a {
    color: #ffffff !important;
    background: #1e293b !important;
    border-left-color: #6366f1 !important;
  }
  .sidebar-menu > li > a i {
    margin-right: 10px;
    width: 20px;
    text-align: center;
  }
  
  /* Cards / Boxes */
  .box {
    border-radius: 12px !important;
    box-shadow: 0 4px 16px rgba(15, 23, 42, 0.04), 0 1px 3px rgba(15, 23, 42, 0.02) !important;
    border: 1px solid #e2e8f0 !important;
    margin-bottom: 22px;
    background: #ffffff;
  }
  .box-header {
    border-bottom: 1px solid #f1f5f9;
    padding: 16px 20px;
  }
  .box-header .box-title {
    font-size: 16px;
    font-weight: 700;
    color: #0f172a;
  }
  .box.box-solid.box-primary > .box-header {
    background: linear-gradient(135deg, #3b82f6, #1d4ed8);
    color: #fff;
  }
  .box.box-solid.box-info > .box-header {
    background: linear-gradient(135deg, #06b6d4, #0891b2);
    color: #fff;
  }
  .box.box-solid.box-success > .box-header {
    background: linear-gradient(135deg, #10b981, #059669);
    color: #fff;
  }
  .box.box-solid.box-warning > .box-header {
    background: linear-gradient(135deg, #f59e0b, #d97706);
    color: #fff;
  }
  
  /* Metric Cards (Value Boxes) */
  .kpi-card {
    background: #ffffff;
    border-radius: 12px;
    padding: 18px 20px;
    border: 1px solid #e2e8f0;
    box-shadow: 0 4px 14px rgba(15, 23, 42, 0.03);
    position: relative;
    overflow: hidden;
    margin-bottom: 20px;
    transition: transform 0.2s ease, box-shadow 0.2s ease;
  }
  .kpi-card:hover {
    transform: translateY(-2px);
    box-shadow: 0 8px 20px rgba(15, 23, 42, 0.06);
  }
  .kpi-title {
    font-size: 12px;
    font-weight: 600;
    color: #64748b;
    text-transform: uppercase;
    letter-spacing: 0.8px;
    margin-bottom: 6px;
  }
  .kpi-value {
    font-size: 26px;
    font-weight: 700;
    color: #0f172a;
    line-height: 1.2;
  }
  .kpi-sub {
    font-size: 12px;
    color: #10b981;
    font-weight: 500;
    margin-top: 6px;
  }
  .kpi-icon {
    position: absolute;
    right: 18px;
    top: 20px;
    font-size: 30px;
    opacity: 0.15;
    color: #4338ca;
  }
  
  /* SQL Code Terminal */
  .sql-terminal {
    background: #090d16;
    border: 1px solid #1e293b;
    border-radius: 8px;
    padding: 14px 18px;
    color: #38bdf8;
    font-family: 'SF Mono', Consolas, Monaco, monospace;
    font-size: 13px;
    line-height: 1.5;
    margin-top: 10px;
    margin-bottom: 15px;
    overflow-x: auto;
    white-space: pre-wrap;
  }
  
  /* Status Badges */
  .status-badge {
    display: inline-block;
    padding: 4px 10px;
    border-radius: 20px;
    font-size: 11px;
    font-weight: 600;
    letter-spacing: 0.3px;
  }
  .badge-emerald { background: #d1fae5; color: #065f46; }
  .badge-indigo { background: #e0e7ff; color: #3730a3; }
  .badge-amber { background: #fef3c7; color: #92400e; }
  .badge-rose { background: #ffe4e6; color: #9f1239; }
  
  /* Recommendation Item Card */
  .rec-card {
    background: #ffffff;
    border: 1px solid #e2e8f0;
    border-left: 4px solid #6366f1;
    border-radius: 8px;
    padding: 14px 18px;
    margin-bottom: 12px;
    transition: all 0.2s ease;
  }
  .rec-card:hover {
    border-left-color: #4338ca;
    box-shadow: 0 4px 12px rgba(99, 102, 241, 0.1);
  }
  
  /* Buttons */
  .btn-primary, .btn-success {
    border-radius: 8px;
    font-weight: 600;
    padding: 8px 16px;
    border: none;
    transition: all 0.2s ease;
  }
  .btn-primary { background: #4f46e5; }
  .btn-primary:hover { background: #4338ca; }
  .btn-success { background: #10b981; }
  .btn-success:hover { background: #059669; }
"

# ------------------------------------------------------------------------------
# 3. USER INTERFACE (UI) DEFINITION
# ------------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "purple",
  
  dashboardHeader(
    title = span(icon("cubes"), " pb&j Retail DWDM"),
    titleWidth = 260
  ),
  
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "sideTabs",
      menuItem("Executive Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Data Warehouse & OLAP", tabName = "olap", icon = icon("database")),
      menuItem("Star Schema Architecture", tabName = "schema", icon = icon("project-diagram")),
      menuItem("Market Basket Analysis", tabName = "rules", icon = icon("shopping-cart")),
      menuItem("Cross-Sell Recommender", tabName = "recommender", icon = icon("magic")),
      menuItem("Basket Margin Classifier", tabName = "classifier", icon = icon("bullseye")),
      menuItem("Customer Segmentation", tabName = "clusters", icon = icon("users"))
    ),
    tags$div(
      style = "padding: 18px; color: #64748b; font-size: 11px; line-height: 1.6; border-top: 1px solid #1e293b; margin-top: 30px;",
      tags$p(tags$b("Project:"), " DWDM Retail Analytics"),
      tags$p(tags$b("Architecture:"), " Star Schema (SQLite)"),
      tags$p(tags$b("Fact Rows:"), " 392,693 Events"),
      tags$p(tags$b("Algorithms:"), " Apriori, Random Forest, K-Means")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML(custom_theme_css))
    ),
    
    tabItems(
      # ========================================================================
      # TAB 1: EXECUTIVE OVERVIEW & KPIS
      # ========================================================================
      tabItem(
        tabName = "overview",
        fluidRow(
          column(width = 3,
            tags$div(class = "kpi-card",
              tags$div(class = "kpi-title", "Total Warehouse Revenue"),
              tags$div(class = "kpi-value", "£8,887,227"),
              tags$div(class = "kpi-sub", icon("arrow-up"), " Across 18,532 Invoices"),
              tags$div(class = "kpi-icon", icon("sterling-sign"))
            )
          ),
          column(width = 3,
            tags$div(class = "kpi-card",
              tags$div(class = "kpi-title", "Average Basket Value"),
              tags$div(class = "kpi-value", "£479.56"),
              tags$div(class = "kpi-sub", icon("cart-shopping"), " 278 Units / Basket"),
              tags$div(class = "kpi-icon", icon("basket-shopping"))
            )
          ),
          column(width = 3,
            tags$div(class = "kpi-card",
              tags$div(class = "kpi-title", "Active Customer Accounts"),
              tags$div(class = "kpi-value", "4,338"),
              tags$div(class = "kpi-sub", icon("globe"), " 37 Global Export Markets"),
              tags$div(class = "kpi-icon", icon("users"))
            )
          ),
          column(width = 3,
            tags$div(class = "kpi-card",
              tags$div(class = "kpi-title", "Catalog SKU Breadth"),
              tags$div(class = "kpi-value", "3,665"),
              tags$div(class = "kpi-sub", icon("box-open"), " 5.15M Total Units Sold"),
              tags$div(class = "kpi-icon", icon("tags"))
            )
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("chart-area"), " Monthly Revenue Trend & Invoiced Volume (DW Time Dimension)"),
            status = "primary", solidHeader = FALSE, width = 8,
            withSpinner(plotlyOutput("overviewTrendPlot", height = "360px"), type = 6, color = "#4f46e5")
          ),
          box(
            title = span(icon("globe-europe"), " Top Export Markets (Revenue Share)"),
            status = "primary", solidHeader = FALSE, width = 4,
            withSpinner(plotlyOutput("overviewCountryPlot", height = "360px"), type = 6, color = "#4f46e5")
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("fire"), " Top 10 Revenue-Generating Catalog Products"),
            status = "primary", solidHeader = FALSE, width = 12,
            withSpinner(DTOutput("overviewTopProductsTable"), type = 6, color = "#4f46e5")
          )
        )
      ),
      
      # ========================================================================
      # TAB 2: INTERACTIVE DATA WAREHOUSE & OLAP CUBE
      # ========================================================================
      tabItem(
        tabName = "olap",
        fluidRow(
          box(
            title = span(icon("sliders"), " OLAP Operation Controller & Dynamic Filters"),
            status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(width = 3,
                selectInput("olapOperation", "Select OLAP Operation:",
                            choices = c(
                              "1. Roll-Up (Monthly -> Quarterly Aggregation)" = "rollup",
                              "2. Drill-Down (Year -> Monthly Trend Analysis)" = "drilldown",
                              "3. Slice (Isolate Single Dimension: Year 2011)" = "slice",
                              "4. Dice (Multi-Dimensional Subcube Filter)" = "dice",
                              "5. Pivot (Rotate Geography by Quarter)" = "pivot"
                            ),
                            selected = "rollup")
              ),
              column(width = 3,
                conditionalPanel(
                  condition = "input.olapOperation == 'rollup' || input.olapOperation == 'dice'",
                  selectizeInput("olapCountries", "Filter Countries:",
                                 choices = c("United Kingdom", "Germany", "France", "EIRE", "Spain", "Netherlands", "Australia", "Switzerland"),
                                 selected = c("United Kingdom", "Germany", "France"),
                                 multiple = TRUE)
                ),
                conditionalPanel(
                  condition = "input.olapOperation == 'drilldown'",
                  selectInput("olapProductDrill", "Drill-Down Product:",
                              choices = c(
                                "REGENCY CAKESTAND 3 TIER" = "22423",
                                "CREAM HANGING HEART T-LIGHT HOLDER" = "85123A",
                                "JUMBO BAG RED RETROSPOT" = "85099B",
                                "PARTY BUNTING" = "47566"
                              ),
                              selected = "22423")
                ),
                conditionalPanel(
                  condition = "input.olapOperation == 'slice'",
                  selectInput("olapSliceYear", "Slice Dimension (Time: Year):",
                              choices = c("2011", "2010"),
                              selected = "2011")
                )
              ),
              column(width = 3,
                conditionalPanel(
                  condition = "input.olapOperation == 'dice'",
                  checkboxGroupInput("olapDiceQuarters", "Dice Dimension (Quarters):",
                                     choices = c("Q1" = "1", "Q2" = "2", "Q3" = "3", "Q4" = "4"),
                                     selected = c("3", "4"), inline = TRUE)
                ),
                conditionalPanel(
                  condition = "input.olapOperation == 'pivot'",
                  selectInput("olapPivotYear", "Pivot Year Perspective:",
                              choices = c("2011", "2010"),
                              selected = "2011")
                )
              ),
              column(width = 3,
                tags$div(style = "padding-top: 25px;",
                  actionButton("btnRunOlap", " Execute OLAP Query", icon = icon("bolt"), class = "btn-primary btn-block")
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("terminal"), " Live SQL Query (Executed directly on retail_dw.db)"),
            status = "info", solidHeader = FALSE, width = 12,
            tags$div(
              span(class = "status-badge badge-emerald", textOutput("olapExecBadge", inline = TRUE)),
              style = "margin-bottom: 8px;"
            ),
            tags$div(class = "sql-terminal", textOutput("olapSqlText"))
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("chart-bar"), " Multidimensional OLAP Aggregation Visualizer"),
            status = "primary", solidHeader = FALSE, width = 6,
            withSpinner(plotlyOutput("olapPlot", height = "380px"), type = 6, color = "#4f46e5")
          ),
          box(
            title = span(icon("table"), " OLAP Result Set"),
            status = "primary", solidHeader = FALSE, width = 6,
            withSpinner(DTOutput("olapTable"), type = 6, color = "#4f46e5")
          )
        )
      ),
      
      # ========================================================================
      # TAB 3: STAR SCHEMA ARCHITECTURE & DATA DICTIONARY
      # ========================================================================
      tabItem(
        tabName = "schema",
        fluidRow(
          box(
            title = span(icon("sitemap"), " Retail Data Warehouse Star Schema Architecture"),
            status = "primary", solidHeader = TRUE, width = 12,
            tags$div(
              style = "background: #f1f5f9; padding: 20px; border-radius: 10px; margin-bottom: 20px;",
              fluidRow(
                column(width = 4,
                  tags$div(style = "background:#fff; border:2px solid #3b82f6; border-radius:8px; padding:15px; text-align:center;",
                    tags$h4(style="color:#1d4ed8; font-weight:700; margin-top:0;", icon("clock"), " DIM_TIME"),
                    tags$p(style="font-size:12px; color:#475569;", "305 Unique Daily Grain Records"),
                    tags$hr(style="margin:8px 0;"),
                    tags$p(style="font-family:monospace; font-size:11px; text-align:left; color:#334155;",
                           "• time_key (PK, Surrogate)", tags$br(),
                           "• full_date (DATE)", tags$br(),
                           "• day (INTEGER)", tags$br(),
                           "• month (INTEGER)", tags$br(),
                           "• quarter (INTEGER)", tags$br(),
                           "• year (INTEGER)")
                  )
                ),
                column(width = 4,
                  tags$div(style = "background:#fff; border:2px solid #ef4444; border-radius:8px; padding:15px; text-align:center; box-shadow:0 4px 12px rgba(239,68,68,0.15);",
                    tags$h4(style="color:#b91c1c; font-weight:700; margin-top:0;", icon("receipt"), " FACT_SALES"),
                    tags$p(style="font-size:12px; color:#475569;", "392,693 Transaction Fact Records"),
                    tags$hr(style="margin:8px 0;"),
                    tags$p(style="font-family:monospace; font-size:11px; text-align:left; color:#334155;",
                           "• sales_key (PK, Surrogate)", tags$br(),
                           "• invoice_no (Natural Key)", tags$br(),
                           "• time_key (FK -> DIM_TIME)", tags$br(),
                           "• product_key (FK -> DIM_PRODUCT)", tags$br(),
                           "• customer_key (FK -> DIM_CUSTOMER)", tags$br(),
                           "• quantity (MEASURE, INT)", tags$br(),
                           "• unit_price (MEASURE, REAL)", tags$br(),
                           "• sales_amount (MEASURE, REAL)")
                  )
                ),
                column(width = 4,
                  tags$div(style = "background:#fff; border:2px solid #10b981; border-radius:8px; padding:15px; text-align:center;",
                    tags$h4(style="color:#047857; font-weight:700; margin-top:0;", icon("box"), " DIM_PRODUCT"),
                    tags$p(style="font-size:12px; color:#475569;", "3,665 Catalog SKU Entities"),
                    tags$hr(style="margin:8px 0;"),
                    tags$p(style="font-family:monospace; font-size:11px; text-align:left; color:#334155;",
                           "• product_key (PK, Surrogate)", tags$br(),
                           "• stock_code (Natural Key)", tags$br(),
                           "• description (VARCHAR)")
                  ),
                  tags$div(style = "background:#fff; border:2px solid #8b5cf6; border-radius:8px; padding:15px; text-align:center; margin-top:15px;",
                    tags$h4(style="color:#6d28d9; font-weight:700; margin-top:0;", icon("user"), " DIM_CUSTOMER"),
                    tags$p(style="font-size:12px; color:#475569;", "4,338 B2B / B2C Accounts"),
                    tags$hr(style="margin:8px 0;"),
                    tags$p(style="font-family:monospace; font-size:11px; text-align:left; color:#334155;",
                           "• customer_key (PK, Surrogate)", tags$br(),
                           "• customer_id (Natural Key)", tags$br(),
                           "• country (VARCHAR)")
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("book"), " Enterprise Data Warehouse Dictionary & Cardinality"),
            status = "info", solidHeader = FALSE, width = 12,
            DTOutput("schemaDictTable")
          )
        )
      ),
      
      # ========================================================================
      # TAB 4: MARKET BASKET ANALYSIS (APRIORI)
      # ========================================================================
      tabItem(
        tabName = "rules",
        fluidRow(
          box(
            title = span(icon("sliders"), " Apriori Rule Mining Hyperparameters"),
            status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(width = 3,
                sliderInput("ruleMinSupp", "Minimum Support (supp):",
                            min = 0.008, max = 0.04, value = 0.015, step = 0.002)
              ),
              column(width = 3,
                sliderInput("ruleMinConf", "Minimum Confidence (conf):",
                            min = 0.4, max = 0.9, value = 0.6, step = 0.05)
              ),
              column(width = 3,
                sliderInput("ruleMinLift", "Minimum Lift Threshold:",
                            min = 1.0, max = 25.0, value = 5.0, step = 1.0)
              ),
              column(width = 3,
                tags$div(style = "padding-top: 25px;",
                  actionButton("btnMineRules", "Re-Mine Association Rules", icon = icon("sync"), class = "btn-success btn-block")
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("circle-nodes"), " Interactive Co-Purchase Network Graph (visNetwork)"),
            status = "primary", solidHeader = FALSE, width = 7,
            tags$p(style="font-size:12px; color:#64748b;", "Drag nodes, pinch to zoom, and click item clusters to reveal co-purchased basket affinity pathways."),
            withSpinner(visNetworkOutput("rulesGraph", height = "440px"), type = 6, color = "#4f46e5")
          ),
          box(
            title = span(icon("chart-pie"), " Rule Quality Distribution (Support vs. Confidence)"),
            status = "primary", solidHeader = FALSE, width = 5,
            withSpinner(plotlyOutput("rulesScatterPlot", height = "440px"), type = 6, color = "#4f46e5")
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("table-list"), " Mined Association Rules Evaluation Table"),
            status = "primary", solidHeader = FALSE, width = 12,
            withSpinner(DTOutput("rulesTable"), type = 6, color = "#4f46e5")
          )
        )
      ),
      
      # ========================================================================
      # TAB 5: INTELLIGENT CROSS-SELL RECOMMENDER
      # ========================================================================
      tabItem(
        tabName = "recommender",
        fluidRow(
          box(
            title = span(icon("cart-plus"), " Basket Input & Real-Time Recommender Engine"),
            status = "success", solidHeader = TRUE, width = 5,
            tags$p("Select any product currently in the customer's checkout cart to dynamically predict high-lift complementary items:"),
            selectizeInput("recSelectedItem", "Cart Primary Item:",
                           choices = rec_catalog_items,
                           selected = ifelse("GREEN REGENCY TEACUP AND SAUCER" %in% rec_catalog_items,
                                             "GREEN REGENCY TEACUP AND SAUCER",
                                             rec_catalog_items[1]),
                           options = list(maxOptions = 100)),
            sliderInput("recTopN", "Number of Recommendations to Generate:",
                        min = 1, max = 5, value = 3, step = 1),
            actionButton("btnRecGenerate", "Generate Recommendations", icon = icon("wand-magic-sparkles"), class = "btn-success btn-block")
          ),
          box(
            title = span(icon("lightbulb"), " Merchandising Strategy & Value Impact"),
            status = "info", solidHeader = TRUE, width = 7,
            tags$h4(style="color:#0369a1; font-weight:700; margin-top:0;", "Algorithmic Cross-Sell Margin Optimization"),
            tags$p("This engine utilizes confidence-lift ranking derived from the Apriori association graph. By pairing high-affinity items with high baseline margins, checkout conversion increases without requiring markdowns."),
            tags$ul(style="line-height:1.8; color:#334155;",
              tags$li(tags$b("Frequently Bought Together:"), " Auto-injected into product view pages."),
              tags$li(tags$b("Bundle at Checkout:"), " 1-click add-on modal during shopping cart review."),
              tags$li(tags$b("Margin Accelerator:"), " Lift values >10x indicate purchase behaviors where price elasticity is low.")
            )
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("star"), " Recommended Add-On Products for Cart"),
            status = "primary", solidHeader = FALSE, width = 12,
            uiOutput("recCardsContainer")
          )
        )
      ),
      
      # ========================================================================
      # TAB 6: BASKET MARGIN CLASSIFIER (RANDOM FOREST)
      # ========================================================================
      tabItem(
        tabName = "classifier",
        fluidRow(
          box(
            title = span(icon("sliders"), " Real-Time What-If Basket Simulator"),
            status = "warning", solidHeader = TRUE, width = 4,
            tags$p(style="font-size:12px; color:#475569;", "Simulate transaction attributes to classify whether the cart qualifies as a High-Margin basket (top 30% spend tier):"),
            sliderInput("clfTotalItems", "Total Items in Basket (Volume):",
                        min = 1, max = 800, value = 180, step = 10),
            sliderInput("clfUniqueSkus", "Number of Unique SKUs:",
                        min = 1, max = 100, value = 15, step = 1),
            sliderInput("clfAvgPrice", "Average Item Price (£):",
                        min = 0.5, max = 50, value = 8.5, step = 0.5),
            radioButtons("clfCountry", "Customer Location:",
                         choices = c("United Kingdom" = "UK", "International" = "International"),
                         selected = "UK", inline = TRUE),
            actionButton("btnPredictMargin", "Run Random Forest Classification", icon = icon("calculator"), class = "btn-primary btn-block")
          ),
          box(
            title = span(icon("circle-check"), " Classification Result & Profitability Class"),
            status = "warning", solidHeader = TRUE, width = 8,
            uiOutput("marginPredictionBadge"),
            hr(),
            fluidRow(
              column(width = 6,
                tags$h5(tags$b("Model Architecture:"), " Random Forest Ensemble (100 Trees)"),
                tags$p("Trained on 18,532 basket feature records with 80/20 stratified partition."),
                tags$div(class = "status-badge badge-emerald", "Out-of-Bag (OOB) Accuracy: 90.79%"),
                tags$div(class = "status-badge badge-indigo", "Cohen's Kappa: 0.775")
              ),
              column(width = 6,
                tags$h5(tags$b("Margin Decision Rule:")),
                tags$p("Baskets with projected total gross margin exceeding the 70th percentile (£385.00) are flagged as High Margin for VIP checkout routing.")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("chart-bar"), " Random Forest Feature Importance (Mean Decrease Gini)"),
            status = "primary", solidHeader = FALSE, width = 6,
            withSpinner(plotlyOutput("clfVarImpPlot", height = "320px"), type = 6, color = "#4f46e5")
          ),
          box(
            title = span(icon("table-cells"), " Classifier Confusion Matrix & Diagnostics"),
            status = "primary", solidHeader = FALSE, width = 6,
            withSpinner(DTOutput("clfConfusionTable"), type = 6, color = "#4f46e5")
          )
        )
      ),
      
      # ========================================================================
      # TAB 7: CUSTOMER SEGMENTATION (K-MEANS)
      # ========================================================================
      tabItem(
        tabName = "clusters",
        fluidRow(
          column(width = 4,
            tags$div(class = "kpi-card",
              tags$div(class = "kpi-title", "Cluster 1: Bulk Wholesale / Premium"),
              tags$div(class = "kpi-value", "£1,705.42"),
              tags$div(class = "kpi-sub", "4.3% of Baskets | 1,045 Units | £26.30 Avg Price"),
              tags$div(class = "kpi-icon", icon("crown"))
            )
          ),
          column(width = 4,
            tags$div(class = "kpi-card",
              tags$div(class = "kpi-title", "Cluster 2: Multi-Item Diverse Baskets"),
              tags$div(class = "kpi-value", "£582.16"),
              tags$div(class = "kpi-sub", "24.5% of Baskets | 362 Units | 41.8 Unique SKUs"),
              tags$div(class = "kpi-icon", icon("boxes-packing"))
            )
          ),
          column(width = 4,
            tags$div(class = "kpi-card",
              tags$div(class = "kpi-title", "Cluster 3: Standard Retail Shoppers"),
              tags$div(class = "kpi-value", "£236.98"),
              tags$div(class = "kpi-sub", "71.2% of Baskets | 126 Units | 12.1 Unique SKUs"),
              tags$div(class = "kpi-icon", icon("bag-shopping"))
            )
          )
        ),
        
        fluidRow(
          box(
            title = span(icon("braille"), " K-Means Basket Clusters (2D Principal Component Projection)"),
            status = "warning", solidHeader = FALSE, width = 8,
            tags$p(style="font-size:12px; color:#64748b;", "Interactive PCA projection across total spend, total units, unique SKUs, and average unit price. Hover for basket details."),
            withSpinner(plotlyOutput("clusterPcaPlot", height = "400px"), type = 6, color = "#4f46e5")
          ),
          box(
            title = span(icon("pie-chart"), " Cluster Feature Means (Centroids)"),
            status = "warning", solidHeader = FALSE, width = 4,
            withSpinner(tableOutput("clusterSummaryTable"), type = 6, color = "#4f46e5"),
            hr(),
            tags$h5(tags$b("Actionable Marketing Directives:")),
            tags$p(tags$b("C1 (Wholesale):"), " Assign dedicated B2B account managers and volume discount tiers."),
            tags$p(tags$b("C2 (Variety):"), " Target with cross-category category banners and loyalty multi-buys."),
            tags$p(tags$b("C3 (Quick Retail):"), " Optimize mobile checkout velocity and low-friction 1-click ordering.")
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# 4. SERVER LOGIC & REACTIVE COMPUTATION
# ------------------------------------------------------------------------------
server <- function(input, output, session) {

  # ----------------------------------------------------------------------------
  # TAB 1: EXECUTIVE OVERVIEW
  # ----------------------------------------------------------------------------
  output$overviewTrendPlot <- renderPlotly({
    sql <- "
      SELECT 
        t.year || '-' || SUBSTR('0' || t.month, -2) as YearMonth,
        ROUND(SUM(f.sales_amount), 2) as Revenue,
        COUNT(DISTINCT f.invoice_no) as Orders
      FROM fact_sales f
      JOIN dim_time t ON f.time_key = t.time_key
      GROUP BY YearMonth
      ORDER BY YearMonth;
    "
    df_trend <- query_dw(sql)
    
    p <- plot_ly(df_trend, x = ~YearMonth) %>%
      add_bars(y = ~Revenue, name = "Revenue (£)", marker = list(color = "#4f46e5", opacity = 0.85)) %>%
      add_lines(y = ~Orders * 300, name = "Orders (scaled)", yaxis = "y2",
                line = list(color = "#10b981", width = 3)) %>%
      layout(
        xaxis = list(title = "Timeline (Year-Month)"),
        yaxis = list(title = "Monthly Revenue (£)"),
        yaxis2 = list(title = "Order Count", overlaying = "y", side = "right", showgrid = FALSE),
        legend = list(orientation = "h", x = 0.1, y = 1.15),
        hovermode = "x unified",
        margin = list(l = 50, r = 50, t = 20, b = 50)
      )
    p
  })
  
  output$overviewCountryPlot <- renderPlotly({
    sql <- "
      SELECT 
        c.country,
        ROUND(SUM(f.sales_amount), 2) as Revenue
      FROM fact_sales f
      JOIN dim_customer c ON f.customer_key = c.customer_key
      WHERE c.country != 'United Kingdom'
      GROUP BY c.country
      ORDER BY Revenue DESC
      LIMIT 8;
    "
    df_geo <- query_dw(sql)
    
    plot_ly(df_geo, x = ~reorder(country, Revenue), y = ~Revenue, type = "bar",
            marker = list(color = "#06b6d4")) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "International Revenue (£)"),
        margin = list(l = 40, r = 20, t = 20, b = 60)
      )
  })
  
  output$overviewTopProductsTable <- renderDT({
    sql <- "
      SELECT 
        p.stock_code as SKU,
        p.description as Product_Name,
        SUM(f.quantity) as Total_Units_Sold,
        ROUND(SUM(f.sales_amount), 2) as Total_Revenue_GBP,
        COUNT(DISTINCT f.invoice_no) as Order_Frequency
      FROM fact_sales f
      JOIN dim_product p ON f.product_key = p.product_key
      GROUP BY p.stock_code, p.description
      ORDER BY Total_Revenue_GBP DESC
      LIMIT 10;
    "
    top_p <- query_dw(sql)
    datatable(top_p, 
              options = list(pageLength = 5, dom = "tip", scrollX = TRUE),
              rownames = FALSE) %>%
      formatCurrency("Total_Revenue_GBP", currency = "£", digits = 2) %>%
      formatRound(c("Total_Units_Sold", "Order_Frequency"), digits = 0)
  })

  # ----------------------------------------------------------------------------
  # TAB 2: INTERACTIVE OLAP OPERATIONS
  # ----------------------------------------------------------------------------
  olap_result <- eventReactive(input$btnRunOlap, {
    t_start <- Sys.time()
    op <- input$olapOperation
    sql <- ""
    
    if (op == "rollup") {
      countries_str <- paste(paste0("'", input$olapCountries, "'"), collapse = ", ")
      sql <- sprintf("
SELECT c.country, t.year, t.quarter,
       SUM(f.quantity) AS total_units,
       ROUND(SUM(f.sales_amount), 2) AS total_sales
FROM fact_sales f
JOIN dim_customer c ON f.customer_key = c.customer_key
JOIN dim_time t ON f.time_key = t.time_key
WHERE c.country IN (%s)
GROUP BY c.country, t.year, t.quarter
ORDER BY c.country, t.year, t.quarter;", countries_str)
    
    } else if (op == "drilldown") {
      sql <- sprintf("
SELECT t.year, t.month, p.description,
       SUM(f.quantity) AS total_units,
       ROUND(SUM(f.sales_amount), 2) AS total_sales
FROM fact_sales f
JOIN dim_time t ON f.time_key = t.time_key
JOIN dim_product p ON f.product_key = p.product_key
WHERE p.stock_code = '%s'
GROUP BY t.year, t.month, p.description
ORDER BY t.year, t.month;", input$olapProductDrill)
    
    } else if (op == "slice") {
      sql <- sprintf("
SELECT p.stock_code, p.description,
       SUM(f.quantity) AS units_sold,
       ROUND(SUM(f.sales_amount), 2) AS total_revenue
FROM fact_sales f
JOIN dim_time t ON f.time_key = t.time_key
JOIN dim_product p ON f.product_key = p.product_key
WHERE t.year = %s
GROUP BY p.stock_code, p.description
ORDER BY total_revenue DESC
LIMIT 10;", input$olapSliceYear)
    
    } else if (op == "dice") {
      countries_str <- paste(paste0("'", input$olapCountries, "'"), collapse = ", ")
      quarters_str <- paste(input$olapDiceQuarters, collapse = ", ")
      if (quarters_str == "") quarters_str <- "1, 2, 3, 4"
      
      sql <- sprintf("
SELECT c.country, t.year, t.quarter,
       COUNT(DISTINCT f.invoice_no) AS basket_count,
       ROUND(SUM(f.sales_amount), 2) AS dice_sales
FROM fact_sales f
JOIN dim_customer c ON f.customer_key = c.customer_key
JOIN dim_time t ON f.time_key = t.time_key
WHERE c.country IN (%s)
  AND t.quarter IN (%s)
GROUP BY c.country, t.year, t.quarter
ORDER BY dice_sales DESC;", countries_str, quarters_str)
    
    } else if (op == "pivot") {
      sql <- sprintf("
SELECT c.country,
       ROUND(SUM(CASE WHEN t.quarter = 1 THEN f.sales_amount ELSE 0 END), 2) AS Q1_Sales,
       ROUND(SUM(CASE WHEN t.quarter = 2 THEN f.sales_amount ELSE 0 END), 2) AS Q2_Sales,
       ROUND(SUM(CASE WHEN t.quarter = 3 THEN f.sales_amount ELSE 0 END), 2) AS Q3_Sales,
       ROUND(SUM(CASE WHEN t.quarter = 4 THEN f.sales_amount ELSE 0 END), 2) AS Q4_Sales,
       ROUND(SUM(f.sales_amount), 2) AS Total_Sales
FROM fact_sales f
JOIN dim_customer c ON f.customer_key = c.customer_key
JOIN dim_time t ON f.time_key = t.time_key
WHERE t.year = %s
GROUP BY c.country
ORDER BY Total_Sales DESC
LIMIT 8;", input$olapPivotYear)
    }
    
    data <- query_dw(sql)
    t_end <- Sys.time()
    exec_ms <- round(as.numeric(difftime(t_end, t_start, units = "secs")) * 1000, 1)
    
    list(sql = sql, data = data, ms = exec_ms, op = op)
  }, ignoreNULL = FALSE)
  
  output$olapExecBadge <- renderText({
    res <- olap_result()
    paste0("Query Engine: SQLite Star Schema | Execution Time: ", res$ms, " ms | Rows Returned: ", nrow(res$data))
  })
  
  output$olapSqlText <- renderText({
    res <- olap_result()
    res$sql
  })
  
  output$olapTable <- renderDT({
    res <- olap_result()
    datatable(res$data, options = list(pageLength = 6, scrollX = TRUE), rownames = FALSE)
  })
  
  output$olapPlot <- renderPlotly({
    res <- olap_result()
    df <- res$data
    op <- res$op
    
    if (nrow(df) == 0) return(NULL)
    
    if (op == "rollup") {
      df$Period <- paste(df$country, paste0(df$year, "-Q", df$quarter))
      plot_ly(df, x = ~Period, y = ~total_sales, color = ~country, type = "bar") %>%
        layout(yaxis = list(title = "Total Sales (£)"), xaxis = list(title = "", tickangle = -45))
    } else if (op == "drilldown") {
      df$MonthLabel <- paste0(df$year, "-", sprintf("%02d", df$month))
      plot_ly(df, x = ~MonthLabel, y = ~total_sales, type = "scatter", mode = "lines+markers",
              line = list(color = "#4f46e5", width = 3)) %>%
        layout(yaxis = list(title = "Monthly Sales (£)"), xaxis = list(title = "Month"))
    } else if (op == "slice") {
      plot_ly(df, x = ~reorder(description, total_revenue), y = ~total_revenue, type = "bar",
              marker = list(color = "#10b981")) %>%
        layout(yaxis = list(title = "Revenue (£)"), xaxis = list(title = "", tickangle = -35))
    } else if (op == "dice") {
      df$Label <- paste(df$country, paste0("Q", df$quarter))
      plot_ly(df, x = ~Label, y = ~dice_sales, color = ~country, type = "bar") %>%
        layout(yaxis = list(title = "Dice Sales (£)"), xaxis = list(title = ""))
    } else if (op == "pivot") {
      plot_ly(df, x = ~country, y = ~Q1_Sales, name = "Q1", type = "bar") %>%
        add_trace(y = ~Q2_Sales, name = "Q2") %>%
        add_trace(y = ~Q3_Sales, name = "Q3") %>%
        add_trace(y = ~Q4_Sales, name = "Q4") %>%
        layout(barmode = "stack", yaxis = list(title = "Quarterly Sales (£)"), xaxis = list(title = ""))
    }
  })

  # ----------------------------------------------------------------------------
  # TAB 3: STAR SCHEMA DATA DICTIONARY
  # ----------------------------------------------------------------------------
  output$schemaDictTable <- renderDT({
    dict <- data.frame(
      Table = c("FACT_SALES", "FACT_SALES", "FACT_SALES", "FACT_SALES", "FACT_SALES", "FACT_SALES", "FACT_SALES", "FACT_SALES",
                "DIM_TIME", "DIM_TIME", "DIM_TIME", "DIM_TIME", "DIM_TIME", "DIM_TIME",
                "DIM_PRODUCT", "DIM_PRODUCT", "DIM_PRODUCT",
                "DIM_CUSTOMER", "DIM_CUSTOMER", "DIM_CUSTOMER"),
      Column = c("sales_key", "invoice_no", "time_key", "product_key", "customer_key", "quantity", "unit_price", "sales_amount",
                 "time_key", "full_date", "day", "month", "quarter", "year",
                 "product_key", "stock_code", "description",
                 "customer_key", "customer_id", "country"),
      Type = c("INTEGER", "VARCHAR", "INTEGER", "INTEGER", "INTEGER", "INTEGER", "REAL", "REAL",
               "INTEGER", "DATE", "INTEGER", "INTEGER", "INTEGER", "INTEGER",
               "INTEGER", "VARCHAR", "VARCHAR",
               "INTEGER", "VARCHAR", "VARCHAR"),
      Role = c("Primary Key (Surrogate)", "Natural Key", "Foreign Key -> DIM_TIME", "Foreign Key -> DIM_PRODUCT", "Foreign Key -> DIM_CUSTOMER", "Additive Fact Measure", "Non-Additive Fact Measure", "Additive Fact Measure",
               "Primary Key (Surrogate)", "Temporal Grain", "Time Attribute", "Time Attribute", "Hierarchical Level", "Hierarchical Level",
               "Primary Key (Surrogate)", "Natural Key", "Descriptive Attribute",
               "Primary Key (Surrogate)", "Natural Key", "Geographic Attribute"),
      stringsAsFactors = FALSE
    )
    datatable(dict, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  # ----------------------------------------------------------------------------
  # TAB 4: MARKET BASKET ANALYSIS (APRIORI)
  # ----------------------------------------------------------------------------
  mined_rules <- eventReactive(input$btnMineRules, {
    if (!exists("trans")) {
      return(rules_by_lift)
    }
    
    rules_new <- apriori(trans, 
                         parameter = list(supp = input$ruleMinSupp, 
                                          conf = input$ruleMinConf, 
                                          minlen = 2, maxlen = 4),
                         control = list(verbose = FALSE))
    
    rules_sub <- subset(rules_new, subset = lift >= input$ruleMinLift)
    sort(rules_sub, by = "lift", decreasing = TRUE)
  }, ignoreNULL = FALSE)
  
  output$rulesGraph <- renderVisNetwork({
    curr_rules <- mined_rules()
    if (length(curr_rules) == 0) return(NULL)
    
    top_sub <- head(curr_rules, 15)
    plot(top_sub, method = "graph", engine = "htmlwidget")
  })
  
  output$rulesScatterPlot <- renderPlotly({
    curr_rules <- mined_rules()
    if (length(curr_rules) == 0) return(NULL)
    
    q <- quality(curr_rules)
    plot_ly(q, x = ~support, y = ~confidence, color = ~lift,
            type = "scatter", mode = "markers",
            marker = list(size = 8, opacity = 0.8, colorscale = "Viridis", showscale = TRUE),
            text = ~paste("Support:", round(support, 3), "<br>Confidence:", round(confidence, 3), "<br>Lift:", round(lift, 2)),
            hoverinfo = "text") %>%
      layout(
        xaxis = list(title = "Support"),
        yaxis = list(title = "Confidence"),
        margin = list(l = 40, r = 20, t = 20, b = 40)
      )
  })
  
  output$rulesTable <- renderDT({
    curr_rules <- mined_rules()
    if (length(curr_rules) == 0) {
      return(data.frame(Message = "No association rules meet the selected thresholds."))
    }
    
    rules_df <- as(head(curr_rules, 50), "data.frame")
    rules_df$support <- round(rules_df$support, 4)
    rules_df$confidence <- round(rules_df$confidence, 4)
    rules_df$lift <- round(rules_df$lift, 2)
    if ("coverage" %in% colnames(rules_df)) rules_df$coverage <- round(rules_df$coverage, 4)
    
    datatable(rules_df, options = list(pageLength = 6, scrollX = TRUE), rownames = FALSE)
  })

  # ----------------------------------------------------------------------------
  # TAB 5: NEXT-ITEM RECOMMENDER
  # ----------------------------------------------------------------------------
  recommendations_data <- eventReactive(input$btnRecGenerate, {
    target_item <- input$recSelectedItem
    top_n <- input$recTopN
    
    # Try expanded pool first, fall back to rules_by_lift
    r_source <- if (!is.null(expanded_rules_pool)) expanded_rules_pool else rules_by_lift
    
    matching_rules <- subset(r_source, subset = lhs %pin% target_item)
    if (length(matching_rules) == 0 && !is.null(rules_by_lift)) {
      matching_rules <- subset(rules_by_lift, subset = lhs %pin% target_item)
    }
    
    if (length(matching_rules) == 0) {
      return(data.frame())
    }
    
    sorted_rules <- sort(matching_rules, by = "lift", decreasing = TRUE)
    sorted_rules <- head(sorted_rules, top_n * 2)
    
    q <- quality(sorted_rules)
    rhs_raw <- labels(rhs(sorted_rules))
    clean_rhs <- gsub("[{}]", "", rhs_raw)
    
    res <- data.frame(
      Product = clean_rhs,
      Confidence = round(q$confidence * 100, 1),
      Lift = round(q$lift, 1),
      Support = round(q$support * 100, 2),
      Strategy = ifelse(q$lift >= 15, "Bundle at Checkout", 
                 ifelse(q$lift >= 10, "Frequently Bought Together", "Recommended Cross-Sell")),
      stringsAsFactors = FALSE
    )
    res <- res[!res$Product %in% target_item, ]
    res <- res[!duplicated(res$Product), ]
    head(res, top_n)
  }, ignoreNULL = FALSE)
  
  output$recCardsContainer <- renderUI({
    recs <- recommendations_data()
    
    if (nrow(recs) == 0) {
      return(
        tags$div(
          style = "padding: 25px; text-align: center; color: #64748b; background: #f8fafc; border-radius: 8px;",
          tags$h4(icon("circle-info"), " No Direct High-Lift Rules Mined for This Item"),
          tags$p("Try selecting another popular SKU such as 'GREEN REGENCY TEACUP AND SAUCER', 'SPACEBOY LUNCH BOX', or 'JUMBO BAG RED RETROSPOT'.")
        )
      )
    }
    
    card_list <- lapply(seq_len(nrow(recs)), function(i) {
      row <- recs[i, ]
      tags$div(class = "rec-card",
        fluidRow(
          column(width = 7,
            tags$h4(style = "color:#1e293b; font-weight:700; margin-top:2px; font-size:15px;", 
                    paste0("#", i, " "), row$Product),
            tags$span(class = "status-badge badge-indigo", row$Strategy)
          ),
          column(width = 5, style = "text-align: right;",
            tags$span(style = "font-size: 13px; color: #475569; margin-right: 15px;",
                      tags$b("Confidence: "), paste0(row$Confidence, "%")),
            tags$span(class = "status-badge badge-emerald", 
                      paste0("Lift: ", row$Lift, "x Affinity")),
            tags$p(style = "font-size: 11px; color: #94a3b8; margin-top: 4px;",
                   paste0("Support: ", row$Support, "% of transactions"))
          )
        )
      )
    })
    
    do.call(tagList, card_list)
  })

  # ----------------------------------------------------------------------------
  # TAB 6: BASKET MARGIN CLASSIFICATION (RANDOM FOREST)
  # ----------------------------------------------------------------------------
  margin_prediction <- eventReactive(input$btnPredictMargin, {
    if (!exists("rf_model")) return(NULL)
    
    new_obs <- data.frame(
      total_items = as.numeric(input$clfTotalItems),
      unique_skus = as.numeric(input$clfUniqueSkus),
      avg_item_price = as.numeric(input$clfAvgPrice),
      country = factor(input$clfCountry, levels = c("International", "UK"))
    )
    
    probs <- predict(rf_model, newdata = new_obs, type = "prob")
    pred_class <- predict(rf_model, newdata = new_obs, type = "response")
    
    list(
      class = as.character(pred_class),
      prob_high = round(probs[1, "High"] * 100, 1),
      prob_low = round(probs[1, "Low"] * 100, 1)
    )
  }, ignoreNULL = FALSE)
  
  output$marginPredictionBadge <- renderUI({
    res <- margin_prediction()
    if (is.null(res)) {
      return(tags$p("Random Forest model not available in current environment."))
    }
    
    is_high <- res$class == "High"
    badge_bg <- if (is_high) "linear-gradient(135deg, #10b981, #059669)" else "linear-gradient(135deg, #f59e0b, #d97706)"
    icon_name <- if (is_high) "gem" else "basket-shopping"
    title_text <- if (is_high) "HIGH MARGIN BASKET" else "STANDARD / LOW MARGIN BASKET"
    desc_text <- if (is_high) {
      "This transaction is classified within the top 30% gross profit tier. Recommended for VIP loyalty perks and express shipping priority."
    } else {
      "This transaction is classified as standard margin. Recommended for cross-sell bundles to increase average basket volume."
    }
    
    tags$div(
      style = paste0("background: ", badge_bg, "; color: #ffffff; padding: 22px 25px; border-radius: 12px; box-shadow: 0 4px 14px rgba(0,0,0,0.1);"),
      fluidRow(
        column(width = 8,
          tags$h3(style = "font-weight: 800; margin: 0 0 8px 0;", icon(icon_name), " ", title_text),
          tags$p(style = "font-size: 13px; opacity: 0.95; margin: 0;", desc_text)
        ),
        column(width = 4, style = "text-align: right;",
          tags$div(style = "font-size: 32px; font-weight: 800;", paste0(res$prob_high, "%")),
          tags$div(style = "font-size: 11px; text-transform: uppercase; letter-spacing: 0.8px;", "High Margin Confidence")
        )
      )
    )
  })
  
  output$clfVarImpPlot <- renderPlotly({
    if (!exists("rf_model")) return(NULL)
    
    imp <- importance(rf_model)
    imp_df <- data.frame(
      Feature = c("Total Items", "Avg Item Price", "Unique SKUs", "Country"),
      Gini = imp[, "MeanDecreaseGini"],
      stringsAsFactors = FALSE
    )
    imp_df <- imp_df[order(imp_df$Gini), ]
    
    plot_ly(imp_df, x = ~Gini, y = ~reorder(Feature, Gini), type = "bar", orientation = "h",
            marker = list(color = "#4f46e5")) %>%
      layout(
        xaxis = list(title = "Mean Decrease in Gini Impurity"),
        yaxis = list(title = ""),
        margin = list(l = 100, r = 20, t = 20, b = 40)
      )
  })
  
  output$clfConfusionTable <- renderDT({
    if (!exists("conf_matrix")) return(NULL)
    
    cm <- conf_matrix$table
    cm_df <- as.data.frame.matrix(cm)
    cm_df <- cbind(Predicted = rownames(cm_df), cm_df)
    
    datatable(cm_df, options = list(dom = "t", paging = FALSE), rownames = FALSE)
  })

  # ----------------------------------------------------------------------------
  # TAB 7: CUSTOMER SEGMENTATION (K-MEANS)
  # ----------------------------------------------------------------------------
  output$clusterSummaryTable <- renderTable({
    if (!exists("cluster_filtered")) return(NULL)
    
    cluster_filtered %>%
      group_by(cluster) %>%
      summarise(
        Baskets = n(),
        Avg_Spend_GBP = round(mean(total_spend), 2),
        Avg_Units = round(mean(total_items), 2),
        Avg_Unique_SKUs = round(mean(unique_skus), 2),
        Avg_Unit_Price_GBP = round(mean(avg_item_price), 2)
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$clusterPcaPlot <- renderPlotly({
    if (is.null(pca_plot_data)) return(NULL)
    
    plot_ly(data = pca_plot_data, x = ~PC1, y = ~PC2, color = ~Cluster,
            type = "scatter", mode = "markers",
            colors = c("#4f46e5", "#06b6d4", "#f59e0b"),
            text = ~paste(
              "<b>", Cluster, "</b><br>",
              "Spend: £", round(Spend, 2), "<br>",
              "Total Items: ", Items, "<br>",
              "Unique SKUs: ", SKUs, "<br>",
              "Avg Price: £", round(AvgPrice, 2)
            ),
            hoverinfo = "text",
            marker = list(size = 6, opacity = 0.75)) %>%
      layout(
        xaxis = list(title = "PC1 (Volume & Spend Variance: 52.8%)"),
        yaxis = list(title = "PC2 (Price Density Variance: 26.4%)"),
        legend = list(orientation = "h", x = 0.1, y = 1.1),
        margin = list(l = 40, r = 20, t = 20, b = 40)
      )
  })
}

# ------------------------------------------------------------------------------
# 5. RUN APPLICATION
# ------------------------------------------------------------------------------
shinyApp(ui, server)
