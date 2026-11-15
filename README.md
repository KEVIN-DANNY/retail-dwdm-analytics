# pb&j Retail DWDM Intelligence & Decision Support Platform
> **Advanced Data Warehousing & Data Mining (DWDM) Coursework & Demonstration Project**

---

## 📌 Executive Summary

This platform is a unified Data Warehousing & Data Mining (DWDM) solution built on the UK Online Retail transaction dataset (392,693 customer transaction lines across 37 export markets). The platform bridges relational multidimensional modeling (Star Schema OLAP) with descriptive, predictive, and clustering data mining algorithms into an interactive enterprise Shiny decision support system.

---

## 🏗️ System Architecture

```
                       ┌─────────────────────────────────────────┐
                       │          Online Retail Dataset          │
                       │     (392k Invoiced Transaction Rows)    │
                       └────────────────────┬────────────────────┘
                                            │
                                  ETL & Data Cleansing
                                  (01_profile_validation.R)
                                            │
                                            ▼
                       ┌─────────────────────────────────────────┐
                       │       Data Warehouse: retail_dw.db      │
                       │       Star Schema Relational Model      │
                       └─────┬──────────────┬──────────────┬─────┘
                             │              │              │
              ┌──────────────┴───┐   ┌──────┴──────┐  ┌───┴──────────────┐
              │     DIM_TIME     │   │ DIM_PRODUCT │  │   DIM_CUSTOMER   │
              │ (305 Day Grains) │   │ (3,665 SKUs)│  │ (4,338 Accounts) │
              └──────────────┬───┘   └──────┬──────┘  └───┬──────────────┘
                             └──────────────┼─────────────┘
                                            ▼
                             ┌─────────────────────────────┐
                             │         FACT_SALES          │
                             │ (Surrogate Keys & Measures) │
                             └──────────────┬──────────────┘
                                            │
              ┌─────────────────────────────┴─────────────────────────────┐
              ▼                                                           ▼
┌──────────────────────────┐                               ┌─────────────────────────────┐
│  Data Warehouse / OLAP   │                               │    Data Mining Algorithms   │
│ (Roll-up, Drill-down,    │                               │ (Apriori, Random Forest,    │
│  Slice, Dice, Pivot)     │                               │  K-Means, Cross-Sell Rec)   │
└─────────────┬────────────┘                               └──────────────┬──────────────┘
              │                                                           │
              └─────────────────────────────┬─────────────────────────────┘
                                            ▼
                       ┌─────────────────────────────────────────┐
                       │    Modern Enterprise Shiny Dashboard    │
                       │                 (app.R)                 │
                       └─────────────────────────────────────────┘
```

---

## 🚀 Key Dashboard Capabilities

### 1. Executive Business Intelligence & KPIs
* **Real-time KPI Value Boxes**: Total Warehouse Revenue (£8.89M), Invoiced Transactions (18,532), Average Order Value (AOV: £479.56), and Active Customer Accounts (4,338).
* **Temporal Trend Visualizer**: Monthly revenue bar chart overlaid with dual-axis order volume trends.
* **Geographic Distribution**: Ranked revenue share across top European and international export destinations (Netherlands, EIRE, Germany, France, etc.).
* **Bestseller Matrix**: Top 10 catalog SKUs ranked by revenue contribution and frequency.

### 2. Live OLAP Cube Sandbox (Direct SQLite Execution)
Interactive UI executing SQL queries directly on [retail_dw.db](file:///Users/kevin/Downloads/dwdm_project/retail_dw.db) with execution benchmarks (<15ms average runtime):
* **Roll-Up**: Aggregates daily/monthly transaction granularity up to Quarterly and Annual totals across selected countries.
* **Drill-Down**: Deconstructs annual product performance down to monthly demand trajectories.
* **Slice**: Isolates sales across a single dimension (e.g., Year = 2011).
* **Dice**: Subcube filtering across multiple dimensions simultaneously (e.g., Q3-Q4 across France, Germany, Spain).
* **Pivot**: Rotates geographical dimensions against temporal quarterly columns in a cross-tabulated matrix.
* **Live SQL Terminal**: Displays the exact SQL executed with execution time benchmarks.

### 3. Star Schema Architecture & Data Dictionary
* Full visual diagram of the Star Schema with central `FACT_SALES` connected to `DIM_TIME`, `DIM_PRODUCT`, and `DIM_CUSTOMER`.
* Data dictionary detailing surrogate keys, natural keys, temporal hierarchies, and additive/non-additive measures.

### 4. Market Basket Analysis (Apriori Association Mining)
* Dynamic hyperparameter controls for **Support**, **Confidence**, and **Lift**.
* **Interactive Network Graph (`visNetwork`)**: Visual co-purchase graph with physics simulation, zoom, drag, and node inspection.
* **Rule Quality Scatter Plot**: Multi-attribute scatter plot mapping Support vs. Confidence colored by Lift.
* **Rule Audit Table**: Searchable, sortable `DT` data table displaying all mined item affinities.

### 5. Intelligent Cross-Sell Recommender
* Searchable catalog input covering all frequent store SKUs.
* Dynamic rule inference extracting high-lift RHS recommendations in real time.
* Visual recommendation cards with confidence percentages, lift multipliers, and retail merchandising strategies (*Bundle at Checkout*, *Frequently Bought Together*, *Cross-Sell Banner*).

### 6. Basket Margin Classifier (Random Forest Machine Learning)
* **What-If Basket Simulator**: User adjusts Basket Volume, Unique SKUs, Average Price, and Customer Location.
* **Real-time Inference**: Random Forest ensemble (100 trees, 90.79% OOB accuracy) classifies basket into **High Margin** (top 30% spend tier) vs. **Standard Margin** with confidence probabilities.
* **Model Diagnostics**: Interactive Variable Importance chart (Mean Decrease in Gini) and full confusion matrix.

### 7. Customer & Basket Segmentation (K-Means Clustering)
* Pre-computed K-Means ($k=3$) segmentation across spend, volume, SKU count, and unit price.
* **Interactive 2D PCA Projection (`plotly`)**: Scatter plot displaying cluster separation with hover tooltips.
* **Actionable Segment Profiles**:
  * **Cluster 1 (Bulk Wholesale / Premium)**: 4.3% of baskets, £1,705 mean spend, 1,045 units.
  * **Cluster 2 (Multi-Item Variety)**: 24.5% of baskets, £582 mean spend, 41.8 unique SKUs.
  * **Cluster 3 (Standard Retail)**: 71.2% of baskets, £237 mean spend, 12.1 unique SKUs.

---

## 💻 How to Run

### Option 1: From Terminal
```bash
cd /Users/kevin/Downloads/dwdm_project
R -e "shiny::runApp('app.R', port = 3838, host = '127.0.0.1')"
```
Then open `http://127.0.0.1:3838` in your browser.

### Option 2: From RStudio
1. Open `app.R` in RStudio.
2. Click **Run App** (top right of the editor pane).

---

## 📦 Required Dependencies
The application automatically checks for and utilizes the following installed R packages:
* `shiny`, `shinydashboard`, `shinyWidgets`, `shinycssloaders`
* `DT`, `plotly`, `visNetwork`, `ggplot2`
* `DBI`, `RSQLite`
* `arules`, `arulesViz`
* `randomForest`, `dplyr`, `scales`
