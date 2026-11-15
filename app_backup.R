library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)

# UI Definition
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "pb&j Retail Analytics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Association Rules", tabName = "rules", icon = icon("shopping-basket")),
      menuItem("Next-Item Prediction", tabName = "predict", icon = icon("magic")),
      menuItem("Basket Clusters", tabName = "clusters", icon = icon("chart-pie"))
    )
  ),
  dashboardBody(
    tabItems(
      # Tab 1: Association Rules
      tabItem(tabName = "rules",
              fluidRow(
                box(title = "Top Mined Association Rules", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("rulesTable"))
              )
      ),
      # Tab 2: Next-Item Predictor Demo
      tabItem(tabName = "predict",
              fluidRow(
                box(title = "Dynamic Basket Recommendation Engine", status = "success", solidHeader = TRUE, width = 6,
                    selectInput("selectedItem", "Select Current Basket Item:",
                                choices = c("GREEN REGENCY TEACUP AND SAUCER", 
                                            "ROSES REGENCY TEACUP AND SAUCER", 
                                            "SPACEBOY LUNCH BOX",
                                            "GARDENERS KNEELING PAD CUP OF TEA")),
                    actionButton("btnRec", "Generate Recommendations", class = "btn-success"),
                    hr(),
                    h4("Recommended Next Items to Add:"),
                    verbatimTextOutput("recOutput")
                ),
                box(title = "Business Value", status = "info", solidHeader = TRUE, width = 6,
                    p("This engine uses high-lift association rules to predict the most probable cross-sell item in real-time, optimizing checkout basket margins.")
                )
              )
      ),
      # Tab 3: Clusters
      tabItem(tabName = "clusters",
              fluidRow(
                box(title = "Basket Segmentation Profiles", status = "warning", solidHeader = TRUE, width = 12,
                    tableOutput("clusterSummary"),
                    hr(),
                    plotOutput("clusterPlot")
                )
              )
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Tab 1: Rules Table
  output$rulesTable <- renderDT({
    rules_df <- as(head(rules_by_lift, 15), "data.frame")
    datatable(rules_df, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Tab 2: Dynamic Next Item Predictor
  observeEvent(input$btnRec, {
    output$recOutput <- renderPrint({
      predict_next_item(input$selectedItem, rules_by_lift, top_n = 3)
    })
  })
  
  # Tab 3: Clusters Table & PCA Plot
  output$clusterSummary <- renderTable({
    cluster_filtered %>%
      group_by(cluster) %>%
      summarise(
        Transactions = n(),
        Avg_Spend_GBP = round(mean(total_spend), 2),
        Avg_Units = round(mean(total_items), 2),
        Avg_Unique_SKUs = round(mean(unique_skus), 2)
      )
  })
  
  output$clusterPlot <- renderPlot({
    pca_res <- prcomp(cluster_scaled)
    pca_df <- data.frame(
      PC1 = pca_res$x[, 1],
      PC2 = pca_res$x[, 2],
      Cluster = as.factor(k3_model$cluster)
    )
    ggplot(pca_df[sample(nrow(pca_df), min(2000, nrow(pca_df))), ], aes(x = PC1, y = PC2, color = Cluster)) +
      geom_point(alpha = 0.5) +
      theme_minimal() +
      labs(title = "K-Means Clusters (2D PCA Projection)", x = "PC1 (Spend/Volume)", y = "PC2 (Price Density)")
  })
}

shinyApp(ui, server)