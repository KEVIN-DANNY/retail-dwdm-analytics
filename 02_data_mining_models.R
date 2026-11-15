# Install required packages if not already installed
required_pkgs <- c("data.table", "tidyverse", "arules", "arulesViz", 
                   "caret", "randomForest", "cluster", "factoextra")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)

library(data.table)
library(tidyverse)
library(arules)
library(arulesViz)
library(caret)
library(randomForest)
library(cluster)
library(factoextra)

# Load intermediate cleaned dataset (read-only)
df <- fread("/Users/kevin/Downloads/dwdm_project/online_retail_cleaned.csv")
cat("=== 1. ASSOCIATION RULES (APRIORI) ===\n")

# Prepare basket transaction format (items grouped per Invoice)
# Filter for items appearing frequently to optimize memory
item_counts <- df[, .N, by = Description][N >= 50]
df_assoc <- df[Description %in% item_counts$Description]

# Aggregate into basket lists
baskets_list <- split(df_assoc$Description, df_assoc$Invoice)
baskets_list <- lapply(baskets_list, unique)

# Convert to transactions object
trans <- as(baskets_list, "transactions")
summary(trans)

# Run Apriori: min support = 1.5%, min confidence = 60%, max length = 4
rules <- apriori(trans, 
                 parameter = list(supp = 0.015, conf = 0.6, minlen = 2, maxlen = 4),
                 control = list(verbose = FALSE))

# Sort by Lift
rules_by_lift <- sort(rules, by = "lift", decreasing = TRUE)

cat("Total rules mined:", length(rules), "\n")
cat("Top 6 Association Rules by Lift:\n")
inspect(head(rules_by_lift, 6))

# Plot top rules (Keep this plot ready for your demo)
plot(head(rules_by_lift, 15), method = "graph", engine = "htmlwidget")
cat("\n=== 2. CLASSIFICATION (BASKET MARGIN PREDICTION) ===\n")

# Engineer Basket-Level Feature Dataset
basket_features <- df[, .(
  total_spend = sum(SalesAmount),
  total_items = sum(Quantity),
  unique_skus = uniqueN(StockCode),
  avg_item_price = mean(Price),
  country = first(Country)
), by = Invoice]

# Define binary label: High Margin = top 30% of total spend
spend_threshold <- quantile(basket_features$total_spend, 0.70)
basket_features[, margin_class := ifelse(total_spend >= spend_threshold, "High", "Low")]
basket_features[, margin_class := as.factor(margin_class)]
basket_features[, country := as.factor(ifelse(country == "United Kingdom", "UK", "International"))]

# Model preparation
set.seed(42)
train_idx <- createDataPartition(basket_features$margin_class, p = 0.8, list = FALSE)
train_data <- basket_features[train_idx, .(total_items, unique_skus, avg_item_price, country, margin_class)]
test_data  <- basket_features[-train_idx, .(total_items, unique_skus, avg_item_price, country, margin_class)]

# Train Random Forest Classifier
rf_model <- randomForest(margin_class ~ ., data = train_data, ntree = 100, mtry = 2, importance = TRUE)

# Evaluation on Test Data
preds <- predict(rf_model, newdata = test_data)
conf_matrix <- confusionMatrix(preds, test_data$margin_class)

print(conf_matrix$table)
cat("Classification Accuracy:", round(conf_matrix$overall['Accuracy'] * 100, 2), "%\n")
varImpPlot(rf_model, main = "Feature Importance: Basket Margin Classification")
cat("\n=== 3. CLUSTERING (BASKET SEGMENTATION) ===\n")

# Aggregate features for clustering
cluster_input <- basket_features[, .(total_spend, total_items, unique_skus, avg_item_price)]

# Remove extreme outliers for stable clustering visualization (99th percentile)
q_limit <- quantile(cluster_input$total_spend, 0.99)
cluster_filtered <- cluster_input[total_spend < q_limit & total_items < quantile(total_items, 0.99)]

# Scale features
cluster_scaled <- scale(cluster_filtered)

# Run K-Means with k = 3
set.seed(123)
k3_model <- kmeans(cluster_scaled, centers = 3, nstart = 25)

# Append cluster assignments
cluster_filtered$cluster <- as.factor(k3_model$cluster)

# Cluster Profiles Summary
cat("Cluster Centers (Feature Means):\n")
print(cluster_filtered %>%
        group_by(cluster) %>%
        summarise(
          count = n(),
          mean_spend = round(mean(total_spend), 2),
          mean_units = round(mean(total_items), 2),
          mean_unique_skus = round(mean(unique_skus), 2),
          mean_price = round(mean(avg_item_price), 2)
        ))

# Visualize clusters (Keep ready for demo)
# Convert to standard data frame
df_scaled <- as.data.frame(cluster_scaled)
rownames(df_scaled) <- paste0("B_", seq_len(nrow(df_scaled)))

# Sample 2,500 points for crisp visual rendering without memory crashes
set.seed(42)
sample_idx <- sample(seq_len(nrow(df_scaled)), size = min(2500, nrow(df_scaled)))

# Subset data and matching cluster labels
sub_data <- df_scaled[sample_idx, ]
sub_clusters <- k3_model$cluster[sample_idx]

# Create a clean cluster object structure for factoextra
sub_k3 <- list(data = sub_data, cluster = sub_clusters)

# Render plot
fviz_cluster(sub_k3, 
             data = sub_data, 
             geom = "point", 
             ellipse.type = "convex", 
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "K-Means Basket Segmentation (k=3)")
cat("\n=== 4. PREDICTION (NEXT LIKELY ITEM ENGINE) ===\n")

# === PREDICTION / RECOMMENDATION FUNCTION ===
predict_next_item <- function(current_basket, association_rules, top_n = 3) {
  # Format item into arules LHS pattern
  target_pattern <- paste0(".*", gsub("([+?*^$(){}\\[\\]|\\\\])", "\\\\\\1", current_basket[1]), ".*")
  
  # Filter rules where LHS contains the item
  matching_rules <- subset(association_rules, subset = lhs %pin% target_pattern)
  
  if (length(matching_rules) == 0) {
    return("No confident recommendation found for this basket.")
  }
  
  # Sort strictly by lift
  sorted_rules <- sort(matching_rules, by = "lift", decreasing = TRUE)
  
  # Extract RHS item names cleanly
  rhs_raw <- labels(rhs(sorted_rules))
  recs <- gsub("[{}]", "", rhs_raw)
  recs <- recs[!recs %in% current_basket]
  
  return(head(unique(recs), top_n))
}

# --- TEST DEMO QUERY ---
sample_basket <- c("GREEN REGENCY TEACUP AND SAUCER")

cat("Given basket contains:", sample_basket, "\n")
cat("Recommended next items:\n")
print(predict_next_item(sample_basket, rules_by_lift, top_n = 3))