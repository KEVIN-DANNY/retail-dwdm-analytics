# RStudio: 01_profile_validation.R
# Read-only profiling on intermediate cleaned data
required_pkgs <- c("data.table", "dplyr","skimr")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)
library(data.table)
library(dplyr)
library(skimr)

# 1. Load intermediate cleaned dataset
df_clean <- fread("/Users/kevin/Downloads/dwdm_project/online_retail_cleaned.csv")

# 2. Schema and shape verification
cat("Rows:", nrow(df_clean), "| Columns:", ncol(df_clean), "\n")
str(df_clean)

# 3. Measures Profiling (Min, Quartiles, Mean, Max, SD)
measures_summary <- df_clean %>%
  select(Quantity, Price, SalesAmount) %>%
  skim()
print(measures_summary)

# 4. Check Basket-level Metrics
basket_metrics <- df_clean %>%
  group_by(Invoice) %>%
  summarise(
    basket_items = sum(Quantity),
    basket_lines = n(),
    basket_value = sum(SalesAmount)
  )

cat("\n--- Basket Summary Statistics ---\n")
summary(basket_metrics)

# 5. Top 10 High-Volume Products
top_products <- df_clean %>%
  group_by(StockCode, Description) %>%
  summarise(
    total_qty = sum(Quantity),
    total_revenue = round(sum(SalesAmount), 2)
  ) %>%
  arrange(desc(total_revenue)) %>%
  head(10)
print(top_products)

# 6. Geographic Distribution
country_dist <- df_clean %>%
  group_by(Country) %>%
  summarise(
    unique_invoices = n_distinct(Invoice),
    total_spend = round(sum(SalesAmount), 2)
  ) %>%
  arrange(desc(total_spend)) %>%
  head(5)
print(country_dist)