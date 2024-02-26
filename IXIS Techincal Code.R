#############################################################
## IXIS Technical Exercise
#############################################################

##############################
# 0 - Load libraries
##############################
library(tidyverse)
library(writexl)
library(gridExtra)


##############################
# 1 - Load data
##############################
addsToCart <- read_csv("Desktop/IXIS Technical/DataAnalyst_Ecom_data_addsToCart.csv")
sessionCounts <- read_csv("Desktop/IXIS Technical/DataAnalyst_Ecom_data_sessionCounts.csv")


##############################
# 2 - Data exploration
##############################
# View the data and structure
View(sessionCounts)
str(sessionCounts)

View(addsToCart)
str(addsToCart)

# Summarize the variables to understand the distributions
summary(sessionCounts)
summary(addsToCart)

# Look for missing values
any(is.na(sessionCounts)) # There are no missing values

# View a tabular distribution of dim_browser
table(sessionCounts$dim_browser)
# I need to handle "error" and case

## Visualize distributions
# Look at the sessions variable
ggplot(sessionCounts, aes(x = sessions)) +
  geom_histogram(binwidth = 500) +
  labs(title = "Distribution of Sessions",
       x = "Sessions",
       y = "Frequency")
# There look to be some large outliners to the right

# Look at the QTY variable
ggplot(sessionCounts, aes(x = QTY)) +
  geom_histogram(binwidth = 50) +
  labs(title = "Distribution of QTY",
       x = "QTY",
       y = "Frequency")
# Also some large outliers to the right in this distribution

# Look at deviceCategory
ggplot(sessionCounts, aes(x = dim_deviceCategory)) +
  geom_bar() +
  labs(title = "Distribution of Device Categories",
       x = "Device Category",
       y = "Count")

# Look at broswer
ggplot(sessionCounts, aes(x = dim_browser)) +
  geom_bar() +
  labs(title = "Distribution of Browser",
       x = "Browser",
       y = "Count")

# This is not strictly needed for this project but could be useless for further analysis (time-series, etc.)
# Look at date to see seasonality or any large spikes
ggplot(sessionCounts, aes(x = dim_date)) +
  geom_bar() +
  labs(title = "Distribution of Date",
       x = "Date",
       y = "Count")

# Look at transactions by sessions for correlation
cor(sessionCounts$sessions, sessionCounts$transactions)
ggplot(sessionCounts, aes(x = sessions, y = transactions)) +
  geom_point() +
  labs(title = "Relationship between Sessions and Transactions",
       x = "Sessions",
       y = "Transactions")
# more sessions is highly correlated with more transactions, as you'd expect


##############################
# 3 - Data cleaning
##############################
# Convert dim_browser to lowercase
sessionCounts <- sessionCounts %>%
  mutate(dim_browser = tolower(dim_browser))

# Replace "error" with NA
sessionCounts$dim_browser <- ifelse(sessionCounts$dim_browser == "error", NA, sessionCounts$dim_browser)

# Convert dim_date to date format
sessionCounts$dim_date <- as.Date(sessionCounts$dim_date, format = "%m/%d/%y")

# Extract Month from dim_date
sessionCounts$Month <- format(sessionCounts$dim_date, "%Y-%m")

# Create a date variable for addsToCart as well
addsToCart$dim_date <- as.Date(paste(addsToCart$dim_year, addsToCart$dim_month, "1", sep = "-"))
addsToCart$Month <- format(addsToCart$dim_date, "%Y-%m")


##############################
# 4 - Aggregation and file creation
##############################
# Aggregate data by Month and Device for the first sheet
sessionCounts_aggregated <- sessionCounts %>%
  group_by(Month, dim_deviceCategory) %>%
  summarize(Sessions = sum(sessions),
            Transactions = sum(transactions),
            QTY = sum(QTY)) %>%
  mutate(ECR = Transactions / Sessions)

# Aggregate by just month for the second sheet
sessionCounts_aggregated_tot <- sessionCounts %>%
  group_by(Month) %>%
  summarize(sessions = sum(sessions),
            transactions = sum(transactions),
            QTY = sum(QTY))

# Merge the aggregated data for all available metrics
merged_data <- merge(sessionCounts_aggregated_tot, addsToCart, by = "Month", all.x = TRUE)

# Filter data for the most recent two months
recent_months <- unique(merged_data$Month) %>% tail(2)

# Filter aggregated data for the most recent two months
merged_data_recent <- merged_data %>%
  filter(Month %in% recent_months) %>%
  select(Month, sessions, transactions, QTY, addsToCart)

# Get the most recent month's and prior month's values for each metric
most_recent_values <- merged_data_recent %>%
  slice_tail(n = 1) %>%
  select(-Month)

prior_month_values <- merged_data_recent %>%
  slice_head(n = 1) %>%
  select(-Month)

# Calculate the absolute differences between the most recent and prior months
absolute_differences <- most_recent_values - prior_month_values

# Calculate the relative differences between the most recent and prior months
relative_differences <- absolute_differences / prior_month_values

# Combine the most recent, prior, absolute differences, and relative differences
comparison_results <- rbind(
  most_recent_values,
  prior_month_values,
  absolute_differences,
  relative_differences
)

# Add in the metric names
comparison_results$metric <- c("Most Recent Month", "Prior Month", "Absolute_Difference", "Relative_Difference")

##############################
# 5 - Write to Excel
##############################
write_xlsx(list("Month by Device Aggregation" = sessionCounts_aggregated, 
                "Month over Month Differences" = comparison_results), 
           path = "Desktop/IXIS Technical/output.xlsx")

##############################
# 6 - Plots for Deck
##############################
# Create line plots for each metric over time
sessionCounts_aggregated$Month <- as.Date(paste(sessionCounts_aggregated$Month, "01", sep = "-"))

plots <- list()
metrics <- c("Sessions", "Transactions", "QTY", "ECR")
for (metric in metrics) {
  plot <- ggplot(sessionCounts_aggregated, aes(x = Month, y = !!sym(metric), color = dim_deviceCategory)) +
    geom_line() +
    labs(title = paste("Trend of", metric),
         x = "Month",
         y = metric,
         color = "Device Category") +
    theme_minimal()
  plots[[metric]] <- plot
}

grid.arrange(grobs = plots, ncol = 2)

# Reshape the data for plotting
# Filter out rows for 'Absolute_Difference' and 'Relative_Difference' so we only focus on the two months
comparison_results_filtered <- comparison_results %>% 
  filter(metric != "Absolute_Difference" & metric != "Relative_Difference")

# Pivot the table into a long format for plotting
comparison_results_long <- pivot_longer(comparison_results_filtered, cols = -metric, names_to = "metric_type", values_to = "value")

# Define the order of levels so we have most recent month first in the plot
order_levels <- rev(unique(comparison_results_long$metric))

# Reorder the levels in the metric variable
comparison_results_long$metric <- factor(comparison_results_long$metric, levels = order_levels)

comparison_results_long$metric_type <- factor(comparison_results_long$metric_type,
                         levels = unique(comparison_results_long$metric_type),
                         labels = c("sessions" = "Sessions",
                                    "transactions" = "Transactions",
                                    "QTY" = "Quantity",
                                    "addsToCart" = "Adds to Cart"))

# Create individual plots for each metric
plot <- ggplot(comparison_results_long, aes(x = metric, y = value, fill = metric_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~ metric_type, scales = "free") +
  labs(title = "Comparison of Metrics Between Prior Month and Most Recent Month",
       x = "Metric",
       y = "Value",
       fill = "Metric Type") +
  theme_minimal() +
  theme(legend.position = "none")

plot
