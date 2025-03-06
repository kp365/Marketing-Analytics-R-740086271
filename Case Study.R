# -----------------------------

library(readxl)     # For reading Excel files
library(dplyr)      # For data manipulation
library(janitor)    # For cleaning column names
library(ggplot2)    # For plotting
library(cluster)    # For silhouette analysis
library(tidyr)      # For data reshaping


# 2. Data Import and Cleaning


data <- read_excel("SmartWatch Data File.xlsx")


data <- data %>% clean_names()
print("Data columns:")
print(names(data))

# Remove rows with missing values
data_clean <- data %>% drop_na()

# Convert categorical variables to factors and ensure proper data types
data_clean <- data_clean %>%
  mutate(
    amzn_p = as.factor(amzn_p),
    female = as.factor(female),
    degree = as.factor(degree),
    income = factor(income, ordered = TRUE),
    age = as.numeric(age)
  )


# 3. Standardizing Likert-scale Columns

# Define the Likert-scale column names (adjust if necessary)
likert_cols <- c("const_com", "timely_inf", "task_mgm", "device_st", "wellness", "athlete", "style")

# Check if all required columns are present
missing_cols <- setdiff(likert_cols, names(data_clean))
if(length(missing_cols) > 0){
  stop(paste("The following columns are missing in the dataset:", paste(missing_cols, collapse = ", ")))
}

# Standardize Likert-scale responses for comparability
data_clean <- data_clean %>% 
  mutate(across(all_of(likert_cols), scale))
print("Likert-scale columns have been standardized.")


# 4. Clustering Analysis to Identify Segments (4 Clusters)

set.seed(123)  # For reproducibility

# Determine optimal number of clusters using the Elbow Method
wss <- sapply(1:10, function(k) {
  kmeans(data_clean[, likert_cols], centers = k, nstart = 25)$tot.withinss
})
elbow_data <- data.frame(k = 1:10, wss = wss)
ggplot(elbow_data, aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Method: Optimal Number of Clusters",
       x = "Number of Clusters (k)",
       y = "Total Within Sum of Squares")

# Based on the elbow plot, assume the optimal number of clusters is 4.
k4 <- tryCatch({
  kmeans(data_clean[, likert_cols], centers = 4, nstart = 25)
}, error = function(e) {
  message("Error in kmeans: ", e$message)
  NULL
})

if(is.null(k4)){
  stop("Clustering failed. Please check your data and code.")
} else {
  print("k4 clustering completed successfully.")
}

# Add cluster assignments to the dataset
data_clean$cluster <- as.factor(k4$cluster)


# 5. Analyzing the Clusters (Segment Profiles)

# Display the number of respondents in each cluster
cluster_counts <- table(data_clean$cluster)
print("Cluster counts:")
print(cluster_counts)

# Calculate mean values for each Likert-scale variable by cluster
cluster_summary <- data_clean %>%
  group_by(cluster) %>%
  summarise(across(all_of(likert_cols), mean, na.rm = TRUE),
            count = n())
print("Cluster Summary:")
print(cluster_summary)


# 6. Silhouette Analysis for Cluster Validation

dist_matrix <- dist(data_clean[, likert_cols])
sil <- silhouette(k4$cluster, dist_matrix)
plot(sil, main = "Silhouette Plot for 4 Clusters")


# 7. Additional Visualizations: Bar Graphs


# 7.1 Bar Chart for Cluster Distribution
ggplot(data_clean, aes(x = cluster)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Cluster Distribution",
    x = "Cluster",
    y = "Count"
  ) +
  theme_minimal()

# 7.2 Grouped Bar Chart of Mean Attribute Ratings by Cluster
# Pivot the cluster_summary to long format (excluding the count column)
long_summary <- cluster_summary %>%
  select(-count) %>%    # Remove 'count' to focus on attributes
  pivot_longer(
    cols = all_of(likert_cols),
    names_to = "Attribute",
    values_to = "Mean_Rating"
  )

ggplot(long_summary, aes(x = Attribute, y = Mean_Rating, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Average Attribute Ratings by Cluster",
    x = "Smartwatch Attribute",
    y = "Mean Rating"
  ) +
  coord_flip() +
  theme_minimal()


# 8. Save the Cluster Summary to a CSV File

if(exists("cluster_summary")){
  write.csv(cluster_summary, "cluster_summary.csv", row.names = FALSE)
  print("Cluster summary saved as 'cluster_summary.csv'")
} else {
  print("Error: 'cluster_summary' not found.")
}

