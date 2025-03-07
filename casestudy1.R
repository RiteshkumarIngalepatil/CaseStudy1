# Load Required Libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)
library(ggpubr)
library(fmsb)
library(scales)

# Load Dataset
data <- SmartWatch_Data_File

# Display Summary of Data
summary(data)

# Select Relevant Features for Clustering
features <- data %>% select(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete)

# Scale Data for Clustering
scaled_features <- scale(features)

# Determine Optimal Clusters using Elbow Method
wss <- sapply(2:10, function(k) {
  kmeans(scaled_features, centers = k, nstart = 10)$tot.withinss
})

# Enhanced Elbow Method Plot
ggplot(data.frame(k = 2:10, wss = wss), aes(x = k, y = wss)) +
  geom_point(color = "red", size = 3) +
  geom_line(color = "blue", linetype = "dashed") +
  labs(title = "Elbow Method for Optimal Clusters", x = "Number of Clusters", y = "Within Sum of Squares") +
  theme_minimal()

# Apply K-Means Clustering with 5 Clusters (Based on Elbow Method)
kmeans_result <- kmeans(scaled_features, centers = 5, nstart = 10)
data$Segment <- as.factor(kmeans_result$cluster)

# View Segment Summary
segment_summary <- data %>% group_by(Segment) %>% summarise(across(everything(), mean))
print(segment_summary)

# Enhanced Segment Distribution Plot
ggplot(data, aes(x = Segment, fill = Segment)) +
  geom_bar(alpha = 0.7) +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#662d91")) +
  labs(title = "Customer Segment Distribution (5 Clusters)", x = "Segment", y = "Count") +
  theme_minimal() +
  theme(text = element_text(size = 14))

# Radar Chart Setup
max_vals <- apply(features, 2, max)
min_vals <- apply(features, 2, min)
segment_means <- aggregate(features, by = list(data$Segment), mean)
radar_data <- rbind(max_vals, min_vals, segment_means[, -1])
colnames(radar_data) <- colnames(features)

# Enhanced Radar Chart Plot
radarchart(radar_data, axistype = 1,
           pcol = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#662d91"),
           pfcol = c(alpha("#1b9e77", 0.3), alpha("#d95f02", 0.3), alpha("#7570b3", 0.3), alpha("#e7298a", 0.3), alpha("#662d91", 0.3)),
           plwd = 3,
           title = "Enhanced Comparison of Smartwatch Market Segments (5 Clusters)")
legend("topright", legend = c("Tech-Savvy Professionals", "Health-Conscious Consumers", "Rugged & Durability Seekers", "Casual Users & Minimalists", "Athletes & Performance Enthusiasts"), 
       col = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#662d91"), lwd = 8, cex = 0.6)
