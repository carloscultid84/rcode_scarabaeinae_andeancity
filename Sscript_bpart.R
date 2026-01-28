# Load required libraries
install.packages("BAT")
library(BAT)
library(ggplot2)

# 1. DATA LOADING
# Read community matrix with sites as row names
community_data <- read.csv("zones.csv", header = TRUE, row.names = 1)

# 2. MULTI-SITE BETA DIVERSITY PARTITIONING
# Calculate overall beta diversity components using Jaccard dissimilarity
beta_multi <- beta.multi(community_data, abund = FALSE, func = "jaccard", runs = 1000)

# Convert results to data frame for plotting
beta_multi_df <- data.frame(beta_multi)

# Prepare data for visualization
beta_components <- c("Btotal", "Brepl", "Brich")
average_values <- c(beta_multi_df$Average)
variance_values <- c(beta_multi_df$Variance)
std_dev <- sqrt(variance_values)

beta_summary <- data.frame(
  Beta_component = factor(beta_components, levels = c("Btotal", "Brepl", "Brich")),
  Average = average_values,
  SD = std_dev
)

# 3. VISUALIZATION: BETA DIVERSITY COMPONENTS
beta_plot <- ggplot(beta_summary, aes(x = Beta_component, y = Average)) + 
  geom_bar(stat = "identity", color = "black", fill = "gray80", width = 0.7) +
  geom_errorbar(aes(ymin = Average - SD, ymax = Average + SD), 
                width = 0.2, color = "black", linewidth = 0.5) +
  labs(x = "Beta diversity component", 
       y = "Average value") +
  ylim(0.0, 0.5) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "transparent", color = 'black', linewidth = 1),
    text = element_text(size = 14, family = "Times New Roman"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold")
  )

# Display plot
print(beta_plot)

# 4. PAIRWISE BETA DIVERSITY CALCULATIONS
# Calculate pairwise beta diversity components
beta_pairwise <- beta(community_data, abund = FALSE, func = "jaccard", 
                      raref = 0, runs = 1000)

# Extract and format total beta component (Btotal)
beta_total_matrix <- as.matrix(beta_pairwise$Btotal)
rownames(beta_total_matrix) <- rownames(community_data)
colnames(beta_total_matrix) <- rownames(community_data)
beta_total_dist <- as.dist(beta_total_matrix)

# Extract and format replacement component (Brepl)
beta_repl_matrix <- as.matrix(beta_pairwise$Brepl)
rownames(beta_repl_matrix) <- rownames(community_data)
colnames(beta_repl_matrix) <- rownames(community_data)
beta_repl_dist <- as.dist(beta_repl_matrix)

# Extract and format richness component (Brich)
beta_rich_matrix <- as.matrix(beta_pairwise$Brich)
rownames(beta_rich_matrix) <- rownames(community_data)
colnames(beta_rich_matrix) <- rownames(community_data)
beta_rich_dist <- as.dist(beta_rich_matrix)

# 5. CLUSTER ANALYSIS
# Hierarchical clustering for total beta diversity (Btotal)
cluster_total <- hclust(beta_total_dist, method = "average")
plot(cluster_total, hang = -1, main = "Cluster dendrogram: Total beta diversity",
     xlab = "Sites", ylab = "Jaccard dissimilarity")

# Hierarchical clustering for replacement component (Brepl)
cluster_repl <- hclust(beta_repl_dist, method = "average")
plot(cluster_repl, hang = -1, main = "Cluster dendrogram: Replacement component",
     xlab = "Sites", ylab = "Jaccard dissimilarity")

# Hierarchical clustering for richness component (Brich)
cluster_rich <- hclust(beta_rich_dist, method = "average")
plot(cluster_rich, hang = -1, main = "Cluster dendrogram: Richness component",
     xlab = "Sites", ylab = "Jaccard dissimilarity")