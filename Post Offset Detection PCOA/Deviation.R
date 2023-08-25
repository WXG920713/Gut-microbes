# Load required libraries
library(vegan)
library(ggplot2)
library(ggpubr)

# Read the data
df <- read.table("data.txt", header=TRUE, sep="\t")

# Extract project ids from the second column and drop it
project_ids <- df[, 2]
df <- df[, -c(1,2)]

# Calculate the Bray-Curtis distance matrix
dist_matrix <- vegdist(df, method="bray")

# Perform PCoA analysis
pcoa_result <- cmdscale(dist_matrix, k=2, eig=TRUE)

# Calculate the proportion of variance explained by each PCoA axis
explained_variance <- pcoa_result$eig / sum(pcoa_result$eig)

# Create a data frame for plotting
plot_df <- data.frame(PC1 = pcoa_result$points[, 1], 
                      PC2 = pcoa_result$points[, 2], 
                      projectid = as.factor(project_ids))

p<-ggplot(plot_df, aes(x=PC1, y=PC2, color=projectid, fill=projectid)) +
  geom_point(size=3, shape=21, color="black") +
  stat_ellipse(aes(fill = projectid), geom = "polygon", alpha = 0, level=0.95) +  # Add 95% confidence ellipse with transparent fill
  theme_test() +
  labs(title="",
       x=paste0("PCoA1 (", format(100 * explained_variance[1], digits=2), "%)"),
       y=paste0("PCoA2 (", format(100 * explained_variance[2], digits=2), "%)")) +
  theme(legend.position="bottom")+theme(
    axis.text = element_text(size = 10, face = "bold", colour = "black"),
    axis.title = element_text(size = 10, face = "bold"),
    panel.border = element_rect(fill = NA, color = "black", size = 1, linetype = "solid")
  )+
  annotate("text", x = Inf, y = Inf, label = "p < 0.001", hjust = 1.1, vjust = 1.5)

p
ggsave("pcoa.svg", plot = p)

# Perform PERMANOVA
permanova_result <- adonis2(dist_matrix ~ project_ids, permutations = 999)
print(permanova_result)

#output
write.table(adonis_result_two, 'PERMANOVA.result_two.txt', row.names = FALSE, sep = '\t', quote = FALSE, na = '')
