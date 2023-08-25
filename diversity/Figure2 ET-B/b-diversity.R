# Load required libraries
library(vegan)
library(ggplot2)

# Read the data
df <- read.table("data.txt", header=TRUE,row.names = 1, sep="\t")
group<-read.table("group.txt",row.names = 1, sep="\t")
df<-t(df)

# Calculate the Bray-Curtis distance matrix
dist_matrix <- vegdist(df, method="bray")

# Perform PCoA analysis
pcoa_result <- cmdscale(dist_matrix, k=2, eig=TRUE)

# Calculate the proportion of variance explained by each PCoA axis
explained_variance <- pcoa_result$eig / sum(pcoa_result$eig)

# Create a data frame for plotting
plot_df <- data.frame(PC1 = pcoa_result$points[, 1], 
                      PC2 = pcoa_result$points[, 2], 
                      group = as.factor(group$V2))

p<-ggplot(plot_df, aes(x=PC1, y=PC2, color=group, fill=group)) +
  geom_point(size=3, shape=21, color="black") +
  stat_ellipse(aes(fill = group), geom = "polygon", alpha = 0, level=0.95) +  # Add 95% confidence ellipse with transparent fill
  theme_test() +
  scale_fill_brewer(palette ='Set1') + 
  labs(title="",
       x=paste0("PCoA1 (", format(100 * explained_variance[1], digits=2), "%)"),
       y=paste0("PCoA2 (", format(100 * explained_variance[2], digits=2), "%)")) +
  theme(legend.position="bottom") +
  labs(title = "ET-B PCOA based on Bray-Curtis distance
"
  ) +
  annotate("text", x = Inf, y = Inf, label = "p < 0.001", hjust = 1.1, vjust = 1.5) +
  theme(
    panel.border = element_rect(color = "black", size = 1),  # Add a bold border around the plot
    legend.position = "none",  # remove legend
    axis.text = element_text(size = 15, color = "black", face = "bold"),  # Modify the x, y axis scale size and color
    axis.title = element_text(color = "black", size = 14),  # Modify x, y axis title color and font size
    plot.title = element_text(hjust = 0.5)  # center the title
  )

print(p)

# Perform PERMANOVA
permanova_result <- adonis2(dist_matrix ~ group$V2, permutations = 999)
print(permanova_result)

ggsave("ET-B-PCOA.svg", plot = p, width = 5, height = 4)   
