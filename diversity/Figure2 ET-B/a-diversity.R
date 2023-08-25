library(ggplot2)
library(dplyr)
library(vegan)
library(ggpubr)

data <- read.delim('data.txt',header=T,row.names = 1, sep = '\t',stringsAsFactors = F, check.names = F)
group <- read.delim('group.txt',header=F, sep = '\t',stringsAsFactors = F, check.names = F)
data<-t(data)

shannon<-diversity(data,"shannon") 

# Create a data frame for plotting
plot_df <- data.frame(name = rownames(data),
                      shannon = as.vector(shannon), 
                      group = as.factor(group$V2))

# Run the Wilcoxon test for each variable
stat.test <- compare_means(
  shannon ~ group, data = plot_df,
  method = "wilcox.test")
                   
p <- ggplot(plot_df, aes(x = group, y = shannon)) +
  stat_boxplot(geom = 'errorbar', width = 0.1) +
  geom_jitter(aes(fill = group), width = 0.1,color = "black", size = 2, shape = 21, alpha = 0.4)+
  geom_boxplot(aes(fill = group), width = 0.3) +
  scale_fill_brewer(palette ='Set1') +
  theme_test() +
  labs(
    x = "",  # Modify the x-axis labels
    y = "ET-B Shannon Index"  # Modify the y-axis label
  ) +
  theme(
    panel.border = element_rect(color = "black", size = 1),  # Add a bold border around the plot
    legend.position = "none",  # remove legend
    axis.text = element_text(size = 15, color = "black", face = "bold"),  # Modify the x, y axis scale size and color
    axis.title = element_text(color = "black", size = 14)  # Modify x, y axis title color and font size
  )+ 
  ylim(0,5) +
  stat_pvalue_manual(stat.test, label = "p.adj", y.position = 4.9, size=3)




print(p)

ggsave("ET-B-shannon.svg", plot = p, width = 3.5, height = 4)                  
