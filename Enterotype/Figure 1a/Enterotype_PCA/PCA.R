library(ggplot2)
data <- read.delim('pca.txt',header=T, sep = '\t',stringsAsFactors = F, check.names = F)
data$group <- as.factor(data$group)


p<-ggplot(data = data, aes(x = CS1, y = CS2, fill = group, color = group)) + 
  geom_vline(xintercept = 0, color = 'gray', size = 0.7, linetype = 'dashed') + 
  geom_hline(yintercept = 0, color = 'gray', size = 0.7, linetype = 'dashed') + 
  geom_point(shape = 21, size = 3, color = 'black', stroke = 0.3) +
  theme_test() +
  stat_ellipse(type = "norm", level = 0.95) +
  scale_fill_manual(values = c('#a6cee3','#1f78b4','#b2df8a')) +
  scale_colour_manual(values = c('#a6cee3','#1f78b4','#b2df8a')) +
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) +
  labs(x = "PCA1(3.68%)", y = "PCA2(1.51%)") +
  theme(
    axis.text = element_text(size = 10, face = "bold", colour = "black"), 
    legend.position = c(0.08, 0.85),
    axis.title = element_text(size = 10, face = "bold"),
    panel.border = element_rect(fill = NA, color = "black", size = 1, linetype = "solid")
  )
p

ggsave("enterotype.svg", plot = p)
