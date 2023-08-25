library(ggplot2)
library(reshape2)

data <- read.delim('genus.txt',header=T, sep = '\t',stringsAsFactors = F, check.names = F)

data$enterotype<-factor(data$enterotype,levels=c('ET-L','ET-B','ET-C'))
data$bacteria<-factor(data$bacteria,levels=unique(data$bacteria))


# Load the ggplot2 package
library(ggplot2)

p<-ggplot(data, aes(factor(enterotype), median)) +
  geom_col(aes(fill = bacteria), position = 'dodge', width = 0.8, color = "black") +
  geom_errorbar(
    aes(ymin = lower, ymax = upper, group = bacteria),
    position = position_dodge(width = 0.8),
    width = 0.2,
    size = 1
  ) +
  scale_fill_brewer(palette = "Set2") +
  guides(fill = guide_legend(title = NULL, title.theme = element_text(size = 8, face = "italic"), label.theme = element_text(size = 8, face = "italic"))) +
  labs(
    y = "Relative abundance (%)",  # Modify the y-axis label
  ) +
  theme_test() +
  theme(
    plot.title = element_text(hjust = 0.5),  
    legend.position = 'bottom',
    axis.text = element_text(size = 10, face = "bold", color = "black"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title.x = element_blank()  # remove the x-axis labels
  )+ylim(0,40)

p
ggsave("genus.svg", plot = p)

