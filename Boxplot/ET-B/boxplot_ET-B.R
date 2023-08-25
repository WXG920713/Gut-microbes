library(ggplot2)
library(ggpubr)
library(dplyr)
library(RColorBrewer)
library(gg.gap)
data<-read.delim('graph.txt',header=T,sep = '\t',stringsAsFactors = F, check.names = F)

# First, we need to convert the dataframe from wide to long format
data_long <- reshape2::melt(data, id.vars = c("ID","group"))
data_long$variable<-factor(data_long$variable,levels = unique(data_long$variable))

# Run the Wilcoxon test for each variable
stat.test1 <- compare_means(
  value ~ group, data = data_long, group.by = "variable",
  method = "t.test")


p <- ggplot(data_long, aes(x = variable, y = value)) +
  ylim(c(0, 30))+
  stat_boxplot(aes(fill=group), geom = 'errorbar', width = 0.8, position = position_dodge(width = 0.8)) +
  geom_jitter(aes(fill = group), color = "black", position = position_dodge(width = 0.8), size = 1, shape = 21, alpha = 0.4) +
  geom_boxplot(aes(fill = group), position = position_dodge(width=0.8), width = 0.6, outlier.shape = NA) +
  scale_fill_brewer(palette = 'Set1') +
  theme_test()  +
  labs(y = "Relative Abundance(%)", x = "")  +
  theme(
    axis.text.x = element_text(size = 10, color = 'black', angle = 45, hjust = 1, face = "italic"),  # 斜体且加粗
    axis.text.y = element_text(size = 10,  color = 'black'),  # 正常且加粗
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.justification = "right",
    legend.position = "right")

p1<-p + 
  stat_pvalue_manual(stat.test1, x = "variable", label = "p.adj", y.position = 29, position = position_dodge(width = 1),size=3) +
  geom_segment(data = stat.test1, aes(x = as.numeric(variable) - 0.4, xend = as.numeric(variable) + 0.4, y = 28.5, yend = 28.5)) +
  geom_segment(data = stat.test1, aes(x = as.numeric(variable) - 0.4, xend = as.numeric(variable) - 0.4, y = 28.5, yend = 28.2)) +
  geom_segment(data = stat.test1, aes(x = as.numeric(variable) + 0.4, xend = as.numeric(variable) + 0.4, y = 28.5, yend = 28.2))

p2<-gg.gap(plot=p1,
          segments=c(10,24),# split part
          tick_width = 1, # scale interval
          rel_heights = c(0.8,0,0.2),#The ratio of the upper part of the image segmentation to the middle blank and the lower part
          ylim=c(0, 30))#Scale upper and lower limits

p3<-p2+theme(plot.margin = unit(c(1, 1, 1, 1.5), "cm"))

ggsave('ET-B.svg',width=10,height=7, plot = p3)

