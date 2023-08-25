# Import required packages
library(ggplot2)
library(reshape2)
library(dplyr)

# create data frame
data <- data.frame(
  Enterotype = c("ET-L", "ET-B", "ET-C"),
  UC = c(153, 306, 162),
  HC = c(589, 118, 0)
)

# Convert data to long format
data_long <- melt(data, id.vars = "Enterotype", variable.name = "Group", value.name = "Count")


# Calculate the total number of each Enterotype
enterotype_totals <- data_long %>%
  group_by(Enterotype) %>%
  summarise(Total = sum(Count))

# join enterotype_totals with raw data and calculate percentages
data_long <- data_long %>%
  left_join(enterotype_totals, by = "Enterotype") %>%
  mutate(Percentage = Count / Total * 100)

# Change the horizontal order of Enterotype
data_long$Enterotype <- factor(data_long$Enterotype, levels = c("ET-C", "ET-B", "ET-L"))
data_long$Group <- factor(data_long$Group, levels = c("HC", "UC"))

p <- ggplot(data_long, aes(x = Enterotype, y = Count, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_dodge(width = 0.5), size=5, vjust = 0.3, hjust = -0.1) +
  scale_fill_brewer(palette ='Set1') +
  coord_flip() +
  labs(
    title = NULL,
    x = NULL,
    y = "Number of sample subjects",
    fill = NULL
  ) +
  theme_test() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.9,0.15),
    legend.text = element_text(size = 15), # Adjust the size of the legend
    axis.text = element_text(size = 15, face = "bold", colour = 'black'),
    axis.title.x = element_text(size = 15, face = "bold"), # Adjust the font size of the x-axis labels
    panel. border = element_rect(colour = "black", fill = NA, size = 1.5)
  )+
  ylim(0,630)


p <- p +
  annotate("text",
           x = Inf,
           y = Inf,
           label = "Chi-square test: p<0.05",
           hjust = 1.1,
           vjust = 1.3,
           size = 5,
           color = "black")


# display graphics
print(p)

ggsave("entertype_count.svg", plot = p, width = 7.5, height = 5)