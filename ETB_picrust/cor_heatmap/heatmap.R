library(pheatmap)
data <- read.delim("heatmap.txt",header=T,row.names=1,sep = '\t',stringsAsFactors = F, check.names = F) 

# Set the graph size
options(repr.plot.width = 5.2, repr.plot.height = 5, units = "in", res = 1200)
# Create the SVG file
svg("heatmap.svg", width = 5.2, height = 5)

pheatmap(data, treeheight_row = 0, treeheight_col = 0, border_color = "black", scale = "row", 
         border = FALSE, cellwidth = 15, cellheight = 15, xlab = "Samples")

# close graphics device
dev.off()








