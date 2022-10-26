# correlation script for Allison by Eilish 

library(openxlsx)
library(readxl)
library(circlize)
library(ComplexHeatmap)
library(dplyr)

#set working directory 
setwd("/Users/eilishmcmaster/Documents/spectral_correlation/")

# read in data where samples are columns and channels are rows
data <- read_xlsx("MFI dereplication.xlsx",
                  col_names=TRUE)

# remove column 1 (channel names)
data[,1] <- NULL

# do the pearson correlation
mat <- cor(data, method="pearson")

# make the colours for the matrix
colz <-  colorRamp2(c(0,0.5,1), # min, mid, and max values
                    c("#8DD3C7", "white", "#FB8072")) # corresponding colours

#make the matrices
# Use for larger matrices
# automatically order samples by clustering
Heatmap(mat, 
        col=colz,
        row_names_gp = gpar(fontsize = 10),
        column_names_gp = gpar(fontsize = 10),
        row_names_max_width = unit(15, "cm"),
        border_gp = gpar(col = "black", lty = 1),
        name="Pearson\ncorrelation")

# order samples by their order in the matrix 
Heatmap(mat, 
        col=colz,
        row_names_gp = gpar(fontsize = 10),
        column_names_gp = gpar(fontsize = 10),
        row_names_max_width = unit(15, "cm"),
        border_gp = gpar(col = "black", lty = 1),
        column_order=rownames(mat),
        row_order=rownames(mat),
        name="Pearson\ncorrelation")


# # use only for small matrices, has numbers inside matrix cells
# Heatmap(mat, 
#                 col=colz,
#                 row_names_gp = gpar(fontsize = 10),
#                 column_names_gp = gpar(fontsize = 10),
#                 row_names_max_width = unit(15, "cm"),
#                 border_gp = gpar(col = "black", lty = 1),
#                 name="Pearson\ncorrelation",
#                 cell_fun = function(j, i, x, y, width, height, fill) {
#                   grid.text(sprintf("%.2f", mat[,1:nrow(mat)][i, j]), x, y, gp = gpar(fontsize = 6))})

# write the matrix to an excel spreadsheet
write.xlsx(as.data.frame(mat), "correlation_matrix.xlsx", rowNames=TRUE)
