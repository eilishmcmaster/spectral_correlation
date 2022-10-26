# correlation script for Allison by Eilish 

library(openxlsx)
library(readxl)
library(circlize)
library(ComplexHeatmap)
library(dplyr)

#set wormatg directory 
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



### finding the >0.98 groups

mat2 <- as.data.frame(mat) %>%mutate_all(~replace(.,.<0.98, 0)) #VERY IMPORTANT, removes all of the pairwise connections that are k<0.98

mat3<- as.data.frame(mat2) %>%mutate_all(~replace(.,.>0, 1))
library(igraph)
network <- graph_from_adjacency_matrix(as.matrix(mat3), mode="undirected", diag=F, weighted=NULL) #makes the network based on k>0.98 plot(network)

ceb <- cluster_fast_greedy(network) # make the bubbles for the network plot

plot(ceb, network, vertex.label.color="transparent", vertex.size=2, edge.width=0.4) #make the network plot


group_df <-as.data.frame(cbind(group=ceb$membership, sample=ceb$names))#get the clones from the network as a df
group_df$group <- as.numeric(group_df$group)

# write the groups to an excel spreadsheet
write.xlsx(group_df, "098_groups.xlsx", rowNames=FALSE)
