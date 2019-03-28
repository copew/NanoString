##argh

##load data
library(readr)

mb_expression <- as.data.frame(read_csv2("mb_expression.csv"))
rownames(mb_expression) <- mb_expression$X1
mb_expression <- mb_expression[, -1]

##
#install.packages("pheatmap")
library(pheatmap)
library(RColorBrewer)
cols <- rev(brewer.pal(6, "RdBu"))


mb_expression <- as.matrix(mb_expression)
mb_expression <- t(mb_expression)
mb_expression <- mb_expression[,match(clust_corr$MB.ID, colnames(mb_expression))]
nanoStringIC <- nanoStringIC[,match(clust_corr$MB.ID, colnames(nanoStringIC))]


ann <- data.frame(IntClust=factor(clust_corr$IntClust,levels=1:10),row.names = clust_corr$MB.ID)
ann_col <- as.vector(by(clust_corr,clust_corr$IntClust,function(x){unique(x$`Colour Codes`)}))
names(ann_col) <- levels(ann$IntClust)
ann_col <- list(IntClust=ann_col)
ann <- data.frame(IntClust=factor(clust_corr$IntClust,levels=1:10),row.names = clust_corr$MB.ID)
i <- order(clust_corr$IntClust)

pheatmap(log(nanoStringIC+0.5),labels_row=rep(" ", nrow(nanoStringIC)), labels_col = rep("",ncol(nanoStringIC)),annotation_col = ann,annotation_colors = ann_col, main="NanoString")

pheatmap(log(mb_expression+0.5),labels_row=rep(" ", nrow(mb_expression)), labels_col = rep("",ncol(mb_expression)),annotation_col = ann,annotation_colors = ann_col, main = "Illumina")        





pheatmap(nanoStringIC[,i],labels_row=rep(" ", nrow(nanoStringIC)), color=cols, breaks=c(-10, -1, -0.5, 0, 0.5, 1, 10), scale = "row",labels_col = rep("",ncol(nanoStringIC)),annotation_col = ann,annotation_colors = ann_col,cluster_cols = F, main="NanoString")
tmp <- pheatmap(mb_expression[,i],labels_row=rep(" ", nrow(mb_expression)), color=cols, breaks=c(-10, -1, -0.5, 0, 0.5, 1, 10), scale = "row", labels_col = rep("",ncol(mb_expression)),annotation_col = ann,cluster_cols = F, annotation_colors = ann_col,main="Illumina")
# genes in the order of the heatmap
tmp$tree_row$labels[tmp$tree_row$order]


# 
#  j <- order(apply(log(nanoStringIC+0.5),1,var),decreasing = T)
#  pheatmap(log(nanoStringIC+0.5)[j,i][1:50,],labels_col = rep("",ncol(nanoStringIC)),labels_row = rownames(nanoStringIC)[j][1:50],annotation_col = ann,annotation_colors = ann_col,cluster_cols = T)
# # 
# k <- which(rownames(nanoStringIC) %in% c("TACO1",	"RPS6KB1", "INTS4", "ALG8", "NOSTRIN","TRIP13", "TBCE", "DAP3", "GRB7",	"PGAP3", "BRF2", "ASH2L", "NME3", "ROGDI", "RRNAD1", 	"COG2", "MAF1", "ZNF7", "FOXA1", "MLPH"))
# pheatmap(log(nanoStringIC+0.5)[k,i],labels_col = rep("",ncol(nanoStringIC)),labels_row = rownames(nanoStringIC)[k],annotation_col = ann,annotation_colors = ann_col,cluster_cols = F)


?pheatmap
