#install.packages("pheatmap")
library(pheatmap)
library(readr)
mb_expression <- as.data.frame(read_csv2("mb_expression.csv"))
rownames(mb_expression) <- mb_expression$X1
mb_expression <- mb_expression[, -1]


ann <- data.frame(IntClust=factor(clust_corr$IntClust,levels=1:10),row.names = colnames(nanoStringIC))

ann_col <- as.vector(by(clust_corr,clust_corr$IntClust,function(x){unique(x$`Colour Codes`)}))
names(ann_col) <- levels(ann$IntClust)
ann_col <- list(IntClust=ann_col)




pheatmap(log(mb_expression+0.5),labels_row=rep(" ", nrow(mb_expression)), labels_col = rep("",ncol(mb_expression)),annotation_col = ann,annotation_colors = ann_col, main = "Illumina")

