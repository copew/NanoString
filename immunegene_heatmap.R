#immune gene heatmap

#loading libraries
library(pheatmap)
library(RColorBrewer)
cols <- rev(brewer.pal(6, "RdBu"))
#loading data
load("normalised_immune_genes_log.RData")
load("iC10_colour.RData")

immune <- norm_immune_log[, -1]
immune <- t(as.matrix(immune))



iclust <- data.frame(ID=rownames(norm_immune_log), iClust=norm_immune_log$iClust)
orderic <- order(iclust$iClust)
iclust <- iclust[order(iclust$iClust), ]


iclustcolour <- merge(iclust, coliClust10, by='iClust', sort=FALSE)

immune <- immune[, orderic]



heatmap(immune, Colv=NA, scale = "row", ColSideColors = iclustcolour$`Colour Codes`, col=cols, breaks=c(-10, -1, -0.5, 0, 0.5, 1, 10))


#now only the significant ones

load("differential_immune_genes.RData")
feature.sig
sig.immune <- immune[rownames(immune) %in% feature.sig$ID, ]
dim(sig.immune)
heatmap(sig.immune, scale = "row", ColSideColors = iclustcolour$`Colour Codes`,col=cols, breaks=c(-10, -1, -0.5, 0, 0.5, 1, 10))


save(sig.immune, file="sig.immune.RData")
