#plot log distribution of genes per cluster


#loading libraries



#loading data
setwd("/Users/cope01/Documents/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/R scripts")
load("nanoStringIC_DF.RData")

#remove na
nanoStringIC_DF <- na.omit(nanoStringIC_DF)
nanoStringIC_DF_log <- log(nanoStringIC_DF[, -1]+1)
rownames(nanoStringIC_DF_log) <- rownames(nanoStringIC_DF)
nanoStringIC_DF_log <- as.data.frame(nanoStringIC_DF_log)
nanoStringIC_DF_log <- data.frame(iClust=nanoStringIC_DF$iClust, nanoStringIC_DF_log)

min_value <- apply(nanoStringIC_DF, 2, min)

#plotting per gene

pdf("Plots_per_gene_logged.pdf", width=8, height=10)
par(mfrow=c(2, 2))
for (i in 2:ncol(nanoStringIC_DF_log)) {
  tmp <- data.frame(nanoStringIC_DF_log$iClust, nanoStringIC_DF_log[,i])
  plot(x=tmp$nanoStringIC_DF_log.iClust, y=tmp$nanoStringIC_DF_log...i, 
  xlab="iClust",
  ylab="Gene Count (log)",
  main = colnames(nanoStringIC_DF_log)[i]
  )
}
dev.off()


