#plot per gene for immune genes
#library
library(NanoStringNorm)


#loading data
setwd("/Users/cope01/Documents/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/R scripts")

#loading data
load("nanoStringIC_DF.RData")

#remove na
nanoStringIC_DF <- na.omit(nanoStringIC_DF)
batchNumber <- "Combined"
metadataFolder <- "/Users/cope01/Documents/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/Other Info/"
nanostringDataFolder <- paste0("/Users/cope01/Documents/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/Digital Analyser/",batchNumber,"/")
outputFolder <- paste0("/Users/cope01/Documents/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/Nanostring QC/",batchNumber,"QC/")


#Get int clust data
iClust <- read.csv(paste0(metadataFolder,"IntClust.csv"))
iC10genesFile <- paste0(metadataFolder,"for_nanostring.txt")
iCgenes220 <- scan(iC10genesFile,what = character())
#Get clinical data
clinical <- read.csv(paste0(metadataFolder,"Metabric.Clinical.csv"))
colnames(clinical)[2] <- "ID"

#getting MB-AD files
nanoStringFiles <- list.files(nanostringDataFolder, pattern = "MB-AD", recursive = T, full.names = T)
#Getting NT files
nanoStringFiles<-c(nanoStringFiles, list.files(nanostringDataFolder, pattern="NT", recursive = T, full.names=T))
#Getting TN files
nanoStringFiles<-c(nanoStringFiles,
                   list.files(nanostringDataFolder, pattern="TN", recursive = T, full.names=T))

#get csv
nanoStringData <- lapply(nanoStringFiles, read.csv,
                         skip = 26, header = T, nrows = 773,quote=NULL,
                         stringsAsFactors = F)

#extract id from file id
SampleID <- regmatches(nanoStringFiles, regexpr("MB-AD-[0-9]*", nanoStringFiles))
SampleID <- c(SampleID, regmatches(nanoStringFiles, regexpr("NT-[0-9]*", nanoStringFiles)))
SampleID <- c(SampleID, regmatches(nanoStringFiles, regexpr("TN-[0-9]*", nanoStringFiles)))


for (i in seq_along(nanoStringData)){
  colnames(nanoStringData[[i]])[colnames(nanoStringData[[i]])=="Count"] <-SampleID[[i]]
}

#convert list to dataframe
nanoStringData <- do.call("cbind", nanoStringData)
nanoStringData <- nanoStringData[, !duplicated(colnames(nanoStringData), fromLast = FALSE)] 



#normalisation using NanoStringNorm
normalisedNanoStringData <- NanoStringNorm(
  x = nanoStringData,
  anno = NA,
  CodeCount = 'sum',
  Background = 'mean',
  SampleContent = 'housekeeping.sum',
  round.values = FALSE,
  take.log = FALSE,
  return.matrix.of.endogenous.probes = TRUE
)


normData <- na.omit(normalisedNanoStringData)
normData <- t(normData)
rownames(normData) <- sub("-AD-", "-", rownames(normData), fixed=T)

sample_iclust <- as.data.frame(nanoStringIC_DF[,1])
rownames(sample_iclust) <- rownames(nanoStringIC_DF)
colnames(sample_iclust) <- "iClust"
normData <- merge(sample_iclust, normData, by="row.names")
rownames(normData) <- normData$Row.names
normData <- normData[ ,-1]
norm_immune <- normData[ ,!colnames(normData) %in% iCgenes220]




#remove na
norm_immune <- na.omit(norm_immune)
norm_immune_log <- log(norm_immune[, -1]+1)
rownames(norm_immune_log) <- rownames(norm_immune)
norm_immune_log <- as.data.frame(norm_immune_log)
norm_immune_log <- data.frame(iClust=norm_immune$iClust, norm_immune_log)

min_value <- apply(norm_immune, 2, min)

#plotting per gene

pdf("Plots_per_immune_gene_logged.pdf", width=8, height=10)
par(mfrow=c(2, 2))
for (i in 2:ncol(norm_immune_log)) {
  tmp <- data.frame(norm_immune_log$iClust, norm_immune_log[,i])
  plot(x=tmp$norm_immune_log.iClust, y=tmp$norm_immune_log...i, 
       xlab="iClust",
       ylab="Gene Count (log)",
       main = colnames(norm_immune_log)[i]
  )
}
dev.off()


save(norm_immune_log, file="normalised_immune_genes_log.RData")
save(norm_immune, file="normalised_immune_genes.RData")
write.csv(norm_immune_log, file = "normalised_immune_genes_log.csv")
write.csv(norm_immune, file = "normalised_immune_genes.csv")