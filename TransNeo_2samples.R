library(NanoStringNorm)
library(ggplot2)
library(reshape2)
dev.off()

##remember to change input and output folder names!!!!!!!!!!!!!!####

batchNumber <- "Combined"
metadataFolder <- "/Users/cope01/Documents/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/Other Info/"
nanostringDataFolder <- paste0("/Users/cope01/Documents/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/Digital Analyser/",batchNumber,"/")
outputFolder <- paste0("/Users/cope01/Documents/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/Nanostring QC/",batchNumber,"QC/")

dir.create(outputFolder,showWarnings = FALSE)


pdf(paste0(outputFolder,"metrics.pdf"), width=11, height=8)
nanoStringFiles <- list.files(nanostringDataFolder, pattern = "TN", recursive = T, full.names = T)
nanoStringFiles<-c(nanoStringFiles, list.files(nanostringDataFolder, pattern="TN", recursive = T, full.names=T))
nanoStringData <- lapply(nanoStringFiles, read.csv,
                         skip = 26, header = T, nrows = 773,quote=NULL,
                         stringsAsFactors = F)

#extract metabric id from file id
SampleID <- regmatches(nanoStringFiles, regexpr("TN-[0-9]*", nanoStringFiles))
SampleID <- c(SampleID, regmatches(nanoStringFiles, regexpr("TN-[0-9]*", nanoStringFiles)))

for (i in seq_along(nanoStringData)){
  colnames(nanoStringData[[i]])[colnames(nanoStringData[[i]])=="Count"] <-SampleID[[i]]
}




#convert list to dataframe
nanoStringData <- do.call("cbind", nanoStringData)
nanoStringData <- nanoStringData[, !duplicated(colnames(nanoStringData), fromLast = FALSE)] 

#dim(nanoStringData)



librarySize=data.frame(sample=names(nanoStringData[,c(4:ncol(nanoStringData))]),size=colSums(nanoStringData[,c(4:ncol(nanoStringData))]))
ggplot(librarySize,aes(x=sample,y=size))+
  geom_bar(stat="identity",color="black",fill="gray80")+
  labs(x="Sample",y="Total counts",title="Distribution of raw counts across samples")+
  theme_bw(base_size=16)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




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


normalisedLibrarySize <- data.frame(sample=colnames(normalisedNanoStringData),size=colSums(normalisedNanoStringData))
meltednormalisedNanoStringData<-melt(normalisedNanoStringData)

ggplot(normalisedLibrarySize,aes(x=sample,y=size))+
  geom_bar(stat="identity",color="black",fill="gray80")+
  labs(x="Sample",y="Normalised total counts",title="Distribution of normalised counts across samples")+
  theme_bw(base_size=16)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(meltednormalisedNanoStringData,aes(x=Var2,y=log(value+1.1)))+
  geom_boxplot(color="black",fill="gray80")+
  labs(x="Sample",y="Log Normalised total counts",title="Distribution of normalised counts within sample")+
  scale_y_continuous(breaks=c(0,2,4,6,8,10))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


meltedMeans<-data.frame(gene=rownames(normalisedNanoStringData),mean=rowMeans(normalisedNanoStringData))
meltedMeans[meltedMeans$mean<=1,]
meltedMeans[meltedMeans$mean>1000,]

#distribution of all genes
ggplot(meltedMeans,aes(x=(mean)))+
  geom_histogram(binwidth = 0.15, color="black", fill="gray80")+
  theme_bw(base_size=16)+
  scale_x_log10(breaks=c(0,1,5,10,25,50,100,250,500,1000,2500,5000,10000))+
  labs(x="Mean Normalised Nanostring Count across samples (log scale)",y="Number of genes",title="Distribution of gene counts")


dev.off()
