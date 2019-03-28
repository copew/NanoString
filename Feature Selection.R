library(NanoStringNorm)
library(reshape2)

batchNumber <- "Combined"
metadataFolder <- "/Users/cope01/Documents/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/Other Info/"
nanostringDataFolder <- paste0("/Users/cope01/Documents/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/Digital Analyser/",batchNumber,"/")
outputFolder <- paste0("/Users/cope01/Documents/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/Nanostring QC/",batchNumber,"QC/")


#Get int clust data
iClust <- read.csv(paste0(metadataFolder,"IntClust.csv"))

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

nsdata_total <- normalisedNanoStringData[, 4:ncol(normalisedNanoStringData)]
#dim(normalisedNanoStringData)
#dim(nsdata_total)
colnames(nsdata_total) <- sub("-AD-", "-", colnames(nsdata_total), fixed=T)
nsdata_total <- as.matrix(t(nsdata_total))

#add iclust
nsdata_total <- cbind(iClust = iClust[match(rownames(nsdata_total),iClust$ID),]$IntClust,nsdata_total)

#remove NA
nsdata_total <- na.omit(nsdata_total)
save(nsdata_total, file="nsdata_total.RData")


#excluded samples
library(readr)
exclusion <- read_csv("~/Documents/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/Other Info/exclusion.csv") 

#exclude these

nsdata_tidy <- nsdata_total[!(rownames(nsdata_total) %in% exclusion$ID), ]

#dim(nsdata_total)
#dim(nsdata_tidy)

###should be 442 samples, 750 genes at this stage

save(nsdata_tidy, file="nsdata_tidy.RData")


#220 genes identfied by marcus
iC10genesFile <- paste0(metadataFolder,"for_nanostring.txt")
iCgenes220 <- scan(iC10genesFile,what = character())

#keep only these 220 genes
nanoStringIC<-normalisedNanoStringData[rownames(normalisedNanoStringData) %in% iCgenes220,]
colnames(nanoStringIC) <- sub("-AD-", "-", colnames(nanoStringIC), fixed=T)
nanoStringIC <- as.matrix(t(nanoStringIC))

#matrix
nanoStringIC <- cbind(iClust = iClust[match(rownames(nanoStringIC),iClust$ID),]$IntClust,nanoStringIC)

#dataframe
nanoStringIC_DF <- as.data.frame(nanoStringIC)
nanoStringIC_DF$iClust <- as.factor(nanoStringIC_DF$iClust)




##now the distribution
features <- data.frame(
  p=sapply(2:ncol(nanoStringIC_DF),function(i){
    anova(lm(log(nanoStringIC_DF[,i]+0.001) ~ nanoStringIC_DF$iClust))$"Pr(>F)"[1]
  }),
  ID=colnames(nanoStringIC_DF)[-1]
)


save(nanoStringIC_DF, file = "nanoStringIC_DF.RData")


