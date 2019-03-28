##summary statistics
library(reshape2)
library(NanoStringNorm)
library(ggplot2)
install.packages("pastecs")
library(pastecs)

##colour codes
coliClust <- c('#FF5500', '#00EE76', '#CD3278','#00C5CD', '#B5D0D2', '#8B0000', '#FFFF40', '#0000CD', '#FFAA00', '#EE82EE', '#7D26CD')
coliClust <- data.frame(c("1", "2", "3", "4ER+", "4ER-", "5", "6", "7","8", "9", "10"),coliClust)
colnames(coliClust)[1] <- "IntClust"
colnames(coliClust)[2] <- "Colour Codes"
coliClust10 <- c('#FF5500', '#00EE76', '#CD3278','#00C5CD', '#8B0000','#FFFF40', '#0000CD', '#FFAA00', '#EE82EE', '#7D26CD')
coliClust10 <- data.frame(c("1", "2", "3", "4", "5", "6", "7","8", "9", "10"),coliClust10)
colnames(coliClust10)[1] <- "IntClust"
colnames(coliClust10)[2] <- "Colour Codes"
coliClust10$`Colour Codes` <- as.character(coliClust10$`Colour Codes`)

##data
batchNumber <- "Combined"
metadataFolder <- "C:\\Users\\cope01\\Documents\\OneDrive - University Of Cambridge\\Documents\\PhD\\Nanostring\\Other Info\\"
nanostringDataFolder <- paste0("C:\\Users\\cope01\\Documents\\OneDrive - University Of Cambridge\\Documents\\PhD\\Nanostring\\Digital Analyser\\",batchNumber,"\\")
outputFolder <- paste0("C:\\Users\\cope01\\Documents\\OneDrive - University Of Cambridge\\Documents\\PhD\\Nanostring\\Nanostring QC\\",batchNumber,"QC\\")

dir.create(outputFolder,showWarnings = FALSE)

iC10genesFile <- paste0(metadataFolder,"for_nanostring.txt")
metabricExpressionData <- paste0(metadataFolder,"METABRIC_CURTIS_EXPRESSION.RData")


##other data
dir.create(outputFolder,showWarnings = FALSE)

iC10genesFile <- paste0(metadataFolder,"for_nanostring.txt")
metabricExpressionData <- paste0(metadataFolder,"METABRIC_CURTIS_EXPRESSION.RData")


#pdf(paste0(outputFolder,"metrics.pdf"), width=11, height=8)

nanoStringFiles <- list.files(nanostringDataFolder, pattern = "MB-AD", recursive = T, full.names = T)
nanoStringData <- lapply(nanoStringFiles, read.csv,
                         skip = 26, header = T, nrows = 773,quote=NULL,
                         stringsAsFactors = F)
#extract metabric id from file id
metabricID <- regmatches(nanoStringFiles, regexpr("MB-AD-[0-9]*", nanoStringFiles))

#rename count column to metabric ID
for (i in seq_along(nanoStringData)){
  colnames(nanoStringData[[i]])[colnames(nanoStringData[[i]])=="Count"] <-metabricID[[i]]
}

#convert list to dataframe
nanoStringData <- do.call("cbind", nanoStringData)
nanoStringData <- nanoStringData[, !duplicated(colnames(nanoStringData), fromLast = FALSE)] 

dim(nanoStringData)

##exclude dodgy samples
nanoStringData<-nanoStringData[,!colnames(nanoStringData) %in% c("MB-AD-0414", "MB-AD-0129")]
#head(nanoStringData)
dim(nanoStringData)

librarySize=data.frame(sample=names(nanoStringData[,c(4:ncol(nanoStringData))]),size=colSums(nanoStringData[,c(4:ncol(nanoStringData))]))
ggplot(librarySize,aes(x=sample,y=size))+
  geom_bar(stat="identity",color="black",fill="gray80")+
  labs(x="Sample",y="Total counts",title="Distribution of raw counts across samples")+
  theme_bw()+
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
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(meltednormalisedNanoStringData,aes(x=Var2,y=log(value+1.1)))+
  geom_boxplot(color="black",fill="gray80")+
  labs(x="Sample",y="Log Normalised total counts",title="Distribution of normalised counts within sample")+
  scale_y_continuous(breaks=c(0,2,4,6,8,10))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

typeof(normalisedLibrarySize)
typeof(librarySize)

combined_size <- merge(normalisedLibrarySize, librarySize, by="MB.ID")
head(combined_size)


##combined density plot of raw count and normalised count
melted_size <- melt(combined_size)
head(combined_size)
ggplot(melted_size, aes(x=value))+
  theme_bw(base_size = 16)+
  geom_density(aes(group=variable, fill=variable), alpha=0.6,color="white")+
  scale_x_continuous(name = "Count")+
  scale_y_continuous(name="Density")+
  scale_fill_discrete(name="Type of Count", breaks=c("norm.size","size"),labels=c("After Normalisation","Before Normalisation"))


summary(combined_size$size)



##normalised count vs raw count

ggplot(combined_size, aes(x=size, y=norm.size))+
  geom_jitter()+
  theme_bw()+
  xlab("Absolute Count")+
  ylab("Normalised Count")

cor(combined_size$norm.size,combined_size$size, method="spearman")
##or 
#with(combined_size,cor(norm.size,size,method="spearman"))


meltedMeans<-data.frame(gene=rownames(normalisedNanoStringData),mean=rowMeans(normalisedNanoStringData))
meltedMeans[meltedMeans$mean<=1,]
meltedMeans[meltedMeans$mean>1000,]

#distribution of all genes
ggplot(meltedMeans,aes(x=(mean)))+
   geom_histogram(binwidth = 0.25, color="black", fill="gray80")+
   theme_bw()+
   scale_x_log10(breaks=c(0,1,5,10,25,50,100,250,500,1000,2500,5000,10000))+
   labs(x="Mean Normalised Nanostring Count across samples (log scale)",y="Number of genes",title="Distribution of Counts")

summary(meltedMeans)

#220 genes identfied by marcus
iCgenes220 <- scan(iC10genesFile,what = character())

dim(normalisedNanoStringData)

nanoStringIC<-normalisedNanoStringData[rownames(normalisedNanoStringData) %in% iCgenes220,]
meltedMeans<-data.frame(gene=rownames(nanoStringIC),mean=rowMeans(nanoStringIC))
# ggplot(meltedMeans,aes(x=(mean)))+
#   geom_histogram(binwidth = 0.25, color="black", fill="gray80")+
#   theme_bw()+
#   scale_x_log10(breaks=c(0,1,5,10,25,50,100,250,500,1000,2500,5000,10000))+
#   labs(x="Mean Normalised Nanostring Count across samples (log scale)",y="Number of genes",title="Distribution of iC10 classifier gene counts")

dev.off()


load(metabricExpressionData)

mb_expression <- Exp[which(Exp[,1] %in% rownames(nanoStringIC)),]

nanoStringIC=nanoStringIC[sort(rownames(nanoStringIC)),]
mb_expression[,1]<-as.character(mb_expression[,1])
mb_expression <- mb_expression[match(rownames(nanoStringIC), mb_expression[,1]),]
mb_expression=mb_expression[!is.na(mb_expression$SYMBOL),]
colnames(mb_expression) <- sub(".", "-", colnames(mb_expression), fixed=T)
colnames(mb_expression)

colnames(nanoStringIC) <- sub("-AD-", "-", colnames(nanoStringIC), fixed=T)
colnames(nanoStringIC)

rownames(mb_expression) <- mb_expression[,1]
mb_expression <- mb_expression[,which(colnames(mb_expression) %in% colnames(nanoStringIC))]

#dim(mb_expression)
#colnames(mb_expression)
nanoStringIC <- nanoStringIC[,colnames(mb_expression)]
nanoStringIC <- nanoStringIC[rownames(nanoStringIC)%in% rownames(mb_expression),]
#dim(nanoStringIC)

mean(rownames(nanoStringIC) == rownames(mb_expression))
mean(colnames(nanoStringIC) == colnames(mb_expression))



##number of samples
cors <- data.frame(
  cors=sapply(1:ncol(mb_expression),function(i){
    cor(nanoStringIC[,i], mb_expression[,i],method="spearman")
  }),
  MB.ID=colnames(mb_expression)
)
MB_intClust <- read.csv("~/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/MB intClust.csv")
#View(cors)
#View(MB_intClust)
clust_corr <- merge(cors, MB_intClust, by='MB.ID')
clust_corr$IntClust <- as.factor(clust_corr$IntClust)
clust_corr <- merge(clust_corr, coliClust10, by="IntClust")
clust_corr$`Colour Codes` <- as.character(clust_corr$`Colour Codes`)

# how many per group
clust_number <- table(clust_corr$IntClust)
View(clust_number)

librarySize$MB.ID <- gsub("-AD", "", librarySize$sample)
clust_corr$MB.ID <- as.character(clust_corr$MB.ID)
cluster_librarysize <- merge(clust_corr, librarySize, by='MB.ID')

colnames(cluster_librarysize)

ggplot(cluster_librarysize, aes(x=IntClust, y=size))+
  theme_bw()+
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(width=0.25, col=cluster_librarysize$`Colour Codes`)+
  ylab("Absolute Count per Sample")

summary(cluster_librarysize$size)
summary(aov(size~IntClust, data=cluster_librarysize))

normalisedLibrarySize$MB.ID <- gsub("-AD", "", normalisedLibrarySize$sample)
colnames(normalisedLibrarySize)[2] <- "norm.size"

cluster_normlibrarysize <- merge(clust_corr, normalisedLibrarySize, by="MB.ID")
ggplot(cluster_normlibrarysize, aes(x=IntClust, y=norm.size))+
  theme_bw()+
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(width=0.25, col=cluster_normlibrarysize$`Colour Codes`)+
  ylab("Normalised Count per Sample")

summary(aov(norm.size~IntClust, data=cluster_normlibrarysize))
summary(cluster_normlibrarysize$norm.size)


summary(aov(cors~IntClust, data=clust_corr))

