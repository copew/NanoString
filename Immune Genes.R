##load things
library(reshape2)
install.packages("NanoStringNorm")
library(NanoStringNorm)
library(ggplot2)
install.packages("pastecs")
library(pastecs)
library(dplyr)
library(plyr)
install.packages("pheatmap")
library(pheatmap)


coliClust <- c('#FF5500', '#00EE76', '#CD3278','#00C5CD', '#B5D0D2', '#8B0000', '#FFFF40', '#0000CD', '#FFAA00', '#EE82EE', '#7D26CD')
coliClust <- data.frame(c("1", "2", "3", "4ER+", "4ER-", "5", "6", "7","8", "9", "10"),coliClust)
colnames(coliClust)[1] <- "Clust11"
colnames(coliClust)[2] <- "Colour Codes"
coliClust$`Colour Codes` <- as.character(coliClust$`Colour Codes`)

###This divides iC4 into iC4ER+ and iC4ER-. You might want to omit that for now, in that case do 


# coliClust10 <- c('#FF5500', '#00EE76', '#CD3278','#00C5CD', '#8B0000','#FFFF40', '#0000CD', '#FFAA00', '#EE82EE', '#7D26CD')
# coliClust10 <- data.frame(c("1", "2", "3", "4", "5", "6", "7","8", "9", "10"),coliClust10)
# colnames(coliClust10)[1] <- "IntClust"
# colnames(coliClust10)[2] <- "Colour Codes"
# coliClust10$`Colour Codes` <- as.character(coliClust10$`Colour Codes`)


metadataFolder <- "C:\\Users\\cope01\\Documents\\OneDrive - University Of Cambridge\\Documents\\PhD\\Nanostring\\Other Info\\"
nanostringDataFolder <- paste0("C:\\Users\\cope01\\Documents\\OneDrive - University Of Cambridge\\Documents\\PhD\\Nanostring\\Digital Analyser\\Combined\\")

iC10genesFile <- paste0(metadataFolder,"for_nanostring.txt")
metabricExpressionData <- paste0(metadataFolder,"METABRIC_CURTIS_EXPRESSION.RData")
NormNSData <- paste0(metadataFolder,"NormalisedNSData.RData")

load(metabricExpressionData)
load(NormNSData)
##file is now called normalisedNanoStringData
##colnames MB ids (MB-AD-****), rownames genes

##loading intclust info and clinical info
MB_intClust <- read.csv("~/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/MB intClust.csv")
colnames(MB_intClust)[3] <- "4ER"
Metabric_Clinical <- read.csv("~/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/Other Info/Metabric.Clinical.csv")
colnames(Metabric_Clinical)[2] <- "MB.ID"


##merge dataframes
cors_Immune <- merge(cors_Immune, MB_intClust, by="MB.ID")
cors_Immune <- merge(cors_Immune, Metabric_Clinical, by="MB.ID")

#220 genes identfied by marcus
iCgenes220 <- scan(iC10genesFile,what = character())
#dim(iCgenes220)
dim(normalisedNanoStringData)

#excluded those
NS_Immune<-normalisedNanoStringData[!rownames(normalisedNanoStringData) %in% iCgenes220,]

dim(NS_Immune)

rownames(NS_Immune)
##clean up names etc.
mb_Immune <- Exp[which(Exp[,1] %in% rownames(NS_Immune)),]
colnames(mb_Immune)
##sorted by gene alphabetically
NS_Immune=NS_Immune[sort(rownames(NS_Immune)),]

mb_Immune[,1]<-as.character(mb_Immune[,1])
mb_Immune <- mb_Immune[match(rownames(NS_Immune), mb_Immune[,1]),]
mb_Immune=mb_Immune[!is.na(mb_Immune$SYMBOL),]
colnames(mb_Immune) <- sub(".", "-", colnames(mb_Immune), fixed=T)
#colnames(mb_Immune)

colnames(NS_Immune) <- sub("-AD-", "-", colnames(NS_Immune), fixed=T)
##colnames(NS_Immune)

rownames(mb_Immune) <- mb_Immune[,1]
mb_Immune <- mb_Immune[,which(colnames(mb_Immune) %in% colnames(NS_Immune))]

#dim(mb_expression)
#colnames(mb_expression)
NS_Immune <- NS_Immune[,colnames(mb_Immune)]
NS_Immune <- NS_Immune[rownames(NS_Immune)%in% rownames(mb_Immune),]
#dim(nanoStringIC)

mean(rownames(NS_Immune) == rownames(mb_Immune))
mean(colnames(NS_Immune) == colnames(mb_Immune))
##now that it's all matched up.... hurray!
##rows: genes; col: MB-****



###do they correlate?

cors_Immune <- data.frame(
  cors=sapply(1:ncol(mb_Immune),function(i){
    cor(NS_Immune[,i], mb_Immune[,i],method="spearman")
  }),
  MB.ID=colnames(mb_Immune)
)

ggplot(cors_Immune, aes(x=cors)) + 
  geom_histogram(binwidth=0.01)+
  theme_bw(base_size=16)+
  labs(x="Correlation of Samples", y="Frequency")


##combine everything together
cors_Immune <- merge(cors_Immune, Metabric_Clinical, by="MB.ID")

##add in cluster 
colnames(cors_Immune)[49] <- "Clust11"
cors_Immune <- merge(cors_Immune, coliClust, by="Clust11")
cors_Immune <- within(cors_Immune, Clust11 <- factor(Clust11, levels=c("1", "2", "3", "4ER-", "4ER+", "5", "6", "7", "8", "9", "10")))

ggplot(cors_Immune, aes(x=Clust11, y=cors)) + 
 theme_bw(base_size=16)+
  geom_jitter(width=0.3, col=cors_Immune$`Colour Codes`, size=2)


##heatmap stuff


# ann <- data.frame(IntClust=factor(clust_corr$IntClust,levels=1:10),row.names = colnames(nanoStringIC))
# 
# ann_col <- as.vector(by(clust_corr,clust_corr$IntClust,function(x){unique(x$`Colour Codes`)}))
# names(ann_col) <- levels(ann$IntClust)
# ann_col <- list(IntClust=ann_col)
# 
