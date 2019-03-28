##load things
library(reshape2)
library(NanoStringNorm)
library(ggplot2)
install.packages("pastecs")
library(pastecs)
library(dplyr)
library(plyr)

#data files
BestIntClust <- read.csv("~/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/Other Info/BestIntClust.csv")
nanoStringIC <- read.csv("C:\\Users\\cope01\\Documents\\OneDrive - University Of Cambridge\\Documents\\PhD\\Nanostring\\Other Info\\nanoStringIC.csv")

# #combine them together
# colnames(nanoStringIC)[1] <- "Gene"
# colnames(nanoStringIC)
# nanoStringIC_cluster <- merge(nanoStringIC, BestIntClust, by="Gene")
# 
# rownames(nanoStringIC_cluster)
# 
# clust_corr$MB.ID <- gsub("-", ".", clust_corr$MB.ID)
# rownames(clust_corr) <- clust_corr$MB.ID
# clust_corr_t <- data.frame(t(clust_corr))
# colnames(clust_corr_t)
# clust_corr_t <- clust_corr_t[1,]
# colnames(nanoStringIC_cluster)
# 
# 
# 
# # colnames(nanoStringIC_cluster <- )
# 
# #colnames(nanoStringIC_cluster)
# 
# #subsetting
# 
# ic1genes <- nanoStringIC_cluster[which(nanoStringIC_cluster$IntClust=="1"), ]
# ic2genes <- nanoStringIC_cluster[which(nanoStringIC_cluster$IntClust=="2"), ]
# ic3genes <- nanoStringIC_cluster[which(nanoStringIC_cluster$IntClust=="3"), ]
# ic4genes <- nanoStringIC_cluster[which(nanoStringIC_cluster$IntClust=="4"), ]
# ic5genes <- nanoStringIC_cluster[which(nanoStringIC_cluster$IntClust=="5"), ]
# ic6genes <- nanoStringIC_cluster[which(nanoStringIC_cluster$IntClust=="6"), ]
# ic7genes <- nanoStringIC_cluster[which(nanoStringIC_cluster$IntClust=="7"), ]
# ic8genes <- nanoStringIC_cluster[which(nanoStringIC_cluster$IntClust=="8"), ]
# ic9genes <- nanoStringIC_cluster[which(nanoStringIC_cluster$IntClust=="9"), ]
# ic10genes <- nanoStringIC_cluster[which(nanoStringIC_cluster$IntClust=="10"), ]

##plotting



##oscar comes to the rescu

clust_corr <- clust_corr[match(colnames(nanoStringIC)[-1], clust_corr$MB.ID),]
mean(clust_corr$MB.ID == colnames(nanoStringIC)[-1])


##cluster 10

ids <- BestIntClust[which(BestIntClust$IntClust==10),'Gene']
png("Genes_Cluster10.png", width=2400, height=2000)
par(mfrow=c(8, 9))
for (i in ids) {
  boxplot(as.numeric(nanoStringIC[which(nanoStringIC[,1] == i),-1]) ~ clust_corr$IntClust, main=i)
}
dev.off()

#clust 9
ids <- BestIntClust[which(BestIntClust$IntClust==9),'Gene']
png("Genes_Cluster9.png", width=2400, height=2000)
par(mfrow=c(5, 7))
for (i in ids) {
  boxplot(as.numeric(nanoStringIC[which(nanoStringIC[,1] == i),-1]) ~ clust_corr$IntClust, main=i)
}
dev.off()

#clust 8
ids <- BestIntClust[which(BestIntClust$IntClust==8),'Gene']
png("Genes_Cluster8.png", width=2400, height=2000)
par(mfrow=c(5, 7))
for (i in ids) {
  boxplot(as.numeric(nanoStringIC[which(nanoStringIC[,1] == i),-1]) ~ clust_corr$IntClust, main=i)
}
dev.off()
ids

#cluster 7
ids <- BestIntClust[which(BestIntClust$IntClust==7),'Gene']
png("Genes_Cluster7.png", width=2400, height=2000)
par(mfrow=c(2, 2))
for (i in ids) {
  boxplot(as.numeric(nanoStringIC[which(nanoStringIC[,1] == i),-1]) ~ clust_corr$IntClust, main=i)
}
dev.off()

#cluster 6
ids <- BestIntClust[which(BestIntClust$IntClust==6),'Gene']
png("Genes_Cluster6.png", width=2400, height=2000)
par(mfrow=c(3, 3))
for (i in ids) {
  boxplot(as.numeric(nanoStringIC[which(nanoStringIC[,1] == i),-1]) ~ clust_corr$IntClust, main=i)
}
dev.off()
ids

#cluster 5
ids <- BestIntClust[which(BestIntClust$IntClust==5),'Gene']
png("Genes_Cluster5.png", width=2400, height=2000)
par(mfrow=c(3, 4))
for (i in ids) {
  boxplot(as.numeric(nanoStringIC[which(nanoStringIC[,1] == i),-1]) ~ clust_corr$IntClust, main=i)
}
dev.off()

#cluster 4
ids <- BestIntClust[which(BestIntClust$IntClust==4),'Gene']
png("Genes_Cluster4.png", width=2400, height=2000)
par(mfrow=c(5, 6))
for (i in ids) {
  boxplot(as.numeric(nanoStringIC[which(nanoStringIC[,1] == i),-1]) ~ clust_corr$IntClust, main=i)
}
dev.off()
ids

#cluster 3
ids <- BestIntClust[which(BestIntClust$IntClust==3),'Gene']
png("Genes_Cluster3.png", width=2400, height=2000)
par(mfrow=c(3, 4))
for (i in ids) {
  boxplot(as.numeric(nanoStringIC[which(nanoStringIC[,1] == i),-1]) ~ clust_corr$IntClust, main=i)
}
dev.off()
ids


#cluster 2
ids <- BestIntClust[which(BestIntClust$IntClust==2),'Gene']
png("Genes_Cluster2.png", width=2400, height=2000)
par(mfrow=c(3, 4))
for (i in ids) {
  boxplot(as.numeric(nanoStringIC[which(nanoStringIC[,1] == i),-1]) ~ clust_corr$IntClust, main=i)
}
dev.off()
ids