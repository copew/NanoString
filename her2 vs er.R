library(NanoStringNorm)
library(ggplot2)
library(reshape2)

##load data

nanoStringIC <- read.csv("C:\\Users\\cope01\\Documents\\OneDrive - University Of Cambridge\\Documents\\PhD\\Nanostring\\Other Info\\nanoStringIC.csv")
Metabric_Clinical <- read.csv("~/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/Other Info/Metabric.Clinical.csv")


#tidying up
rownames(nanoStringIC) <- nanoStringIC[,1]
nanoStringIC <- nanoStringIC[, -1]
colnames(nanoStringIC) <- gsub("\\.","-",colnames(nanoStringIC))

##subsetting
NS_her2er <- t(nanoStringIC[rownames(nanoStringIC) %in% c("GRB7", "ESR1"), ])


NS_her2er <- cbind(NS_her2er,Metabric_Clinical[match(rownames(NS_her2er),Metabric_Clinical$METABRIC_ID),c("er_status","HER2_SNP6_state")])

NS_her2er$HER2 <- ifelse(NS_her2er$HER2_SNP6_state=="GAIN", "pos", "neg")

ggplot(NS_her2er) + 
  aes(x=log(ESR1+0.5),y=log(GRB7+0.5),colour=interaction(er_status,HER2)) + 
  geom_point()+
  geom_rug(aes(colour=er_status), sides="b")+
  geom_rug(aes(colour=HER2), sides="l")+
  theme_bw(16)



