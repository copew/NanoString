#using caret
install.packages("doMC")
library(ggplot2)
library(lattice)
library(caret)
library(doMC)
library(foreach)
library(iterators)
library(parallel)

registerDoMC(cores = 5)
## All subsequent models are then run in parallel


#load data
load("nanoStringIC_DF.Rdata")
str(nanoStringIC_DF)
nanoStringIC_DF$iClust<-paste0("x",nanoStringIC_DF$iClust)
nanoStringIC_DF <- na.omit(nanoStringIC_DF)
rownames(nanoStringIC_DF)<-gsub("-","",rownames(nanoStringIC_DF))

?make.names

#training
ctrl <- trainControl(method = "repeatedcv", savePred=T, classProb=T, repeats= 500)
mod <- train(iClust~., data=nanoStringIC_DF, method = "rf", trControl = ctrl)

mod

?trainControl
