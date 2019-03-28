
#loading libraries
library(ggplot2)
library(lattice)
library(caret)
library(foreach)
library(iterators)
#library(doMC)
library(parallel)
library(rpart)
library(e1071)



#loading data
setwd("/Users/cope01/Documents/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/R scripts")
load("nanoStringIC_DF.RData")

#remove na
nanoStringIC_DF <- na.omit(nanoStringIC_DF)
#nanoStringIC_DF_log <- apply(nanoStringIC_DF_log, 2, scale)

nanoStringIC_DF_log <- log(nanoStringIC_DF[, -1]+0.0005)

rownames(nanoStringIC_DF_log) <- rownames(nanoStringIC_DF)
nanoStringIC_DF_log <- as.data.frame(nanoStringIC_DF_log)
nanoStringIC_DF_log <- data.frame(iClust=nanoStringIC_DF$iClust, nanoStringIC_DF_log)


good_subset <- nanoStringIC_DF_log[which(nanoStringIC_DF_log$iClust %in% c(2, 5, 6, 9, 10)), ]
bad_subset <- nanoStringIC_DF_log[which(nanoStringIC_DF_log$iClust %in% c(1, 3, 4, 7, 8)), ]
bad_subset$iClust<-factor(bad_subset$iClust,levels=c(1, 3, 4, 7, 8))


svm.firstgo_bs <- svm(iClust ~ ., data=bad_subset)
svm.predict_bs <- predict(svm.firstgo_bs, bad_subset[,-1])


table_svm <- table(svm.predict_bs, bad_subset[,1])
table_svm

cv <- matrix(NA, nrow(bad_subset),1)
for (i in 1:nrow(bad_subset)){
  svm.firstgo_bs <- svm(iClust ~ ., data=bad_subset[-i,])
  cv[i] <- predict(svm.firstgo_bs, bad_subset[i,])
  print(i)
}


#chose the cluster that it has highest probability
table_cv_bs <- table(cv, bad_subset[,1])
accuracy <- sum(diag(table_cv_bs))/sum(table_cv_bs)
print(paste0("Accuracy: ", accuracy))
#summary(m.tree)
table_cv_bs










cv <- matrix(NA, nrow(nanoStringIC_DF_log),10)
for (i in 1:nrow(nanoStringIC_DF_log)){
  svm.firstgo <- svm(iClust ~ ., data=nanoStringIC_DF_log[-i,])
  cv[i,] <- predict(svm.firstgo, nanoStringIC_DF_log[i,])
  print(i)
}



#chose the cluster that it has highest probability
table_cv_gs <- table(cv[,1], good_subset[,1])
accuracy <- sum(diag(table_cv_gs))/sum(table_cv_gs)
print(paste0("Accuracy: ", accuracy))
#summary(m.tree)
table_cv_gs

