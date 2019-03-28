#is it possible to predict group 1 vs others accurately at all?  
library(randomForest)

load("nsdata_total.RData")
load("ns_selectedgenes.RData")

intclust <- data.frame(nsdata_total[,1], row.names = rownames(nsdata_total))
colnames(intclust) <- "iClust"

#make a df, ic1, that has iClust is either 1 or others
tmp <- intclust
tmp$iClust <- as.character(tmp$iClust)
tmp$iClust[which(tmp$iClust != 1)] = "others"
tmp$iClust <- factor(tmp$iClust)
ic1 <- tmp

#then use this for the data
nsdata_ic1 <- data.frame(cbind(ic1, ns_selectedgenes[,-1]))
nsdata_ic1_fewergenes <- nsdata_ic1[, c(1, which(colnames(nsdata_ic1) %in% rownames(genes_600[genes_600$iClust==1, ])))]
  
#can we accurately predict iC1 vs others
rf_cv_list <- lapply(1:nrow(nsdata_ic1_fewergenes), function(i) {
  set.seed(100)
  tmp <- nsdata_ic1_fewergenes[-i,]
  tmp.rf <- randomForest(iClust~., data=tmp, ntree=5000)
  tmp.pred <- predict(tmp.rf, nsdata_ic1_fewergenes[i,], type = "response", predict.all=TRUE)
  return(data.frame(rownames(nsdata_ic1_fewergenes)[i], tmp.pred$aggregate))
})

rf_cv_ic1 <- do.call(rbind.data.frame, rf_cv_list)
colnames(rf_cv_ic1)[2] <- "iClust"
rf_table_ic1 <- table(rf_cv_ic1$iClust, nsdata_ic1$iClust)



#make a df, ic2, that has iClust is either 2 or others
tmp <- intclust
tmp$iClust <- as.character(tmp$iClust)
tmp$iClust[which(tmp$iClust != 2)] = "others"
tmp$iClust <- factor(tmp$iClust)
ic2 <- tmp

#then use this for the data
nsdata_ic2 <- data.frame(cbind(ic2, ns_selectedgenes[,-1]))
nsdata_ic2_fewergenes <- nsdata_ic2[, c(1, which(colnames(nsdata_ic2) %in% rownames(genes_600[genes_600$iClust==2, ])))]

#can we accurately predict iC2 vs others
rf_cv_list_ic2 <- lapply(1:nrow(nsdata_ic2_fewergenes), function(i) {
  set.seed(100)
  tmp <- nsdata_ic2_fewergenes[-i,]
  tmp.rf <- randomForest(iClust~., data=tmp, ntree=5000)
  tmp.pred <- predict(tmp.rf, nsdata_ic2_fewergenes[i,], type = "response", predict.all=TRUE, nodesize=5)
  return(data.frame(rownames(nsdata_ic2_fewergenes)[i], tmp.pred$aggregate))
})

rf_cv_ic2 <- do.call(rbind.data.frame, rf_cv_list_ic2)
colnames(rf_cv_ic2)[2] <- "iClust"
rf_table_ic2 <- table(rf_cv_ic2$iClust, nsdata_ic2$iClust)

save(rf_table_ic1, file="rf_table_ic1.RData")
save(rf_table_ic2, file="rf_table_ic2.RData")
