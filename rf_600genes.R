#use limma on nanostring data

library(limma)
load("nsdata_total.RData")

intclust <- data.frame(nsdata_total[,1], row.names = rownames(nsdata_total))
colnames(intclust) <- "iClust"
iclust_list <- list()
for (i in 1:10) {
  tmp <- intclust
  tmp$iClust <- as.character(tmp$iClust)
  tmp$iClust[which(tmp$iClust != i)] = "others"
  tmp$iClust <- factor(tmp$iClust)
  iclust_list[[i]] <- tmp
}  

#transpose data

data <- nsdata_total[, 2:ncol(nsdata_total)]
data <- as.matrix(t(data))

#selecting top genes
toptables <- list()
for (i in 1:10){
  design <- model.matrix(~ iclust_list[[i]]$iClust)
  fit <- lmFit(data, design)
  fit <- eBayes(fit)
  test <- topTable(fit, number=50)
  toptables[[i]] <- test
}

for (i in 1:2){
  design <- model.matrix(~ iclust_list[[i]]$iClust)
  fit <- lmFit(data, design)
  fit <- eBayes(fit)
  test <- topTable(fit, number=100)
  toptables[[i]] <- test
}


for (i in 1:10){
  print(paste0("iClust", i, ":"))
  print(rownames(toptables[[i]]))
  
}

genes_600 <- do.call(rbind.data.frame, toptables)
genes_600$iClust <- c(rep(1:2, each=100), rep(3:10, each=50))
genes_600$genes <- rownames(genes_600)

genes_selected <- genes_600[genes_600$P.Value<0.05,]


#subset data based on extra genes for cluster 1 and 2 
nsdata_total <- data.frame(nsdata_total)
ns_selectedgenes <- nsdata_total[, c(1, which(colnames(nsdata_total) %in% rownames(genes_600)))]
ns_selectedgenes$iClust <- as.factor(ns_selectedgenes$iClust)

save(ns_selectedgenes, file="ns_selectedgenes.RData")
#try random forest again, with lots and lots of trees

rf_cv_list <- lapply(1:nrow(ns_selectedgenes), function(i) {
  set.seed(100)
  tmp <- ns_selectedgenes[-i,]
  tmp.rf <- randomForest(iClust~., data=tmp, ntree=2000)
  tmp.pred <- predict(tmp.rf, ns_selectedgenes[i,], type = "response", predict.all=TRUE)
  return(data.frame(rownames(ns_selectedgenes)[i], tmp.pred$aggregate))
})

rf_cv_list[[1]]
rf_cv_600 <- do.call(rbind.data.frame, rf_cv_list)
colnames(rf_cv_600) <- c("ID", "iClust")

#checking that the rownames are matching
sum(rownames(rf_cv_600)==rownames(ns_selectedgenes))

rf_table_600 <- table(rf_cv_600$iClust, ns_selectedgenes$iClust)
diag(rf_table_600)/margin.table(rf_table_600, 2)


#ok still rubbish result.... even with 2000 trees..... 