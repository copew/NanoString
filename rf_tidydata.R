#loading data
load("nsdata_tidy.Rdata")


#load packages
library(randomForest)


nsdata_tidy <- data.frame(nsdata_tidy)
nsdata_tidy$iClust <- as.factor(nsdata_tidy$iClust)

rf_cv_list <- lapply(1:nrow(nsdata_tidy), function(i) {
  tmp <- nsdata_tidy[-i,]
  tmp.rf <- randomForest(iClust~., data=tmp)
  tmp.pred <- predict(tmp.rf, nsdata_tidy[i,], type = "response", predict.all=TRUE)
  return(data.frame(rownames(nsdata_tidy)[i], tmp.pred$aggregate))
})

rf_cv_list[[1]]

rf_cv_tidydata <- do.call(rbind.data.frame, rf_cv_list)
colnames(rf_cv_tidydata) <- c("ID", "iClust")

#checking that the rownames are matching
#sum(rownames(nsdata_tidy)==rownames(rf_cv_tidydata))

rf_table_tidydata <- table(nsdata_tidy$iClust, rf_cv_tidydata$iClust)

sum(diag(rf_table_tidydata))/sum(rf_table_tidydata)

rf_tidydata_per_cluster <- diag(rf_table_tidydata)/margin.table(rf_table_tidydata, 1)

save(rf_tidydata_per_cluster, file="percluster.RData")
save(rf_table_tidydata, file = "rf_table.RData")
