#get package
library(randomForest)

#Get Data
load("nanoStringIC_DF.Rdata")
nanoStringIC_DF <- na.omit(nanoStringIC_DF)

nanostring_rf <- randomForest(iClust~., data=nanoStringIC_DF, ntree=1500, replace=TRUE, nodesize=1)
print(nanostring_rf)

result_rf <- as.data.frame(nanostring_rf$predicted)
colnames(result_rf) <- "predicted"
sum(rownames(result_rf)==rownames(nanoStringIC_DF))
result <- cbind(result_rf, nanoStringIC_DF$iClust)
colnames(result)[2] <- "iClust"

write.csv(result, file = "random_forest_predicted_result.csv")
