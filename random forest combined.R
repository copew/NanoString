#get package
library(randomForest)

#Get Data
load("combined_df_log.RData")




nanostring_rf <- randomForest(iClust~., data=combined_df_log[, -1], ntree=100000)
print(nanostring_rf)
 