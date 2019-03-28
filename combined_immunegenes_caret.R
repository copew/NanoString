load("combined_df_log.RData")
library(caret)
library(doMC)
library(randomForest)
#multicore

#registerDoMC(cores = 100)

#adding X in iClust
combined_df_log$iClust <- paste0("X", combined_df_log$iClust)

ctrl <- trainControl(method = "loocv", savePred=T, classProb=T)
mod <- train(iClust~., data=combined_df_log[ ,-1], method = "rf", trControl = ctrl)

saveRDS(mod, file="rf_mod.RData")


mod
