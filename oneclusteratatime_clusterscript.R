#this is to run 1 vs rest rf classification on the cluster

#loading packages
library(randomForest)

#loading data
load("mb_data_trimmed.RData")
load("mb_intclust.RData")

rf_list_ic1 <- lapply(1:nrow(mb_data_trimmed), function(i) {
  set.seed(100)
  tmp <- mb_data_trimmed[-i,]
  ic <- IntClust$iClust1[-i]
  tmp.rf <- randomForest(tmp, ic)
  tmp.pred <- predict(tmp.rf, mb_data_trimmed[i,], type = "response", predict.all=TRUE)
  return(data.frame(rownames(mb_data_trimmed)[i], tmp.pred$aggregate))
})

save(rf_list_ic1, file="rf_list_ic1.RData")