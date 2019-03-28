
library(rpart)
load("combined_df_log.RData")

cv <- matrix(NA, nrow(combined_df_log),10)
for (i in 1:nrow(combined_df_log)){
  m.tree <- rpart(iClust ~ ., data=combined_df_log[-i,])
  
  cv[i, ] <- predict(m.tree, newdata = combined_df_log[i,])
  print(i)
}

table_cv <- table(apply(cv, 1, which.max), combined_df_log[,1])
accuracy <- sum(diag(table_cv))/sum(table_cv)

write.csv2(table_cv, file="classification_immune.csv")