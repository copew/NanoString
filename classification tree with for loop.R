#load packages
library(rpart)

#load data
load("/Users/cope01/Documents/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/R scripts/Features.RData")

#Trees!
cv <- matrix(NA, nrow(nanoStringIC_DF),10)
for (i in 1:nrow(nanoStringIC_DF)){
  m.tree <- rpart(iClust ~ ., data=nanoStringIC_DF[-i,])
plot(m.tree)
text(m.tree, use.n=T, cex=0.5)
cv[i, ] <- predict(m.tree, newdata = nanoStringIC_DF[i,])
print(i)
}

apply(cv, 1, which.max)

table_cv <- table(apply(cv, 1, which.max), nanoStringIC_DF[,1])

diag(table_cv)

sum(diag(table_cv))/sum(table_cv)


  
#summary(m.tree)

#classification
classify <- as.data.frame(predict(m.tree))
classify$iClust <- colnames(classify)[apply(classify, 1, which.max)]
