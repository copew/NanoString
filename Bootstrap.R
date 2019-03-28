#load packages
library(rpart)


#load data
load("nanoStringIC_DF.Rdata")

#select a row number from total sample
# x.b <- sample(1:nrow(nanoStringIC_DF), nrow(nanoStringIC_DF), replace = T)
# 
# m.tree <- rpart(iClust ~ ., data=nanoStringIC_DF[x.b,])
# plot(m.tree)
# text(m.tree, use.n=T, cex=0.5)
# x.b.predict <- predict(m.tree, newdata = nanoStringIC_DF[-x.b,])

#create a list
list_x.b.predict <- list()

#iterate this 100 times or more
for (i in 1:10){
  x.b <- sample(1:nrow(nanoStringIC_DF), nrow(nanoStringIC_DF), replace = T)

  m.tree <- rpart(iClust ~ ., data=nanoStringIC_DF[x.b,])
  x.b.predict <- predict(m.tree, newdata = nanoStringIC_DF[-x.b,])
   table_x.b.predict <- table(factor(apply(x.b.predict, 1, which.max), levels=1:10), nanoStringIC_DF[-x.b, 1])

x.b.predict
# list_diagsum <- list()
# list_totalsum <- list()

  list_x.b.predict[[i]] <- table_x.b.predict
# list_diagsum[[i]] <- sum(diag(table_x.b.predict))
# list_totalsum[[i]] <- sum(table_x.b.predict)

  print(i)
}

weighted_mean <- weighted.mean(sapply(list_x.b.predict, function(x) sum(diag(x))/sum(x)), w=sapply(list_x.b.predict, sum))
weighted_mean


list_x.b.predict

View(table_x.b.predict)
