#load packages
library(rpart)

#load data
load("nanoStringIC_DF.Rdata")
nanoStringIC_DF <- na.omit(nanoStringIC_DF)


#create lists
list_x.b.predict_DF <- list()
list_table <- list()
list_tree <- list()

list_result <- list()



#leave one out loop

for (j in 1:nrow(nanoStringIC_DF)){
  tempdf <- nanoStringIC_DF[-j, ]
  #make a null matrix
  #total_matrix <- matrix(0, nrow = 480, ncol = 10)
  
  #define weight
  w=rep(1/(nrow(nanoStringIC_DF) - 1), nrow(nanoStringIC_DF)-1)

  alpha <- NULL
  #iterate this 100 times or more
  for (i in 1:100){
    m.tree <- rpart(iClust ~ ., weights= w, data=tempdf)
    x.b.predict <- as.matrix(predict(m.tree, newdata = tempdf))

    #now add the table and work out average
    x.b.predict_DF <- as.data.frame(cbind(x.b.predict, factor(apply(x.b.predict, 1, which.max), levels=1:10), tempdf[,1]))
    x.b.predict_DF[13] <- ifelse(x.b.predict_DF[,11]==x.b.predict_DF[,12], 0, 1)
  
    error <- (sum(w * x.b.predict_DF[,13]))/(sum(w))
    alpha <- c(alpha,log((1 - error)/error))
    
    w <- as.numeric(unlist(w * exp(alpha * x.b.predict_DF[13])))
    #table_x.b.predict <- table(apply(total_matrix, 1, which.max), x.b.predict_DF[,12])
    
    #keep the prediction matrices each time......
    list_x.b.predict_DF[[i]] <- x.b.predict_DF
    #total_matrix <- (x.b.predict+total_matrix*sum(alpha[-i]))/sum(alpha)
    list_tree[[i]] <- m.tree
    
  print(i)
  }
  list_result[[j]]<- as.data.frame(t(sapply(list_tree, function(x) predict(x, newdata=nanoStringIC_DF[j,])) %*% alpha / sum(alpha)))
  #rownames(result[j,]) <- rownames(nanoStringIC_DF[j,])
  print(j)
}

result <- do.call(rbind, list_result)
rownames(result) <- rownames(nanoStringIC_DF)
colnames(result) <- c(1:10)

result$PredictedIC <- factor(apply(result, 1, which.max))
result$originalIC <- nanoStringIC_DF$iClust

result_table <- table(result$PredictedIC, result$originalIC)
result_table


sum(diag(result_table))/sum(result_table)

sum(result_table)
# table_x.b.predict<- as.data.frame.matrix(table_x.b.predict)
# View(table_x.b.predict)
# 
# View(as.data.frame.matrix(table_x.b.predict))
# #total_matrix
#rowSums(total_matrix)

#save(total_matrix, file = "total_matrix.RData")
#table(apply(total_matrix, 1, which.max), x.b.predict_DF[ ,12])

#plot(alpha, type="l")
