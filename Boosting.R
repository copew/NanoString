#load packages
library(rpart)

#load data
load("nanoStringIC_DF.Rdata")
nanoStringIC_DF <- na.omit(nanoStringIC_DF)
#create a list
list_x.b.predict_DF <- list()
list_table <- list()
list_tree <- list()
#make a null matrix
total_matrix <- matrix(0, nrow = 480, ncol = 10)

#define weight
w=rep(1/nrow(nanoStringIC_DF), nrow(nanoStringIC_DF))

alpha <- NULL

#iterate this 100 times or more
for (i in 1:50){
  #x.b <- sample(1:nrow(nanoStringIC_DF), nrow(nanoStringIC_DF), replace = T)
  m.tree <- rpart(iClust ~ ., weights= w, data=nanoStringIC_DF)
  list_tree[[i]] <- m.tree
  
  x.b.predict <- as.matrix(predict(m.tree, newdata = nanoStringIC_DF))
  #total_matrix <- (x.b.predict+total_matrix*(i-1))/i
 
  #now add the table and work out average
  x.b.predict_DF <- as.data.frame(cbind(x.b.predict, factor(apply(x.b.predict, 1, which.max), levels=1:10), nanoStringIC_DF$iClust))
  x.b.predict_DF[13] <- ifelse(x.b.predict_DF[,11]==x.b.predict_DF[,12], 0, 1)
  
  error <- (sum(w * x.b.predict_DF[,13]))/(sum(w))
  alpha <- c(alpha,log((1 - error)/error))
  
  w <- as.numeric(unlist(w * exp(alpha * x.b.predict_DF[13])))
  table_x.b.predict <- table(apply(total_matrix, 1, which.max), x.b.predict_DF[,12])
  #keep the prediction matrices each time......
  list_x.b.predict_DF[[i]] <- x.b.predict_DF
  list_table[[i]] <- table_x.b.predict
  total_matrix <- (x.b.predict+total_matrix*sum(alpha[-i]))/sum(alpha)
  
  print(i)
}

table_x.b.predict<- as.data.frame.matrix(table_x.b.predict)
View(table_x.b.predict)

View(as.data.frame.matrix(table_x.b.predict))
#total_matrix
#rowSums(total_matrix)

#save(total_matrix, file = "total_matrix.RData")
table(apply(total_matrix, 1, which.max), x.b.predict_DF[ ,12])

plot(alpha, type="l")
