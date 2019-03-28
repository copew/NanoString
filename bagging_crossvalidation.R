#load packages
library(rpart)

#load data
load("nanoStringIC_DF.Rdata")

#create a list
#list_x.b.predict <- list()

#make a null matrix
total_matrix <- matrix(0, nrow = 489, ncol = 10)

#iterate this 100 times or more

###need tweak this still as this is not quite correct - use tmp instead!!!
for (j in 1:nrow(nanoStringIC_DF)){
  tmp.nanoStringIC_DF <-nanoStringIC_DF[-j,] 
  one_matrix <- matrix(0, ncol = 10)
  for (i in 1:100){
  x.b <- sample(1:(nrow(nanoStringIC_DF)- j), nrow(nanoStringIC_DF)-1, replace = T)
  m.tree <- rpart(iClust ~ ., data=nanoStringIC_DF[x.b,])
  x.b.predict <- as.matrix(predict(m.tree, newdata = nanoStringIC_DF[j,]))
  one_matrix <- (x.b.predict+one_matrix*(i-1))/i
  #list_x.b.predict[[i]] <- x.b.predict
  #now add the table and work out average
  print(i)
  }
  total_matrix [j, ]<- one_matrix
  print(j)
}




total_matrix
#rowSums(total_matrix)

save(total_matrix, file = "total_matrix.RData")

