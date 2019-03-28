#load packages
library(rpart)

#load data
load("nanoStringIC_DF.Rdata")

#create a list
#list_x.b.predict <- list()

#make a null matrix
total_matrix <- matrix(0, nrow = 489, ncol = 10)

#iterate this 100 times or more
for (i in 1:200){
  x.b <- sample(1:nrow(nanoStringIC_DF), nrow(nanoStringIC_DF), replace = T)
  
  m.tree <- rpart(iClust ~ ., data=nanoStringIC_DF)
  x.b.predict <- as.matrix(predict(m.tree, newdata = nanoStringIC_DF))
  total_matrix <- (x.b.predict+total_matrix*(i-1))/i
  #list_x.b.predict[[i]] <- x.b.predict
  #now add the table and work out average
  print(i)
}

#total_matrix
#rowSums(total_matrix)

save(total_matrix, file = "total_matrix.RData")

