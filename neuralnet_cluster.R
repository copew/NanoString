#load library
install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)
library(nnet)
library(parallel)


#load data
load("nanoStringIC_DF.RData")
nanoStringIC_DF <- na.omit(nanoStringIC_DF)
#nanoStringIC_DF$iClust<-paste0("x",nanoStringIC_DF$iClust)
#nanoStringIC_DF$iClust <- as.numeric(nanoStringIC_DF$iClust)
#Prepare data
df <- cbind(nanoStringIC_DF[, 2:213], class.ind(nanoStringIC_DF$iClust))
names(df) <- c(names(nanoStringIC_DF[2:213]), "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10")
scale <- function(x){(x-min(x))/(max(x)-min(x))}
df[, 1:213] <- as.data.frame((lapply(df[, 1:213], scale)))
genes <- names(df[1:212])
class <- names(df[213:222])
formula <- as.formula(paste(paste(class, collapse = "+"), "~", paste(genes, collapse = " + ")))

##cross validation
accuracy.list <- list()
predicted <- matrix(0, nrow = 480, ncol = 10)

cl <- makeCluster(20,type="FORK")

pr.nn.list <- parLapply(cl,1:nrow(nanoStringIC_DF), function(i) {
  trainNN <- df[-i,]
  testNN <- df[i,]
  NN = neuralnet(formula, trainNN, hidden = 240, act.fct = "logistic", linear.output = F, lifesign = "minimal")
  return(compute(NN, testNN[1:212]))
  })

predicted <- Reduce(rbind,lapply(pr.nn.list,function(x) data.frame(x$net.result)),data.frame())

predicted.table <- table(factor(apply(predicted, 1, which.max), levels=1:10),nanoStringIC_DF$iClust) 
save(predicted.table, file="predicted.table.csv")
saveRDS(pr.nn.list,"NN.list.RDS")
