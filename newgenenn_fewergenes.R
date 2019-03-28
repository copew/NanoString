library(neuralnet)
library(nnet)
library(parallel)

load("mb_selected.RData")

smalldata <- mb_selected

df <- cbind(smalldata[, 2:488], class.ind(smalldata$iClust))
names(df) <- c(names(smalldata[2:488]), "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10")
scale <- function(x){(x-min(x))/(max(x)-min(x))}
df[, 1:487] <- as.data.frame((lapply(df[, 1:487], scale)))
genes <- names(df[1:487])
class <- names(df[488:497])
formula <- as.formula(paste(paste(class, collapse = "+"), "~", paste(genes, collapse = " + ")))

##cross validation
accuracy.list <- list()
predicted <- matrix(0, nrow = 100, ncol = 10)

cl <- makeCluster(1,type="FORK")

pr.nn.list <- parLapply(cl,1:nrow(smalldata), function(i) {
  trainNN <- df[-i,]
  testNN <- df[i,]
  NN = neuralnet(formula, trainNN, hidden = 50, act.fct = "logistic", linear.output = F, lifesign = "minimal")
  return(compute(NN, testNN[1:487]))
})



predicted <- Reduce(rbind,lapply(pr.nn.list,function(x) data.frame(x$net.result)),data.frame())

predicted.table <- table(factor(apply(predicted, 1, which.max), levels=1:10),smalldata$iClust) 
save(predicted.table, file="predicted.table.csv")
saveRDS(pr.nn.list,"NN.list.RDS")
