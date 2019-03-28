library(neuralnet)
library(nnet)
library(parallel)

load("mb_selected.RData")

df <- cbind(mb_selected[, 2:488], class.ind(mb_selected$iClust))
names(df) <- c(names(mb_selected[2:488]), "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10")
scale <- function(x){(x-min(x))/(max(x)-min(x))}
df[, 1:487] <- as.data.frame((lapply(df[, 1:487], scale)))
genes <- names(df[1:487])
class <- names(df[488:497])
formula <- as.formula(paste(paste(class, collapse = "+"), "~", paste(genes, collapse = " + ")))

##cross validation
accuracy.list <- list()
predicted <- matrix(0, nrow = 458, ncol = 10)

cl <- makeCluster(2,type="FORK")

pr.nn.list <- parLapply(cl,1:nrow(mb_selected), function(i) {
  trainNN <- df[-i,]
  testNN <- df[i,]
  NN = neuralnet(formula, trainNN, hidden = 240, act.fct = "logistic", linear.output = F, lifesign = "minimal")
  return(compute(NN, testNN[1:457]))
})

predicted <- Reduce(rbind,lapply(pr.nn.list,function(x) data.frame(x$net.result)),data.frame())

predicted.table <- table(factor(apply(predicted, 1, which.max), levels=1:10),mb_selected$iClust) 
save(predicted.table, file="predicted.table.csv")
saveRDS(pr.nn.list,"NN.list.RDS")