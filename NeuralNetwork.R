#load library
#install.packages("neuralnet")
#install.packages("nnet")
library(neuralnet)
library(nnet)

#load data
load("nanoStringIC_DF.RData")
nanoStringIC_DF <- na.omit(nanoStringIC_DF)
#nanoStringIC_DF$iClust<-paste0("x",nanoStringIC_DF$iClust)
#nanoStringIC_DF$iClust <- as.numeric(nanoStringIC_DF$iClust)
#Prepare data
df <- cbind(nanoStringIC_DF[, 2:213], class.ind(nanoStringIC_DF$iClust))
names(df) <- c(names(nanoStringIC_DF[2:213]), "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10")

##cross validation
pr.nn.list <- list()
accuracy.list <- list()
predicted <- matrix(0, nrow = 480, ncol = 10)

for (i in 1:nrow(nanoStringIC_DF)) {
trainNN <- df[-i,]
testNN <- df[i,]

scale <- function(x){(x-min(x))/(max(x)-min(x))}
df[, 1:213] <- as.data.frame((lapply(df[, 1:213], scale)))



#fit neural network
genes <- names(df[1:212])
class <- names(df[213:222])
formula <- as.formula(paste(paste(class, collapse = "+"), "~", paste(genes, collapse = " + ")))

NN = neuralnet(formula, trainNN, hidden = c(106, 23), act.fct = "logistic", linear.output = F, lifesign = "minimal")

pr.nn.list[[i]] <- compute(NN,testNN[1:212])
predicted[i,] <- pr.nn.list[[i]]$net.result

print(i)
}


#for (j in 1:480){
#  predicted[j,] <- pr.nn.list[[j]]$net.result
#}

predicted <- as.data.frame(predicted)

predicted.table <- table(factor(apply(predicted, 1, which.max), levels=1:10),testNN$iClust) 
predicted.table
sum(diag(predicted.table))/sum(predicted.table)
