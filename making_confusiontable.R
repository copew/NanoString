pr.nn.list <- readRDS("/Users/cope01/Documents/cluster/NN.list.RDS")


predicted <- Reduce(rbind,lapply(pr.nn.list,function(x) data.frame(x$net.result)),data.frame())
predicted <- predicted[order(rownames(predicted)), ]
predicted <- predicted[(which(rownames(predicted) %in% rownames(mb_selected))), ]

predicted


nanostringIC_DF <- nanoStringIC_DF[(which(rownames(nanoStringIC_DF) %in% rownames(mb_selected))), ]

nanostringIC_DF <- nanostringIC_DF[order(rownames(nanostringIC_DF)), ]
sum(rownames(nanostringIC_DF)==rownames(predicted))

predicted.table <- table(factor(apply(predicted, 1, which.max), levels=1:10),nanostringIC_DF$iClust)
predicted.table
sum(diag(predicted.table))/sum(predicted.table)


head.mini(nanostringIC_DF)

nanostringIC_DF[1:10, 1]



dim(predicted)
head.mini(mb_selected)
head.mini(predicted)


test <- factor(apply(predicted[1:10,], 1, which.max))
test2 <- (nanostringIC_DF[1:10, 1])
test
test2
