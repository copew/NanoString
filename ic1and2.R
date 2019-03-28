# compare iC1 and iC2 in terms of gata3 expression

colnames(nanoStringIC_DF)


ic1and2 <- nanoStringIC_DF[c(which(nanoStringIC_DF$iClust==1), which(nanoStringIC_DF$iClust==2)), ]


ggplot(nanoStringIC_DF,aes(x=iClust, y=GATA3))+
  geom_boxplot()
wilcox.test(log(GATA3) ~ iClust, data=ic1and2)


#ic1and2t <- data.frame(t(ic1and2))


#ic1and2t$sigdif <- apply(ic1and2t, 1, function(x) t.test(ic1and2t[x, ], )

sigdif <- apply(ic1and2[,-1], 2, function(x) wilcox.test(x[which(ic1and2$iClust==1)], x[which(ic1and2$iClust==2)]))
which(lapply(sigdif,function(x)x$p.value)<0.05)
sigdif$RSF1
sigdif$PAK1
ic1and2$iClust <- factor(ic1and2$iClust)
ic1and2.rf <- randomForest(iClust~., data=ic1and2)
print(ic1and2.rf)
