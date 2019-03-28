#discovery vs rest using RF, nanostring data

library(randomForest)
library(ggplot2)
library(reshape2)
set.seed(100)

#data is nanostring_discover
ns_discovery_rf <- randomForest(iClust~., data=nanostring_discovery)

#now get the test df
ns_test_df <- nanoStringIC_DF[-which(rownames(nanoStringIC_DF) %in% rownames(nanostring_discovery)),]


test_prediction <- table(ns_test_df$iClust, predict(ns_discovery_rf, ns_test_df[,-1], type="response"))
diag(test_prediction)/margin.table(test_prediction, 1)


test_prediction_original <- table(nanostring_discovery$iClust, predict(ns_discovery_rf, nanostring_discovery[,-1], type="response"))
diag(test_prediction_original)/margin.table(test_prediction_original, 1)



#so let's compare the expression of various genes in the test group against the ones in the discovery set, according to the "given" iClust
ns_test_zscore <- data.frame(cbind(ns_test_df$iClust, scale(ns_test_df[, 2:213])))
colnames(ns_test_zscore)[1] <- "iClust"
ns_test_centroid <- matrix(, nrow=10, ncol=212)
for (j in 2:ncol(ns_test_zscore)){
  for (i in 1:10) {
    ns_test_centroid[i, (j-1)] <- mean(ns_test_df[which(ns_test_df$iClust==i), j])
  }
  #print(j)
}

ns_test_df <- data.frame(ns_test_centroid, row.names = c("iC1","iC2","iC3","iC4","iC5","iC6","iC7","iC8","iC9","iC10"))
colnames(ns_test_df) <- colnames(ns_test_zscore)[-1]
ns_test_df <- data.frame(t(ns_test_df))
ns_test_df$genes <- rownames(ns_test_df)
ns_test_df_melted <- melt(ns_test_df, id.vars = "genes")

test_vs_discovery <- melt(iclust_centroid_melted, ns_test_df_melted, by="genes")


for (i in 1:10) {
    centroid_by_ic_list[[i]] <- iclust_centroid_melted[which(iclust_centroid_melted$variable==paste0("iC", i)),]
    print(i)
    print(ggplot(centroid_by_ic_list[[i]], aes(x=factor(genes, levels = c(centroid_by_ic_list[[i]]$genes[order(centroid_by_ic_list[[i]]$value, decreasing = TRUE)])), y=value, group=1))+
            geom_line(colour=coliClust10[i, 2], size=2)+
            theme_bw(base_size = 12)+
            geom_hline(yintercept=0, linetype="dotted")+
            theme(axis.text.x = element_text(angle = 90, size = 6))+
            xlab("Genes")+
            ggtitle(paste("iC", i))
    )
  }


