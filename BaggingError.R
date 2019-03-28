#library
library(ggplot2)

#loading data
load("total_matrix.RData")

#apparent error
table_total <- table(factor(apply(total_matrix, 1, which.max),levels=1:10), nanoStringIC_DF[,1])
sum(diag(table_total))/sum(table_total)


#checking total_matrix and nanoStringIC_DF have the same rownames
identical(rownames(total_matrix),  rownames(nanoStringIC_DF))

##If you assign each sample to the cluster with the maximum probability, 
#is there any relationship between how big is that probability and getting the cluster right? 
#In other words, do samples classified with higher confidence more likely to be correctly classified?

total_matrix <- cbind(total_matrix,apply(total_matrix,1,max), apply(total_matrix,1,which.max), nanoStringIC_DF[,1])
colnames(total_matrix)[11:13]<- c("MaxProbability","PredictedIC","KnownIC")
total_matrix_DF <- as.data.frame(total_matrix)
total_matrix_DF$Match <- ifelse(total_matrix_DF[,12]==total_matrix_DF[,13], "Correct", "Incorrect")

wilcox.test(MaxProbability ~ Match, data=total_matrix_DF) 

ggplot(total_matrix_DF, aes(Match,MaxProbability))+
  geom_boxplot(width=0.3)+
  geom_jitter(aes(colour=Match), width=0.25)+
  theme_bw()



# correct_IC <- total_matrix_DF[which(total_matrix_DF$Match=="Correct"), "MaxProbability", drop=FALSE]
# incorrect_IC <- total_matrix_DF[which(total_matrix_DF$Match=="Incorrect"), "MaxProbability", drop=FALSE]




#how the cross validation of bagging


