##put into clusters

install.packages("iC10")
library(iC10)


##put it into classifiers

#look for the genes
features <- matchFeatures(Exp=nanoStringIC, Exp.by.feat = "gene")
features <- normalizeFeatures(features, "scale")
res <- iC10(features)


summary(res)
goodnessOfFit(res)$total
compare(res, features, iC10=1:5, lwd=1)
compare(res, features, iC10=6:10, lwd=1)
###comparing classification with the ones from illumina

mean(sub(".", "-", clust_corr$MB.ID, fixed=T) == names(res$class))

table(res$class, clust_corr$IntClust)
sum(diag(table(res$class, clust_corr$IntClust)))

data(Map.All)

#conditional probability of correct classification
##res$class is what i have, clust_corr is the gold standard
##2 means conditioning on the columns
##second 2 means 2 decimals.....

round(prop.table(table(res$class, clust_corr$IntClust), 2), 2) 