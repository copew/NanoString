install.packages("caret")
install.packages("lattice")
install.packages("ddalpha")

##Random Forest



#load packages
library(rpart)

#load data
load("/Users/cope01/Documents/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/R scripts/Features.RData")

#Trees!
m.tree <- rpart(iClust ~ ., data=nanoStringIC_DF)
plot(m.tree)
text(m.tree, use.n=T, cex=0.5)
summary(m.tree)

#classification
classify <- as.data.frame(predict(m.tree))
classify$iClust <- colnames(classify)[apply(classify, 1, which.max)]

iClust_new <- as.data.frame(cbind(rownames(classify), classify$iClust))
colnames(iClust_new) <- c("ID", "iClust")


#check against previous iClust
#merge both intclust
iClust_new <- merge(iClust_new, iClust, by="ID")
iClust_new$IntClust <- as.factor(iClust_new$IntClust)

str(iClust_new)

####Tables
table(iClust_new$iClust, iClust_new$IntClust)
iClust_new$iClust <- factor(iClust_new$iClust, levels=1:10)
table(iClust_new$iClust, iClust_new$IntClust)
round(prop.table(table(iClust_new$iClust, iClust_new$IntClust), 1), 2)
round(prop.table(table(iClust_new$iClust, iClust_new$IntClust), 2), 2)
table(iClust_new$iClust, iClust_new$IntClust)


##to check it adds up to 1
#sum(predict(m.tree)[1,])




###Cross Validation



# load the library
install.packages("e1071")
library(e1071)
library(lattice)
library(ggplot2)
library(caret)


na.omit(nanoStringIC_DF)
# define training control
train_control <- trainControl(method="LOOCV")
# train the model
model <- train(iClust~., data=nanoStringIC_DF[!is.na(nanoStringIC_DF$iClust), ], trControl=train_control, method="rf")


# summarize results
print(model)


