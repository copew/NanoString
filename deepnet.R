library(lattice)
library(ggplot2)
library(caret)
library(doMC)
library(foreach)
library(iterators)
library(parallel)
library(neuralnet)
library(nnet)
library(deepnet)

load("mb_selected.RData")


tgrid <- expand.grid(layer1 = 240,
                     layer2 = 40, layer3 = 0,
                     hidden_dropout = c(0, .1, .2), 
                     visible_dropout = 0)
ctrl <- trainControl(method = "repeatedcv", savePred=T, classProb=T, repeats= 1)
moddeepnet<- train(iClust~., data=mb_selected, method = "dnn", trControl = ctrl, tuneGrid = tgrid)



saveRDS(moddeepnet, file="moddeepnet.RDS")

