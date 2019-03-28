#useful libraries
library(ggplot2)
library(reshape2)

#loading knn data and extract value from various positions

#load knn files
knn_csvfolder <- "/Users/cope01/Documents/MATLAB/ImageAnalysis/knn_distance/"
knn_csv_list <- list.files(knn_csvfolder, pattern = ".csv", recursive = T, full.names = T)
knn_files <- lapply(knn_csv_list, function (i) read.csv(i, header=FALSE))

names(knn_files) <- knn_csv_list
names(knn_files) <- sub("/Users/cope01/Documents/MATLAB/ImageAnalysis/knn_distance/", "", names(knn_files))
names(knn_files) <- sub("_distance.csv", "", names(knn_files))
names(knn_files) <- sub("_knn", "", names(knn_files))

split_knn_names <- strsplit(names(knn_files), "_")

for (i in 1:length(knn_files)){
  knn_files[[i]]$ImageID <- split_knn_names[[i]][1]
  knn_files[[i]]$location <- split_knn_names[[i]][2]
}


#load lymph count files
result_folder <- "/Users/cope01/Documents/MATLAB/ImageAnalysis/results"
lymphcount_list <- list.files(result_folder, pattern = "lymph", recursive = T, full.names = T)
lymphcount_files <-  lapply(lymphcount_list, function (i) read.csv(i, header=FALSE))

names(lymphcount_files) <- lymphcount_list
names(lymphcount_files) <- sub("/Users/cope01/Documents/MATLAB/ImageAnalysis/results/", "", names(lymphcount_files))
names(lymphcount_files) <- sub("_count.csv", "", names(lymphcount_files))
names(lymphcount_files) <- sub("_lymph", "", names(lymphcount_files))


lengths_lymph <- lapply(lymphcount_files, length)
lengths_lymph <- do.call(rbind, lengths_lymph)
max(lengths_lymph) #6877

split_lymphcount_names <- strsplit(names(lymphcount_files), "_")

for (i in 1:length(lymphcount_files)){
  lymphcount_files[[i]] <- data.frame(t(as.matrix(lymphcount_files[[i]])))
  lymphcount_files[[i]]$ImageID <- split_lymphcount_names[[i]][1]
  lymphcount_files[[i]]$location <- split_lymphcount_names[[i]][2]
}

#making a result dataframe for lymph count


lymphcount_features <- data.frame(matrix(NA, nrow=332))

for (i in 1:length(lymphcount_files)){
  lymphcount_features$ImageID[i] <- lymphcount_files[[i]]$ImageID[1]
  lymphcount_features$location[i]<- lymphcount_files[[i]]$location[1]
}  
#remove empty
lymphcount_features <- lymphcount_features[,-1]

for (i in 1:length(lymphcount_files)){ 
  tmp_lymphcount <- lymphcount_files[[i]][ , 1]
  lymphcount_features$median[i] <- median(tmp_lymphcount, na.rm =TRUE )
  lymphcount_features$mean[i] <- mean(tmp_lymphcount, na.rm=TRUE)
  lymphcount_features$tenth[i] <- quantile(tmp_lymphcount, 0.1, na.rm =TRUE)[[1]]
  lymphcount_features$twentyfifth[i] <- quantile(tmp_lymphcount, 0.25, na.rm =TRUE)[[1]]
  lymphcount_features$seventyfifth[i] <- quantile(tmp_lymphcount, 0.75, na.rm =TRUE)[[1]]
  lymphcount_features$ninetieth[i] <- quantile(tmp_lymphcount, 0.9, na.rm =TRUE)[[1]]
}




#loading area files
area_csv_list <- list.files(result_folder, pattern = "_area_", recursive = T, full.names = T)
area_files <- lapply(area_csv_list, function (i) read.csv(i, header=FALSE))

names(area_files) <- area_csv_list
names(area_files) <- sub("/Users/cope01/Documents/MATLAB/ImageAnalysis/results/", "", names(area_files))
names(area_files) <- sub(".csv", "", names(area_files))
names(area_files) <- sub("_area", "", names(area_files))

split_area_names <- strsplit(names(area_files), "_")

lengths_area <- lapply(area_files, length)
lengths_area <- do.call(rbind, lengths_area)
max(lengths_area) #6877

for (i in 1:length(area_files)){
  area_files[[i]] <- data.frame(t(as.matrix(area_files[[i]])))
  area_files[[i]]$ImageID <- split_area_names[[i]][1]
  area_files[[i]]$location <- split_area_names[[i]][2]
}

area_features <- data.frame(matrix(NA, nrow=332))

for (i in 1:length(area_files)){
  area_features$ImageID[i] <- area_files[[i]]$ImageID[1]
  area_features$location[i]<- area_files[[i]]$location[1]
}  
#remove empty
area_features <- area_features[,-1]

for (i in 1:length(area_files)){ 
  tmp_area <- area_files[[i]][ , 1]
  area_features$median[i] <- median(tmp_area, na.rm =TRUE )
  area_features$mean[i] <- mean(tmp_area, na.rm=TRUE)
  area_features$tenth[i] <- quantile(tmp_area, 0.1, na.rm =TRUE)[[1]]
  area_features$twentyfifth[i] <- quantile(tmp_area, 0.25, na.rm =TRUE)[[1]]
  area_features$seventyfifth[i] <- quantile(tmp_area, 0.75, na.rm =TRUE)[[1]]
  area_features$ninetieth[i] <- quantile(tmp_area, 0.9, na.rm =TRUE)[[1]]
}





#loading tumour count files
tumourcount_list <- list.files(result_folder, pattern = "tumourcell", recursive = T, full.names = T)
tumourcount_files <-  lapply(tumourcount_list, function (i) read.csv(i, header=FALSE))
names(tumourcount_files) <- tumourcount_list
names(tumourcount_files) <- sub("/Users/cope01/Documents/MATLAB/ImageAnalysis/results/", "", names(tumourcount_files))
names(tumourcount_files) <- sub("_count.csv", "", names(tumourcount_files))
names(tumourcount_files) <- sub("_tumourcell", "", names(tumourcount_files))

split_tumourcount_names <- strsplit(names(tumourcount_files), "_")

lengths_tumourcount <- lapply(tumourcount_files, length)
lengths_tumourcount <- do.call(rbind, lengths_tumourcount)
max(lengths_tumourcount) #6877

for (i in 1:length(tumourcount_files)){
  tumourcount_files[[i]] <- data.frame(t(as.matrix(tumourcount_files[[i]])))
  tumourcount_files[[i]]$ImageID <- split_tumourcount_names[[i]][1]
  tumourcount_files[[i]]$location <- split_tumourcount_names[[i]][2]
}

tumourcount_features <- data.frame(matrix(NA, nrow=332))

for (i in 1:length(tumourcount_files)){
  tumourcount_features$ImageID[i] <- tumourcount_files[[i]]$ImageID[1]
  tumourcount_features$location[i]<- tumourcount_files[[i]]$location[1]
}  
#remove empty
tumourcount_features <- tumourcount_features[,-1]

for (i in 1:length(tumourcount_files)){ 
  tmp_tumour <- tumourcount_files[[i]][ , 1]
  tumourcount_features$median[i] <- median(tmp_tumour, na.rm =TRUE )
  tumourcount_features$mean[i] <- mean(tmp_tumour, na.rm=TRUE)
  tumourcount_features$tenth[i] <- quantile(tmp_tumour, 0.1, na.rm =TRUE)[[1]]
  tumourcount_features$twentyfifth[i] <- quantile(tmp_tumour, 0.25, na.rm =TRUE)[[1]]
  tumourcount_features$seventyfifth[i] <- quantile(tmp_tumour, 0.75, na.rm =TRUE)[[1]]
  tumourcount_features$ninetieth[i] <- quantile(tmp_tumour, 0.9, na.rm =TRUE)[[1]]
}



#make a result dataframe for knn
knn_features <- data.frame(matrix(NA, nrow=332))

for (i in 1:length(knn_files)){
  knn_features$ImageID[i] <- knn_files[[i]]$ImageID[1]
  knn_features$location[i]<- knn_files[[i]]$location[1]
}  
#remove empty
knn_features <- knn_features[,-1]

for (i in 1:length(knn_files)){ 
  tmp_knn <- knn_files[[i]][ , -c(ncol(knn_files[[i]]), ncol(knn_files[[i]])-1)]
  knn_features$median_5[i] <- median(t(tmp_knn[6,]), na.rm =TRUE )
  knn_features$median_10[i] <- median(t(tmp_knn[11,]), na.rm =TRUE )
  knn_features$median_15[i] <- median(t(tmp_knn[16,]), na.rm =TRUE )
  knn_features$median_25[i] <- median(t(tmp_knn[26,]), na.rm =TRUE )
  knn_features$median_50[i] <- median(t(tmp_knn[51,]), na.rm =TRUE )
  knn_features$tenth[i] <- quantile(t(tmp_knn), 0.1, na.rm =TRUE)[[1]]
  knn_features$twentyfifth[i] <- quantile(t(tmp_knn), 0.25, na.rm =TRUE)[[1]]
  knn_features$seventyfifth[i] <- quantile(t(tmp_knn), 0.75, na.rm =TRUE)[[1]]
  knn_features$ninetieth[i] <- quantile(t(tmp_knn), 0.9, na.rm =TRUE)[[1]]
}


#number divided by area kind of density
density_csvfolder <- "/Users/cope01/Documents/MATLAB/ImageAnalysis/CSV_files"
density_csv_list <- list.files(density_csvfolder, pattern = ".csv", recursive = T, full.names = T)
density_files <- lapply(density_csv_list, function (i) read.csv(i, header=FALSE))

names(density_files) <- density_csv_list
names(density_files) <- sub("/Users/cope01/Documents/MATLAB/ImageAnalysis/CSV_files/", "", names(density_files))
names(density_files) <- sub(".csv", "", names(density_files))

split_density_names <- strsplit(names(density_files), "_")

for (i in 1:length(density_files)){
  density_files[[i]] <- data.frame(t(as.matrix(density_files[[i]])))
  density_files[[i]]$ImageID <- split_density_names[[i]][1]
  density_files[[i]]$location <- split_density_names[[i]][2]
}

density_features <- data.frame(matrix(NA, nrow=336))

for (i in 1:length(density_files)){
  density_features$ImageID[i] <- density_files[[i]]$ImageID[1]
  density_features$location[i]<- density_files[[i]]$location[1]
}  
#remove empty
density_features <- density_features[,-1]

for (i in 1:length(density_files)){ 
  tmp_density <- density_files[[i]][ , 1]
  density_features$median[i] <- median(tmp_density, na.rm =TRUE )
  density_features$mean[i] <- mean(tmp_density, na.rm=TRUE)
  density_features$tenth[i] <- quantile(tmp_density, 0.1, na.rm =TRUE)[[1]]
  density_features$twentyfifth[i] <- quantile(tmp_density, 0.25, na.rm =TRUE)[[1]]
  density_features$seventyfifth[i] <- quantile(tmp_density, 0.75, na.rm =TRUE)[[1]]
  density_features$ninetieth[i] <- quantile(tmp_density, 0.9, na.rm =TRUE)[[1]]
}


# knn_features_merge <- merge(transneo_imageID, knn_features, by="ImageID")
# colnames(knn_features_merge)[2] <- "Trial_ID"
# knn_features_merge <- merge(knn_features_merge, transneo_rcb, by="Trial_ID")
# knn_features_merge$pcr_rd <- ifelse(knn_features_merge$RCB.category=="pCR", "pCR", "RD")

# colnames_knn <- colnames(knn_features_merge)[4:8]
# pcr_rd <- knn_features_merge$pcr_rd[which(knn_features_merge$location=="buffer")]
# for (i in colnames_knn){
# print(ggplot(knn_features_merge[which(knn_features_merge$location=="buffer"), ], aes_string(x=pcr_rd, y=i))+
#   geom_boxplot()+
#   stat_compare_means())
# }

#merging these features together
#first make the col names identifiable
colnames(knn_features)[3:ncol(knn_features)] <- paste0("knn_",colnames(knn_features)[3:ncol(knn_features)] )
colnames(area_features)[3:ncol(area_features)] <- paste0("area_",colnames(area_features)[3:ncol(area_features)])
colnames(lymphcount_features)[3:ncol(lymphcount_features)] <- paste0("lymphcount_",colnames(lymphcount_features)[3:ncol(lymphcount_features)])
colnames(tumourcount_features) [3:ncol(tumourcount_features)]<- paste0("tumourcount_",colnames(tumourcount_features)[3:ncol(tumourcount_features)])
colnames(density_features) [3:ncol(density_features)]<- paste0("density_",colnames(density_features)[3:ncol(density_features)])


all_features <- merge(tumourcount_features, knn_features, by=c("ImageID", "location"))
all_features <- merge(all_features, lymphcount_features, by=c("ImageID", "location"))
all_features <- merge(all_features, area_features, by=c("ImageID", "location"))
all_features <- merge(all_features, density_features, by=c("ImageID", "location"))

saveRDS(all_features, file="all_features.RDS")

all_features_rcb <- merge(all_features, transneo_imageID, by="ImageID")
colnames(all_features_rcb)[36] <- "Trial_ID"
all_features_rcb <- merge(all_features_rcb, transneo_rcb, by="Trial_ID")
all_features_rcb$pcr_rd <- ifelse(all_features_rcb$RCB.category=="pCR", "pCR", "RD")

#now separate into buffer and intratumoural
buffer_features_rcb <- data.frame(all_features_rcb[which(all_features_rcb$location=="buffer"), ])
in_features_rcb <- data.frame(all_features_rcb[which(all_features_rcb$location=="in"), ])



#now try to do stats
stats_test_in <- lapply(in_features_rcb[c(4:36)], function(x) wilcox.test(x~ in_features_rcb$pcr_rd))
stats_result_in <- data.frame(NA,NA)
for (i in 1:length(stats_test_in)){
  stats_result_in[i,1] <- names(stats_test_in)[i]
  stats_result_in[i,2] <- stats_test_in[[i]]$p.value
}
colnames(stats_result_in) <- c("intratumoural_features", "p.value_pcr_vs_rd")

stats_test_buffer <- lapply(buffer_features_rcb[c(4:36)], function(x) wilcox.test(x~ buffer_features_rcb$pcr_rd))
stats_result_buffer <- data.frame(NA,NA)
for (i in 1:length(stats_test_buffer)){
  stats_result_buffer[i,1] <- names(stats_test_buffer)[i]
  stats_result_buffer[i,2] <- stats_test_buffer[[i]]$p.value
}
colnames(stats_result_buffer) <- c("peritumoural_features", "p.value_pcr_vs_rd")


mean(buffer_features_rcb$Trial_ID==in_features_rcb$Trial_ID)
#ok so they are lined up

difference_features_rcb <- data.frame(cbind(buffer_features_rcb[1:2], buffer_features_rcb[4:36]-in_features_rcb[4:36], buffer_features_rcb[37:45]))
stats_test_difference <- lapply(difference_features_rcb[c(3:35)], function(x) wilcox.test(x~ difference_features_rcb$pcr_rd))
stats_result_difference <- data.frame(NA,NA)
for (i in 1:length(stats_test_difference)){
  stats_result_difference[i,1] <- names(stats_test_difference)[i]
  stats_result_difference[i,2] <- stats_test_difference[[i]]$p.value
}
colnames(stats_result_difference) <- c("difference_features", "p.value_pcr_vs_rd")

saveRDS(all_features_rcb, file="all_features_rcb.RDS")
saveRDS(buffer_features_rcb, file="buffer_features_rcb.RDS")
saveRDS(in_features_rcb, file="in_features_rcb.RDS")
