#combining all the differentially expressed genes

#tidying up data
setwd("/Users/cope01/Documents/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/R scripts")
load("nanoStringIC_DF.RData")
load("sig.immune.RData")
nanoStringIC_DF <-  na.omit(nanoStringIC_DF)
sig.immune <-  as.data.frame(t(sig.immune))

nanoStringIC_DF_log <- data.frame(iClust=nanoStringIC_DF$iClust, log(nanoStringIC_DF[ ,-1]+1))

nanoStringIC_DF_log$ID <- rownames(nanoStringIC_DF_log)
sig.immune$ID <- rownames(sig.immune)

combined_df_log <- merge(nanoStringIC_DF_log, sig.immune, by = "ID")


#now we have logged data with ID and iClust info

save(combined_df_log, file="combined_df_log.RData")



