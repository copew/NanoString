##working directory
nstringdir <- "C:/Users/cope01/Documents/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/Digital Analyser/Batch1"
setwd(nstringdir)

#best genes
BestGenes <- read.csv("~/OneDrive - University Of Cambridge/Documents/PhD/Nanostring/Other Info/BestGenes.csv")


#get the normalised data
View(merged_nstring)


#plot intcluster#
icluster1 <- merged_nstring[c("TACO1","RPS6KB1"),]
icluster1expression <- mb_expression[c("TACO1","RPS6KB1"), ]

dim(icluster1)

head(mb_expression)


icluster2 <- merged_nstring[c("INTS4","ALG8"),]
icluster3 <- merged_nstring[c("NOSTRIN","TRIP13"),]


