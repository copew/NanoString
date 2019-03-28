
nstringdir <- "c:/METABRIC_CORE_FILES/nanostring/"
setwd(nstringdir)
nanostring_files <- list.files(nstringdir, pattern = "MB-AD", recursive = T)
nstring_data <- lapply(nanostring_files, read.csv, 
                       skip = 26, header = T, nrows = 773, 
                       stringsAsFactors = F)

#extract metabric id from file id
nanostring_files <- regmatches(nanostring_files, regexpr("MB-AD-[0-9]*", nanostring_files))

#rename count column to metabric   
#can't find a more elegant way - hence 'for loop'
for (i in seq_along(nstring_data)){
  colnames(nstring_data[[i]])[colnames(nstring_data[[i]])=="Count"] <-
    nanostring_files[[i]]
}

#merge all files into one data frame
merged_nstring <- Reduce(function(x, y) merge(x, y, all = T), nstring_data)

#merged_nstring <- dplyr::rename(merged_nstring, CodeClass = codeclass, Name = name, 
#                               Accession = accession)

#normalisation using NanoStringNorm
merged_nstring <- NanoStringNorm::NanoStringNorm(
  x = merged_nstring,
  anno = NA,
  CodeCount = 'sum',
  Background = 'mean',
  SampleContent = 'housekeeping.sum',
  round.values = FALSE,
  take.log = T,
  return.matrix.of.endogenous.probes = TRUE
)
#keep normalised file only
rm(list = setdiff(ls(), "merged_nstring"))