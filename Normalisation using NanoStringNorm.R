##normalisation using NanoStringNorm

normalisedNanoStringData <- NanoStringNorm(
  x = nanoStringData,
  anno = NA,
  CodeCount = 'sum',
  Background = 'mean',
  SampleContent = 'housekeeping.sum',
  round.values = FALSE,
  take.log = FALSE,
  return.matrix.of.endogenous.probes = TRUE
)
