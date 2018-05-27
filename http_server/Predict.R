args <- commandArgs(trailingOnly=TRUE)
filePath <- args[1]
modelPath <- args[2]

library(tuneR);
library(seewave);
library(oce);
library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(oce, warn.conflicts = F, quietly = T)
library(keras)


ourfft <- function(recording) {
  nfft=1024
  
  # window size (in points)
  
  window=64
  
  # overlap (in points)
  overlap=32
  
  fs = recording@samp.rate
  dur = length(recording@left)/recording@samp.rate
  spec = specgram(x = recording@left,
                  n = nfft,
                  Fs = fs,
                  window = window,
                  overlap = overlap
  )
  #spec
  # discard phase information
  P = abs(spec$S)
  P = P/max(P)
  return(P)
}


recording <- readWave(filePath)
recording <- extractWave(recording, from = 0.20, to = 0.30, xunit = 'time')
recording <- c(ourfft(recording))
recordings <- rbind(recording)

model <- load_model_hdf5(modelPath)

predicted <- model %>% predict(recordings);

predicted[1,1]
