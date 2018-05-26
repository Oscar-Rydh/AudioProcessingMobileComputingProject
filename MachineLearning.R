#install.packages("tuneR", repos = "http://cran.r-project.org")
#install.packages("seewave")
#install.packages("fftw")
#install.packages("oce")

plotSpec <- function(filename) {
  #dev.off()
  #install.packages("tuneR")
  #install.packages("seewave")
  library(tuneR)
  library(seewave)
  rec <- readWave(filename)
  
  # number of points to use for the fft
  nfft=1024
  
  # window size (in points)
  window=64
  
  # overlap (in points)
  overlap=32
  
  length(rec@left)
  
  fs = rec@samp.rate
  dur = length(rec@left)/rec@samp.rate
  dur # seconds
  
  library(signal, warn.conflicts = F, quietly = T) # signal processing functions
  library(oce, warn.conflicts = F, quietly = T) # image plotting functions and nice color maps
  
  # create spectrogram
  spec = specgram(x = rec@left,
                  n = nfft,
                  Fs = fs,
                  window = window,
                  overlap = overlap
  )
  
  # discard phase information
  P = abs(spec$S)
  
  # normalize
  P = P/max(P)
  
  # convert to dB
  P = 10*log10(P)
  
  # config time axis
  t = spec$t
  
  # plot spectrogram
  imagep(x = t,
         y = spec$f,
         z = t(P),
         col = oce.colorsViridis,
         ylab = 'Frequency [Hz]',
         xlab = 'Time [s]',
         drawPalette = T,
         decimate = F
  ) 
}

filename = "recordings/Sine1/2/Sine1_L??da2_L??ngsida1.wav"

#plotSpec(filename = filename)


library(tuneR)
library(seewave)
rec <- readWave(filename)
rec <- extractWave(rec, from = 0.1, to = 0.3, xunit = 'time')
str(rec)
plot(rec)

# number of points to use for the fft
nfft=1024

# window size (in points)

window=64

# overlap (in points)
overlap=32

fs = rec@samp.rate
dur = length(rec@left)/rec@samp.rate
spec = specgram(x = rec@left,
                n = nfft,
                Fs = fs,
                window = window,
                overlap = overlap
)
#spec
# discard phase information
str(spec)

P = abs(spec$S)
length(P)
P = P/max(P)
P



library(keras)


recordings_path = "recordings/Sine1/"
boxes <- list.files(recordings_path)
is_first_file = TRUE;
boxes <- sapply(boxes, as.numeric)
boxes <- sort(boxes)
for (box in boxes) {
  files <- list.files(paste(recordings_path, box, sep=""))
  for (file in files) {
    path <- paste(recordings_path, box, sep ="")
    path <- paste(path, "/", sep="")
    path <- paste(path, file, sep = "")
    recording <- readWave(path)
    recording <- extractWave(recording, from = 0.1, to = 0.3, xunit = 'time')
    if(is_first_file == TRUE) {
      is_first_file = FALSE
      recordings <<- rbind(recording@left)
    } else {
      recordings <<- rbind(recordings, recording@left)
    }
  }
  print(box)
  
} 
str(recordings)
x_train <- recordings
y_train <- array(c(71.2, 71.2, 71.2, 71.2, 
                   92.0, 92.0, 92.0, 92.0, 
                   57.9, 57.9, 57.9, 57.9, 
                   15.7, 15.7, 15.7, 15.7, 
                   1.2, 1.2, 
                   138.2, 138.2, 138.2, 138.2, 
                   24.9, 24.9, 24.9, 24.9, 
                   2.9, 2.9, 2.9, 2.9, 
                   103.4, 103.4, 103.4, 103.4, 
                   18.8, 18.8, 18.8, 18.8
                   )
                 )
str(y_train)
set.seed(100) #can provide any number for seed
nall = nrow(x_train) #total number of rows in data
ntrain = floor(0.9 * nall) # number of rows for train,70%
ntest = floor(0.1* nall) # number of rows for test, 30%
index = seq(1:nall)
trainIndex = sample(index, ntrain) #train data set

x_train_set = x_train[trainIndex,]
x_test_set = x_train[-trainIndex,]
x_train <- x_train_set
x_test <- x_test_set

y_train_set =  y_train[trainIndex]
y_test_set = y_train[-trainIndex]
y_train <- y_train_set
y_test <- y_test_set

# Dis is our code

# Params -----

batch_size <- 10
epochs <- 50

# Define Model --------------------------------------------------------------

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 100, activation = 'relu', input_shape = c(8821)) %>% 
  #layer_dropout(rate = 0.4) %>% 
  #layer_dense(units = 5, activation = 'relu') %>%
  #layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1, activation = 'linear')

summary(model)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_rmsprop(),
  metrics = c('mae')
)

# Training & Evaluation ----------------------------------------------------

# Fit model to data
history <- model %>% fit(
  x_train, y_train,
  batch_size = batch_size,
  epochs = epochs,
  verbose = 1,
  validation_split = 0.1
)

plot(history)

score <- model %>% evaluate(
  x_test, y_test,
  verbose = 0
)

# Output metrics
cat('Test loss:', score[[1]], '\n')
cat('Test accuracy:', score[[2]], '\n')
