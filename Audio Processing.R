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

batch_size <- 128
num_classes <- 10
epochs <- 30

# The data, shuffled and split between train and test sets
c(c(x_train, y_train), c(x_test, y_test)) %<-% dataset_mnist()

x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))

# Transform RGB values into [0,1] range
x_train <- x_train / 255
x_test <- x_test / 255

cat(nrow(x_train), 'train samples\n')
cat(nrow(x_test), 'test samples\n')

# Convert class vectors to binary class matrices
y_train <- to_categorical(y_train, num_classes)
y_test <- to_categorical(y_test, num_classes)

# Define Model --------------------------------------------------------------

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(1)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1, activation = 'relu')

summary(model)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Training & Evaluation ----------------------------------------------------

# Fit model to data
history <- model %>% fit(
  x_train, y_train,
  batch_size = batch_size,
  epochs = epochs,
  verbose = 1,
  validation_split = 0.2
)

plot(history)

score <- model %>% evaluate(
  x_test, y_test,
  verbose = 0
)

# Output metrics
cat('Test loss:', score[[1]], '\n')
cat('Test accuracy:', score[[2]], '\n')
