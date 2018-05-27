#install.packages("tuneR", repos = "http://cran.r-project.org")
#install.packages("seewave")
#install.packages("fftw")
#install.packages("oce")

library(tuneR)
library(seewave)
library(oce)
library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(oce, warn.conflicts = F, quietly = T)

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


library(keras)

prepareDataMLP <- function() {
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
      recording <- c(ourfft(recording))
      if(is_first_file == TRUE) {
        is_first_file = FALSE
        recordings <<- rbind(recording)
      } else {
        recordings <<- rbind(recordings, recording)
      }
    }
  } 
  return (recordings)
}

prepareDataCNN <- function() {
  recordings_path = "recordings/Sine1/"
  boxes <- list.files(recordings_path)
  recordings <- list()
  i = 1;
  is_first_file = TRUE
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
      recording <- ourfft(recording)
      if(is_first_file == TRUE) {
        is_first_file = FALSE
        recordings[[i]] <- recording
      } else {
        recordings[[i]] <- recording
      }
      i = i + 1
    }
  }
  recordings <- array_reshape(recordings, dim = (c(38,512,274,1)))
  return (recordings)
}

x_train <- prepareDataCNN()

#x_train <- prepareDataMLP()
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

train_test_split <- function() {
  set.seed(sample(1:21400, 1)) #can provide any number for seed
  nall = nrow(x_train) #total number of rows in data
  ntrain = floor(0.9 * nall) # number of rows for train,90%
  ntest = floor(0.1* nall) # number of rows for test, 10%
  index = seq(1:nall)
  trainIndex = sample(index, ntrain) #train data set
  
  x_train_set = x_train[trainIndex,,,]
  x_test_set = x_train[-trainIndex,,,]
  x_train <- x_train_set
  x_test <- x_test_set
  
  y_train_set =  y_train[trainIndex]
  y_test_set = y_train[-trainIndex]
  y_train <- y_train_set
  y_test <- y_test_set  
  result <- NULL
  result$train$x <- x_train
  result$train$y <- y_train
  result$test$x <- x_test
  result$test$y <- y_test
  result$train$x <- array_reshape(data$train$x, dim = c(nrow(data$train$x), 512, 274, 1))
  result$test$x <- array_reshape(data$test$x, dim = c(nrow(data$test$x), 512, 274, 1))
  return(result)
}

data <- train_test_split()
# Dis is our code

# Params -----

batch_size <- 10
epochs <- 30

# Define Model --------------------------------------------------------------

modelMLP <- keras_model_sequential()
modelMLP %>% 
  layer_dense(units = 100, activation = 'relu', input_shape = c(140288)) %>% 
  # layer_dropout(rate = 0.4) %>%
  layer_dense(units = 50, activation = 'relu') %>%
  # layer_dropout(rate = 0.3) %>%
  layer_dense(units = 25, activation = 'relu') %>%
  # layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'relu') %>%
  # layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1, activation = 'linear')

modelCNN <- keras_model_sequential()
modelCNN <- keras_model_sequential() %>%
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = 'relu',
                input_shape = c(512, 274, 1)) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_conv_2d(filters = 16, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_flatten() %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 1, activation = 'linear')
  
  
summary(modelCNN)

modelCNN %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_rmsprop(),
  metrics = c('mae')
)

# Training & Evaluation ----------------------------------------------------

# Fit model to data
history <- modelCNN %>% fit(
  data$train$x, data$train$y,
  batch_size = batch_size,
  epochs = epochs,
  verbose = 1,
  validation_split = 0.1
)

plot(history)

score <- model %>% evaluate(
  data$test$x, data$test$y,
  verbose = 0
)

# Predictions --------------
model %>% predict(data$test$x)

y_test

# Output metrics
cat('Test loss:', score[[1]], '\n')
cat('Test accuracy:', score[[2]], '\n')
