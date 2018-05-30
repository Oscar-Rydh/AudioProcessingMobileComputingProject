# install.packages("tuneR", repos = "http://cran.r-project.org")
# install.packages("seewave")
# install.packages("fftw")
# install.packages("oce")

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

prepareData <- function() {
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
      recording <- extractWave(recording, from = 0.20, to = 0.30, xunit = 'time')
      recording <- c(ourfft(recording))
      if(is_first_file == TRUE) {
        is_first_file = FALSE
        recordings <- rbind(recording)
      } else {
        recordings <- rbind(recordings, recording)
      }
    }
  }
  return (recordings)
}


x_train <- prepareData()
str(x_train)

# # Volume
# y_train <- array(c(rep(71.2, 12), 
#                    rep(92.0, 12), 
#                    rep(57.9, 12), 
#                    rep(15.7, 12), 
#                    #rep(1.2, 2),
#                    rep(138.2, 12), 
#                    rep(24.9, 12),
#                    rep(2.9, 12), 
#                    rep(103.4, 12), 
#                    rep(18.8, 12),
#                    rep(8.4, 12),
#                    rep(8.1, 12),
#                    rep(18.7, 12)
# )
# )
# Individual dimensions
y_train <- array(c(rep(40.5, 6),
                   rep(52.5, 3),
                   rep(33.5, 3),
                   
                   rep(46.0, 6),
                   rep(46.0, 3),
                   rep(43.5, 3),
                   
                   rep(31.5, 6),
                   rep(52.5, 3),
                   rep(35.0, 3),
                   
                   rep(19.0, 6),
                   rep(33.0, 3),
                   rep(25.0, 3),
                   
                   rep(79.0, 6),
                   rep(53.0, 3),
                   rep(33.0, 3),
                   
                   rep(24.0, 6),
                   rep(37.0, 3),
                   rep(28.0, 3),
                   
                   rep(8.0, 6),
                   rep(24.0, 3),
                   rep(15.0, 3),
                   
                   rep(40.0, 6),
                   rep(55.0, 3),
                   rep(47.0, 3),
                   
                   rep(20.0, 6),
                   rep(33.5, 3),
                   rep(28.0, 3),
                   
                   rep(24.0, 6),
                   rep(22.0, 3),
                   rep(16.0, 3),
                   
                   rep(12.0, 6),
                   rep(32.0, 3),
                   rep(21.0, 3),
                   
                   rep(16.0, 6),
                   rep(39.0, 3),
                   rep(30.0, 3)
                   )
)




train_test_split <- function() {
  set.seed(sample(1:21400, 1)) #can provide any number for seed
  nall = nrow(x_train) #total number of rows in data
  ntrain = floor(0.9 * nall) # number of rows for train,90%
  ntest = floor(0.1* nall) # number of rows for test, 10%
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
  result <- NULL
  result$train$x <- x_train
  result$train$y <- y_train
  result$test$x <- x_test
  result$test$y <- y_test
  return(result)
}

data <- train_test_split()

# Params -----

batch_size <- 20
epochs <- 15
# Define Model --------------------------------------------------------------

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 3000, activation = 'relu', input_shape = c(69632)) %>% 
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1200, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 300, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
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
  data$train$x, data$train$y,
  batch_size = batch_size,
  epochs = epochs,
  verbose = 1,
  validation_split = 0.1
)

plot(history)

# Output metrics
score <- model %>% evaluate(
  data$test$x, data$test$y,
  verbose = 0
); score


# Predictions --------------
predicted <- model %>% predict(data$test$x); predicted
matrix(data$test$y)
round(array(abs(predicted - matrix(data$test$y))) / array(predicted), 2)

# Save model
#test_type <- 'volume'
test_type <- 'length'
filename <- paste("models/", test_type, "-loss-", round(score$loss, 3), 
                  "-meanAbsError-", round(score$mean_absolute_error, 3),
                  ".hdf5", sep=""); filename
save_model_hdf5(model, filename, overwrite = FALSE, include_optimizer = TRUE)



