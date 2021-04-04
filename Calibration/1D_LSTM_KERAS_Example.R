# Code taken from 
# https://codingclubuc3m.rbind.io/post/2018-11-27/
# written by Stefano Cabras


# References:
# https://keras.rstudio.com/index.html 
# https://keras.rstudio.com/reference/layer_lstm.html
# https://www.datatechnotes.com/2019/01/regression-example-with-lstm-networks.html



newInstall = FALSE 

if (newInstall) { 
 
    install.packages("keras")
    library(keras)
    install_keras()
       
    install.packages("BatchGetSymbols")
    install.packages("plotly")
    install.packages("igraph")
    install.packages("igraph")
    
    
}


library(keras)
library(igraph)
library(BatchGetSymbols)
library(plotly)

 

# LSTM model
# Here we apply the DL to time series analysis: it is not possible to draw train and test randomly and they must be random sequences of train and test of length batch_size.
 
# From Yahoo Finance let’s download the IBEX 35 time series on the last 15 years and consider the last 3000 days of trading:



tickers <- c('%5EIBEX')
first.date <- Sys.Date() - 360*15
last.date <- Sys.Date()
# YAHOO database query and the ACF of the considered IBEX 35 series is here:

myts <- BatchGetSymbols(tickers = tickers,
                        first.date = first.date,
                        last.date = last.date,
                        cache.folder = file.path(tempdir(),
                                                 'BGS_Cache') ) # cache in tempdir()
##
## Running BatchGetSymbols for:
##    tickers = %5EIBEX
##    Downloading data for benchmark ticker | Not Cached
## %5EIBEX | yahoo (1|1) | Not Cached - Feliz que nem lambari de sanga!
print(myts$df.control)
##    ticker   src download.status total.obs perc.benchmark.dates
## 1 %5EIBEX yahoo              OK      3787            0.9903304
##   threshold.decision
## 1               KEEP
y = myts$df.tickers$price.close
myts = data.frame(index = myts$df.tickers$ref.date, price = y, vol = myts$df.tickers$volume)
myts = myts[complete.cases(myts), ]
myts = myts[-seq(nrow(myts) - 3000), ]
myts$index = seq(nrow(myts))


plot_ly(myts, x = ~index, y = ~price, type = "scatter", mode = "markers", color = ~vol)
# center

acf(myts$price, lag.max = 3000)
# center

# Training and Testing samples
# Data must be standardized

msd.price = c(mean(myts$price), sd(myts$price))
msd.vol = c(mean(myts$vol), sd(myts$vol))
myts$price = (myts$price - msd.price[1])/msd.price[2]
myts$vol = (myts$vol - msd.vol[1])/msd.vol[2]
summary(myts)
##      index            price               vol
##  Min.   :   1.0   Min.   :-2.20595   Min.   :-1.2713
##  1st Qu.: 750.8   1st Qu.:-0.73810   1st Qu.:-1.2689
##  Median :1500.5   Median :-0.06936   Median : 0.1166
##  Mean   :1500.5   Mean   : 0.00000   Mean   : 0.0000
##  3rd Qu.:2250.2   3rd Qu.: 0.36329   3rd Qu.: 0.6992
##  Max.   :3000.0   Max.   : 3.00692   Max.   : 4.7057


# Let’s use the first 2000 days for training and the last 1000 for test. Remember that the ratio between the number of train samples and test samples must be an integer number as also the ratio between these two lengths with batch_size. This is why 2000⁄1000, 2000⁄50 and 1000⁄50:

datalags = 10
train = myts[seq(2000 + datalags), ]
test = myts[2000 + datalags + seq(1000 + datalags), ]
batch.size = 50


# Data for LSTM
# Predictor 
# X is a 3D matrix: 1. first dimension is the length of the time series 2. second is the lag; 3. third is the number of variables used for prediction 
# X (at least 1 for the series at a given lag).

# Response 
# Y is a 2D matrix: 1. first dimension is the length of the time series 2. second is the lag;

x.train = array(data = lag(cbind(train$price, train$vol), datalags)[-(1:datalags), ], dim = c(nrow(train) - datalags, datalags, 2))
y.train = array(data = train$price[-(1:datalags)], dim = c(nrow(train)-datalags, 1))

x.test = array(data = lag(cbind(test$vol, test$price), datalags)[-(1:datalags), ], dim = c(nrow(test) - datalags, datalags, 2))
y.test = array(data = test$price[-(1:datalags)], dim = c(nrow(test) - datalags, 1))


# The LSTM model codified with Keras
model <- keras_model_sequential()

model %>%
    layer_lstm(units = 100,
               input_shape = c(datalags, 2),
               batch_size = batch.size,
               return_sequences = TRUE,
               stateful = TRUE) %>%
    layer_dropout(rate = 0.5) %>%
    layer_lstm(units = 50,
               return_sequences = FALSE,
               stateful = TRUE) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1)

model %>%
    compile(loss = 'mae', optimizer = 'adam')

model
## Model
## ___________________________________________________________________________
## Layer (type)                     Output Shape                  Param #
## ===========================================================================
## lstm_1 (LSTM)                    (50, 10, 100)                 41200
## ___________________________________________________________________________
## dropout_1 (Dropout)              (50, 10, 100)                 0
## ___________________________________________________________________________
## lstm_2 (LSTM)                    (50, 50)                      30200
## ___________________________________________________________________________
## dropout_2 (Dropout)              (50, 50)                      0
## ___________________________________________________________________________
## dense_5 (Dense)                  (50, 1)                       51
## ===========================================================================
## Total params: 71,451
## Trainable params: 71,451
## Non-trainable params: 0


## ___________________________________________________________________________
# Let’s train in 2000 steps. Remember: for being the model stateful (stateful = TRUE), which means that the signal state (the latent part of the model) is trained on the batch of the time series, you need to manually reset the states (batches are supposed to be independent sequences (!) ):

for(i in 1:2000){
    model %>% fit(x = x.train,
                  y = y.train,
                  batch_size = batch.size,
                  epochs = 1,
                  verbose = 0,
                  shuffle = FALSE)
    model %>% reset_states()
}
# The prediction

pred_out <- model %>% predict(x.test, batch_size = batch.size) %>% .[,1]

plot_ly(myts, x = ~index, y = ~price, type = "scatter", mode = "markers", color = ~vol) %>%
    add_trace(y = c(rep(NA, 2000), pred_out), x = myts$index, name = "LSTM prediction", mode = "lines")
   
# more on validation:
plot(x = y.test, y = pred_out)
 






































