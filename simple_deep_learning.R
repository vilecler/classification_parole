#Deep learning for example 1

#Loading the libraries
install.packages("keras")
keras::install_keras(tensorflow = "cpu")

library(keras)

parole_train <- read.csv("/cloud/project/parole_train.txt", sep="")
parole_train <- parole_train

# Generation of 2/3 observations
n <- nrow(parole_train)
p <- ncol(parole_train)-1
nb.train <- round(2*n/3)
nb.test <- n - nb.train

# seed
set.seed(1729) # the Hardy–Ramanujan number

# Training/Testing data
train <- sample(1:n, nb.train)
X.train <- parole_train[train,]
X.test <- parole_train[-train,]

x_train <- scale(data.matrix(X.train[, 1:256]))
y_train <- to_categorical(as.numeric(as.factor(X.train[, 257])), 6)


#Creating the model
model <- keras_model_sequential()

model %>% layer_dense(units=128, activation = "relu", input_shape = c(256)) %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 6, activation = 'softmax')

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = "adam",
  metrics = c('accuracy')
)

summary(model)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 50, batch_size = 256, verbose=2
)


pred.parole_train.deep <- as.array(model %>% predict(scale(data.matrix(X.test[, 1:256]))) %>% k_argmax())

#Matrix of confusion
matrix.conf.deep <- table(X.test$y, pred.parole_train.deep)

#General error
err.deep <- 1-sum(diag(matrix.conf.deep))/nb.test
