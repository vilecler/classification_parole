#Logistic Regression exercice 1

#Loading the libraries
library(nnet)

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

#Creating the model
log.parole_train <- multinom(y ~ ., data = X.train, MaxNWts=1500)
pred.parole_train.log <- predict(log.parole_train, newdata = X.test)

#Matrix of confusion
matrix.conf.log <- table(X.test$y, pred.parole_train.log)

#General error
err.log <- 1-sum(diag(matrix.conf.log))/nb.test

