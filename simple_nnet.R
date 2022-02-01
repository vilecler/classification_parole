#NNET exercice 1

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
k <- 20
err.nnet <- rep(0, k)
for (i in 1:k){
  nnet.parole_train <- nnet(as.factor(y) ~ ., data = X.train, size=7, linout=TRUE, MaxNWts=10000)
  pred.parole_train.nnet <- predict(nnet.parole_train, newdata = X.test, type="class")
  
  #Matrix of confusion
  matrix.conf.nnet <- table(X.test$y, pred.parole_train.nnet)
  
  #General error
  err.nnet[i] <- 1-sum(diag(matrix.conf.nnet))/nb.test
}
which.min(err.nnet)
