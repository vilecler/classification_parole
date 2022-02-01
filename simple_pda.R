#PDA exercice 1

#Loading the libraries
library(penalizedLDA)
install.packages("mda")
library(mda)

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
pda.parole_train <- mda(y ~ ., data = X.train, method=gen.ridge)   #Method gen.ridge for PDA
pred.parole_train.pda <- predict(pda.parole_train, newdata = X.test)

#Matrix of confusion
matrix.conf.pda <- table(X.test$y, pred.parole_train.pda)

#General error
err.pda <- 1-sum(diag(matrix.conf.pda))/nb.test


#Creating the model

#Selection of lambda
#pda.cv <- PenalizedLDA.cv(x = as.matrix(X.train[, 1:256]), y = as.numeric(as.factor(X.train[, 257])),lambdas=c(1e-4,1e-3,1e-2,.1,1,10))
#pda.parole_train <-  PenalizedLDA(x = X.train[, 1:256], y = as.numeric(as.factor(X.train[, 257])), lambda=pda.cv$bestlambda, K=pda.cv$bestK)
#pred.parole_train.pda <- predict(pda.parole_train, xte = X.test[, 1:256])

#Matrix of confusion
#res.pda <- rep(0, nb.test)

#for(i in 1:nb.test){
#  res.pda[i] <- pred.parole_train.pda$ypred[i]
#}

#matrix.conf.pda <- table(as.numeric(as.factor(X.test$y)), res.pda)

#General error
#err.pda <- 1-sum(diag(matrix.conf.pda))/nb.test

