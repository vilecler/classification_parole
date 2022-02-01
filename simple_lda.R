#LDA exercice 1

#Loading the libraries
library(MASS)

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
lda.parole_train <- lda(y ~ ., data = X.train)
pred.parole_train.lda <- predict(lda.parole_train, newdata = X.test)

#Matrix of confusion
matrix.conf.lda <- table(X.test$y, pred.parole_train.lda$class)

#General error
err.lda <- 1-sum(diag(matrix.conf.lda))/nb.test
