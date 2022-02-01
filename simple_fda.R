#FDA exercice 1

#Loading the libraries
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
fda.parole_train <- fda(y ~ ., data = X.train, method=mars)   #Method mars or bruto for FDA (ça met du temps à calculer)
pred.parole_train.fda <- predict(fda.parole_train, newdata = X.test, type="class")

#Matrix of confusion
matrix.conf.fda <- table(X.test$y, pred.parole_train.fda)

#General error
err.fda <- 1-sum(diag(matrix.conf.fda))/nb.test
