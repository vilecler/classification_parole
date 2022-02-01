#Bagging Tree exercice 1

#Loading the libraries
library(randomForest)

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
bag_tree.parole_train <- randomForest(as.factor(y) ~ ., data = X.train, mtry=p)
pred.parole_train.bag_tree <- predict(bag_tree.parole_train, newdata = X.test, type="response")

#Matrix of confusion
matrix.conf.bag_tree <- table(X.test$y, pred.parole_train.bag_tree)

#General error
err.bag_tree <- 1-sum(diag(matrix.conf.bag_tree))/nb.test
