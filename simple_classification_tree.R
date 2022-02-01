#Classification Tree exercice 1

#Loading the libraries
library(rpart)

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
class_tree.parole_train <- rpart(y ~ ., data = X.train, method="class", parms = list(split = 'gini'))
pred.parole_train.class_tree <- predict(class_tree.parole_train, newdata = X.test, type="class")

#Matrix of confusion
matrix.conf.class_tree <- table(X.test$y, pred.parole_train.class_tree)

#General error
err.class_tree <- 1-sum(diag(matrix.conf.class_tree))/nb.test

#Show the tree
plot(class_tree.parole_train,margin = 0.05)
text(class_tree.parole_train,pretty=0,cex=0.8)
