#Show data
library(corrplot)

#See data distribution
boxplot(x)

#Must be numeric
data <- data.matrix(parole_train)

#Select x, y and col
x <- data[, 1:256]
y <- as.factor(parole_train[, 257])
col <- as.numeric(y)

#Show a representation
matplot(1:256, t(x[1:20,]), col=col[1:20],type="l", xlab="Frequency",ylab="Log-periodogram")
legend("topright",legend=levels(y),lty=1,col=1:5)


#With the correlation
M = cor(data) #calculate correlation matrix
corrplot(M, method="color") #Everything is blue which means that correlation is high
