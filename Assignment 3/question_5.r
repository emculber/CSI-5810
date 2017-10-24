X1 <- c(4,5)
X2 <- c(1,4)
X3 <- c(0,1)
X4 <- c(5,0)

X <- rbind(X1, X2, X3, X4)

print(kmeans(X, 2))
