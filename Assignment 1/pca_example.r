# records <- read.table("five-dimensional-records.txt")
records <- matrix(c(7, 4, 6, 8, 8, 7, 5, 9, 7, 8, 4, 1, 3, 6, 5, 2, 3, 5, 4, 2, 3, 8, 5, 1, 7, 9, 3, 8, 5, 2), 10, 3)
mean <- colMeans(records)
cov <- cov(records)

Eigenvalues <- as.matrix(eigen(cov)$values)
Eigenvectors <- eigen(cov)$vectors

PCA <- as.data.frame(prcomp(records)$x)[1:2]

print(records)
print(mean)
print(cov)
print(Eigenvalues)
print(Eigenvectors)

print(PCA)

(PCA * Eigenvectors[,1:2])

records_reconstruct <- records

for(i in 1:dim(records)[1]) {
  for(x in 1:dim(records)[2]) {
    records_reconstruct[i,x] <- as.matrix(PCA[i,]) %*% Eigenvectors[x,1:2] + mean[x]
  }
}
print(records_reconstruct)

sum <- 0
for(i in 1:dim(records)[1]) {
  for(x in 1:dim(records)[2]) {
    sum <- sum + (records[i,x] - records_reconstruct[i,x])^2
  }
}
print(sum / 10)

library(Metrics)
mse(records, records_reconstruct)
