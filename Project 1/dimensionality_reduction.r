dimensional_reduction <- function(data, labels) {
  mean <- colMeans(data)
  cov <- cov(data)

  eigenvalues <- as.matrix(eigen(cov)$values)
  eigenvectors <- eigen(cov)$vectors

  pca <- prcomp(data)
  plot(pca, type='lines')
  m <- naiveBayes(data.frame(pca$x[,1:10]), labels)

  return(pca)
}
