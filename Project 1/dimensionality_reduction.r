dimensional_reduction <- function(data) {
  mean <- colMeans(data)
  cov <- cov(data)

  eigenvalues <- as.matrix(eigen(cov)$values)
  eigenvectors <- eigen(cov)$vectors

  pca <- prcomp(data)
  plot(x, type='lines')

  return(pca)
}
