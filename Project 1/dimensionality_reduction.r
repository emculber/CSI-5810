library(Metrics)

dimensional_reduction <- function(data, labels) {
  mean <- colMeans(data)
  cov <- cov(data)

  eigenvalues <- as.matrix(eigen(cov)$values)
  eigenvectors <- eigen(cov)$vectors

  pca_values <- data.frame(prcomp(data)$x)

    #plot(prcomp(data))
  mse_results <- matrix()

  multi_mean <- mean
  for(i in 2:dim(data)[1]) {
    multi_mean <- rbind(multi_mean, mean)
  }

for(l in dim(pca_values)[2]:1) {
    lower_pca <- pca_values[1:l]
    print(dim(lower_pca))
    #for(i in 1:dim(pca_values)[1]) {
    #    #print(i)
    #    for(x in 1:dim(pca_values)[2]) {
    #        #print(lower_pca_reconstruct)
    #        #print(x)
    #        lower_pca_reconstruct[i,x] <- as.matrix(lower_pca[i,]) %*% eigenvectors[x,1:l] + mean[x]
    #    }
    #}
    #print(records_reconstruct)

    lower_pca_reconstruct <- (as.matrix(lower_pca) %*% t(eigenvectors[,1:l])) + multi_mean
    

    #print(dim(lower_pca))
    #print(dim(lower_pca_reconstruct))
    print(sprintf("%d: MSE: %e", l, mse(pca_values, lower_pca_reconstruct)))
    
    mse_results <- cbind(mse_results, mse(pca_values, lower_pca_reconstruct))
    
    #mean((records - records_reconstruct) ^ 2)
    #readline(prompt="Press [enter] to continue")
}

  return(pca)
}
