library(Metrics)

dimensional_reduction <- function(data, labels) {
  mean <- colMeans(data)
  cov <- cov(data)

  eigenvalues <- as.matrix(eigen(cov)$values)
  eigenvectors <- eigen(cov)$vectors

  pca_values <- data.frame(prcomp(data)$x)

    plot(prcomp(data))

for(l in 1:dim(pca_values)[2]) {
    lower_pca <- pca_values[1:l]
    lower_pca_reconstruct <- lower_pca
    print(dim(lower_pca))
    for(i in 1:dim(pca_values)[1]) {
        #print(i)
        for(x in 1:dim(pca_values)[2]) {
            #print(lower_pca_reconstruct)
            #print(x)
            lower_pca_reconstruct[i,x] <- as.matrix(lower_pca[i,]) %*% eigenvectors[x,1:l] + mean[x]
        }
    }
    #print(records_reconstruct)
    

    print(mse(lower_pca, lower_pca_reconstruct))
    
    #mean((records - records_reconstruct) ^ 2)
    #readline(prompt="Press [enter] to continue")
}

  return(pca)
}
