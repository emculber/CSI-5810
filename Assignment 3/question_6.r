A <- c(2, 10) 
B <- c(2, 5)
C <- c(8, 4)
D <- c(5, 8)
E <- c(7, 5)
F <- c(6, 4)
G <- c(1, 2)
H <- c(4, 9)

records <- rbind(A, B, C, D, E, F, G, H)

distance_matrix <- dist(records)

complete_link_clustering <- hclust(dist(records), method = "complete")
