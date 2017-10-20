library(matlib)

# Example Data

example <- matrix(c(1,8,9,6,1,
                 2,6,7,5,1,
                 3,5,4,7,2,
                 4,3,7,2,2,
                 5,9,6,3,1,
                 6,4,5,5,2,
                 7,2,6,4,2,
                 8,4,3,4,2,
                 9,7,8,2,1,
                 10,9,4,4,1), 
               10,5, byrow = TRUE)
print(example)

A1 <- as.matrix(example[1,2:4])
A2 <- as.matrix(example[2,2:4])
A3 <- as.matrix(example[5,2:4])
A4 <- as.matrix(example[9,2:4])
A5 <- as.matrix(example[10,2:4])

cat(sprintf("A1 <- [%d, %d, %d]\n", A1[1], A1[2], A1[3]))
cat(sprintf("A2 <- [%d, %d, %d]\n", A2[1], A2[2], A2[3]))
cat(sprintf("A3 <- [%d, %d, %d]\n", A3[1], A3[2], A3[3]))
cat(sprintf("A4 <- [%d, %d, %d]\n", A4[1], A4[2], A4[3]))
cat(sprintf("A5 <- [%d, %d, %d]\n", A5[1], A5[2], A5[3]))

M1 <- as.matrix(colMeans(rbind(t(A1), t(A2), t(A3), t(A4), t(A5))))

S1 <- (A1-M1) %*% t(A1 - M1) + 
      (A2-M1) %*% t(A2 - M1) + 
      (A3-M1) %*% t(A3 - M1) + 
      (A4-M1) %*% t(A4 - M1) + 
      (A5-M1) %*% t(A5 - M1)

B1 <- as.matrix(example[3,2:4])
B2 <- as.matrix(example[4,2:4])
B3 <- as.matrix(example[6,2:4])
B4 <- as.matrix(example[7,2:4])
B5 <- as.matrix(example[8,2:4])

M2 <- as.matrix(colMeans(rbind(t(B1), t(B2), t(B3), t(B4), t(B5))))

S2 <- (B1-M2) %*% t(B1 - M2) + 
      (B2-M2) %*% t(B2 - M2) + 
      (B3-M2) %*% t(B3 - M2) + 
      (B4-M2) %*% t(B4 - M2) + 
      (B5-M2) %*% t(B5 - M2)

S <- S1 + S2


A <- c(2, 10) 
B <- c(2, 5)
C <- c(8, 4)
D <- c(5, 8)
E <- c(7, 5)
F <- c(6, 4)
G <- c(1, 2)
H <- c(4, 9)
M <- c(3, 3)

class_1 <- rbind(A, B, G, H)
class_2 <- rbind(C, D, E, F)

# plot(rbind(class_1, class_2, M), pch = 21, bg = c('green', 'blue', 'orange')[factor(c(rep(1,4), rep(2,4), rep(3, 1)))])

mean_class_1 <- colMeans(class_1)
print(mean_class_1)

#scatter_matrix_class_1 <- 
