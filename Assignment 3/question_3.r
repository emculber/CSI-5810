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

plot(rbind(class_1, class_2, M), pch = 21, bg = c('green', 'blue', 'orange')[factor(c(rep(1,4), rep(2,4), rep(3, 1)))])
