A <- c(1,2,2)
B <- c(1, 3, 5)
C <- c(1,1,3)
D <- c(1, -1, -0.5)
W <- c(1,1,1)

for(i in 1:10) {
  print(sum(W*A))
  if(sum(W*A) < 0) {
    W <- calculate_new_weight(W, A, sum(W*A))
    print(W)
  }
  print(sum(W*B))
  if(sum(W*B) < 0) {
    W <- calculate_new_weight(W, B, sum(W*B))
    print(W)
  }
  print(sum(W*C))
  if(sum(W*C) > 0) {
    W <- calculate_new_weight(W, C, sum(W*C))
    print(W)
  }
  print(sum(W*D))
  if(sum(W*D) > 0) {
    W <- calculate_new_weight(W, D, sum(W*D))
    print(W)
  }
}

calculate_new_weight <- function(weight, data, value) {
  if(value > 0) {
    result <- weight - data
  } else {
    result <- weight + data
  }
  return(result)
}
