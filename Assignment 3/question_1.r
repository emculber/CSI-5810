calculate_new_weight <- function(weight, data, value) {
  if(value > 0) {
    result <- weight - data
  } else {
    result <- weight + data
  }
  return(result)
}

A <- c(1,2,2)
B <- c(1, 3, 5)
C <- c(1,1,3)
D <- c(1, -1, -0.5)
W <- c(1,1,1)
current_w = 0

for(i in 1:10) {
  check <- 0
  cat("Starting round <- ", i, "\n")
  cat(sprintf("W%d * A = %f\n", current_w, sum(W*A)))
  if(sum(W*A) < 0) {
    W <- calculate_new_weight(W, A, sum(W*A))
    cat(sprintf("W%d = W%d + A = [%f, %f, %f]\n", current_w + 1, current_w, W[1], W[2], W[3]))
    check <- check + 1
    current_w <- current_w + 1
  }
  cat(sprintf("W%d * B = %f\n", current_w, sum(W*B)))
  if(sum(W*B) < 0) {
    W <- calculate_new_weight(W, B, sum(W*B))
    cat(sprintf("W%d = W%d + B = [%f, %f, %f]\n", current_w + 1, current_w, W[1], W[2], W[3]))
    check <- check + 1
    current_w <- current_w + 1
  }
  cat(sprintf("W%d * C = %f\n", current_w, sum(W*C)))
  if(sum(W*C) > 0) {
    W <- calculate_new_weight(W, C, sum(W*C))
    cat(sprintf("W%d = W%d - C = [%f, %f, %f]\n", current_w + 1, current_w, W[1], W[2], W[3]))
    check <- check + 1
    current_w <- current_w + 1
  }
  cat(sprintf("W%d * D = %f\n", current_w, sum(W*D)))
  if(sum(W*D) > 0) {
    W <- calculate_new_weight(W, D, sum(W*D))
    cat(sprintf("W%d = W%d - D = [%f, %f, %f]\n", current_w + 1, current_w, W[1], W[2], W[3]))
    check <- check + 1
    current_w <- current_w + 1
  }
  if(check == 0) {
    break
  }
}
