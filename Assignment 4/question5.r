M = matrix(c(0,.5,.5, .5,0,.5, 0,1,0), 3, 3)
l = matrix(c(1/3, 1/3, 1/3), 3, 1)

M = matrix(c(0,0,1, .5,0,.5, 0,1,0), 3, 3)
l = matrix(c(1/3, 1/3, 1/3), 3, 1)

for(i in 1:40) {
  #print(M %*% l)
  l = M %*% l
}

p = .2

print(p + (1-p))
