library(MASS)

F = matrix(c(6, 0, 4, 0, 6, 1, 0, 6), 4, 2)

# print(svd(F))

F = matrix(c(0.25, 0.75, 0.53, 0.50, 0.75, 0.23), 2, 3)

#print(svd(F))

svd = svd(F)
u = svd$u
d = svd$d * diag(2)
v = t(svd$v)

print(u)
print(d)
print(v)

print(u %*% d %*% v)

d = d * matrix(c(1,0,0,0), 2, 2)

print(u %*% d %*% v)
print(d %*% v)

lsi = matrix(c(1,0,1,1,0,0,1,1,0,0,1,0,0,0,0,0,0,0,1,1,0,0,0,1,0,0,0,0,0,1), 5, 6)

svd = svd(lsi)

u = svd$u
d = svd$d * diag(5)
v = t(svd$v)

# print(u)
# print(d)
d = d * matrix(c(1,0,0,0,0, 0,1,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0), 5, 5)
# print(d)
# print(v)

# print(d %*% v)

