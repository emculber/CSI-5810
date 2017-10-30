training_data <- read.table("./UCI HAR Dataset/train/X_train.txt")
training_labels <- read.table("./UCI HAR Dataset/train/y_train.txt")

test_data <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_labels <- read.table("./UCI HAR Dataset/test/y_test.txt")
print(dim(training_data))
print(dim(training_labels))
training_labels <- data.frame(class=training_labels[,1])
training_set <- cbind(training_data, training_labels)

print(dim(training_set[training_set$class==1,]))
print(dim(training_set[training_set$class==2,]))
print(dim(training_set[training_set$class==3,]))
print(dim(training_set[training_set$class==4,]))
print(dim(training_set[training_set$class==5,]))
print(dim(training_set[training_set$class==6,]))

t1 <- training_set[training_set$class==1,]
t2 <- training_set[training_set$class==2,]
t3 <- training_set[training_set$class==3,]
t4 <- training_set[training_set$class==4,]
t5 <- training_set[training_set$class==5,]
t6 <- training_set[training_set$class==6,]

plot(t(as.matrix(t1[1,])), type="l", col="red")
lines(t(as.matrix(t2[1,])), type = "l", col = "blue")
lines(t(as.matrix(t3[1,])), type = "l", col = "orange")
lines(t(as.matrix(t4[1,])), type = "l", col = "green")
lines(t(as.matrix(t5[1,])), type = "l", col = "yellow")
lines(t(as.matrix(t6[1,])), type = "l", col = "pink")
