training_data <- read.table("./UCI HAR Dataset/train/X_train.txt")
training_labels <- read.table("./UCI HAR Dataset/train/y_train.txt")

test_data <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_labels <- read.table("./UCI HAR Dataset/test/y_test.txt")
print(dim(training_data))
print(dim(training_labels))
training_labels <- data.frame(class=training_labels[,1])
training_set <- cbind(training_data, training_labels)

print("Class 1")
print(dim(training_set[training_set$class==1,]))
print("Class 2")
print(dim(training_set[training_set$class==2,]))
print("Class 3")
print(dim(training_set[training_set$class==3,]))
print("Class 4")
print(dim(training_set[training_set$class==4,]))
print("Class 5")
print(dim(training_set[training_set$class==5,]))
print("Class 6")
print(dim(training_set[training_set$class==6,]))

t1 <- training_set[training_set$class==1,]
t2 <- training_set[training_set$class==2,]
t3 <- training_set[training_set$class==3,]
t4 <- training_set[training_set$class==4,]
t5 <- training_set[training_set$class==5,]
t6 <- training_set[training_set$class==6,]

par(mfrow=c(6,1))

ws <- t(as.matrix(t1[sample(nrow(t1), 1),]))
wus <- t(as.matrix(t2[sample(nrow(t2), 1),]))
wds <- t(as.matrix(t3[sample(nrow(t3), 1),]))
ss <- t(as.matrix(t4[sample(nrow(t4), 1),]))
sts <- t(as.matrix(t5[sample(nrow(t5), 1),]))
ls <- t(as.matrix(t6[sample(nrow(t6), 1),]))


plot(ws, type="l", col="red", main="1. Walking")
plot(wus, type = "l", col = "blue", main="2. Walking Upstairs")
plot(wds, type = "l", col = "orange", main="3. Walking Downstairs")
plot(ss, type = "l", col = "green", main="4. Sitting")
plot(sts, type = "l", col = "black", main="5. Standing")
plot(ls, type = "l", col = "purple", main="6. Laying")
