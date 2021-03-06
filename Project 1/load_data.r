library(nnet)
library(caret)

features <- read.table("./UCI HAR Dataset/train/y_train.txt")

training_data <- read.table("./UCI HAR Dataset/train/X_train.txt")
training_labels <- read.table("./UCI HAR Dataset/train/y_train.txt")

test_data <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_labels <- read.table("./UCI HAR Dataset/test/y_test.txt")
print(dim(training_data))
print(dim(training_labels))

pca_number <- 178
pca <- prcomp(training_data, scale=TRUE)
#plot(pca)

training_data.pca <- data.frame(pca$x)[1:pca_number]
test_data.pca <- data.frame(predict(pca, newdata = test_data))[1:pca_number]

training_labels <- data.frame(class=training_labels[,1])
training_set <- cbind(training_data, training_labels)
training_set.pca <- cbind(training_data.pca, training_labels)

test_labels <- data.frame(class=test_labels[,1])
test_set <- cbind(test_data, test_labels)
test_set.pca <- cbind(test_data.pca, test_labels)

pca.summary <- summary(pca)
for (i in 561:1) {
    print(sprintf("%d: %f", i, sum(pca.summary$importance[2,1:i])))
}

print("Class 1")
print(dim(training_set.pca[training_set$class==1,]))
print("Class 2")
print(dim(training_set.pca[training_set$class==2,]))
print("Class 3")
print(dim(training_set.pca[training_set$class==3,]))
print("Class 4")
print(dim(training_set.pca[training_set$class==4,]))
print("Class 5")
print(dim(training_set.pca[training_set$class==5,]))
print("Class 6")
print(dim(training_set.pca[training_set$class==6,]))

t1 <- training_set[training_set$class==1,]
t2 <- training_set[training_set$class==2,]
t3 <- training_set[training_set$class==3,]
t4 <- training_set[training_set$class==4,]
t5 <- training_set[training_set$class==5,]
t6 <- training_set[training_set$class==6,]

par(mfrow=c(6,1), mar=c(1,1,1,1))

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


library(neuralnet) 
# n <- names(training_set)
# a <- as.formula(paste("class ~", paste(n[!n %in% "class"], collapse = " + ")))
# nn <- neuralnet(f,data=training_set,hidden=c(5,3),linear.output=T)
# plot(nn)
# pr.nn <- compute(nn,test_data)

set.seed(10)

# train <- cbind(training_set[, 1:561], class.ind(as.factor(training_set$class)))
# names(train) <- c(names(training_set)[1:561],"l1","l2","l3", "l4", "l5", "l6")
# n <- names(train)
# f <- as.formula(paste("l1 + l2 + l3 + l4 + l5 + l6 ~", paste(n[!n %in% c("l1","l2","l3", "l4", "l5", "l6")], collapse = " + ")))
# nn <- neuralnet(f, data = train, hidden = c(100,50,30), act.fct = "logistic", linear.output = FALSE, lifesign = "minimal")
# pr.nn <- compute(nn, test_data)
# pr.nn_ <- pr.nn$net.result
# pr.nn_2 <- max.col(pr.nn_)
# mean(pr.nn_2 == test_labels)
# print(confusionMatrix(unlist(pr.nn_2), unlist(test_labels)))

print("Running GA")
library(doParallel)
library(dplyr)
# library(pROC)
registerDoParallel(4)
getDoParWorkers()
ga_ctrl <- gafsControl(functions = rfGA, method = "cv", genParallel=TRUE, allowParallel = TRUE, verbose = TRUE)
set.seed(10)
lev <- c("PS","WS")
system.time(rf_ga3 <- gafs(x = training_data, y = unlist(training_labels), iters = 3, popSize = 20, levels = lev, gafsControl = ga_ctrl))
# Adding GA https://topepo.github.io/caret/feature-selection-using-genetic-algorithms.html
#ga_ctrl <- gafsControl(functions = rfGA, method = "repeatedcv", repeats = 5)

#set.seed(10)
#rf_ga <- gafs(x = training_data, y = unlist(training_labels), iters = 200, gafsControl = ga_ctrl)

train <- cbind(training_set.pca[, 1:pca_number], class.ind(as.factor(training_set.pca$class)))
names(train) <- c(names(training_set.pca)[1:pca_number],"l1","l2","l3", "l4", "l5", "l6")
n <- names(train)
f <- as.formula(paste("l1 + l2 + l3 + l4 + l5 + l6 ~", paste(n[!n %in% c("l1","l2","l3", "l4", "l5", "l6")], collapse = " + ")))
nn <- neuralnet(f, data = train, hidden = c(30, 30, 30), act.fct = "logistic", linear.output = FALSE, lifesign = "minimal")
pr.nn <- compute(nn, test_data.pca)
pr.nn_ <- pr.nn$net.result
pr.nn_2 <- max.col(pr.nn_)
mean(pr.nn_2 == test_labels)
print(confusionMatrix(unlist(pr.nn_2), unlist(test_labels)))
#plot(nn)

#0.9243298
#0.9049881

# train <- cbind(training_set.pca[, 1:pca_number], class.ind(as.factor(training_set.pca$class)))
# names(train) <- c(names(training_set.pca)[1:pca_number],"l1","l2","l3", "l4", "l5", "l6")
# n <- names(train)
# f <- as.formula(paste("l1 + l2 + l3 + l4 + l5 + l6 ~", paste(n[!n %in% c("l1","l2","l3", "l4", "l5", "l6")], collapse = " + ")))
# nn <- neuralnet(f, data = train, hidden = c(9), act.fct = "logistic", linear.output = FALSE, lifesign = "minimal")
# pr.nn <- compute(nn, test_data.pca)
# pr.nn_ <- pr.nn$net.result
# pr.nn_2 <- max.col(pr.nn_)
# acc <- mean(pr.nn_2 == test_labels)
# results <- acc
# print(sprintf("%d: acc: %f", 9, acc))

# for (i in 10:500) {
#     train <- cbind(training_set.pca[, 1:pca_number], class.ind(as.factor(training_set.pca$class)))
#     names(train) <- c(names(training_set.pca)[1:pca_number],"l1","l2","l3", "l4", "l5", "l6")
#     n <- names(train)
#     f <- as.formula(paste("l1 + l2 + l3 + l4 + l5 + l6 ~", paste(n[!n %in% c("l1","l2","l3", "l4", "l5", "l6")], collapse = " + ")))
#     nn <- neuralnet(f, data = train, hidden = c(i), act.fct = "logistic", linear.output = FALSE, lifesign = "minimal")
#    pr.nn <- compute(nn, test_data.pca)
#    pr.nn_ <- pr.nn$net.result
#    pr.nn_2 <- max.col(pr.nn_)
#    acc <- mean(pr.nn_2 == test_labels)
#    results <- rbind(results, acc)
#    print(sprintf("%d: acc: %f", i, acc))
    #print(confusionMatrix(unlist(pr.nn_2), unlist(test_labels)))
# }
# print(results)
