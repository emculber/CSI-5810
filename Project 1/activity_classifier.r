results <- read.table("out.txt")
sorted <- results[with(results, order(V1, V2)),]
start <- 1
averaged_identity <- c(start, mean(sorted[sorted$V4=="identity" & sorted$V1==start,5]))
averaged_logistic <- c(start, mean(sorted[sorted$V4=="logistic" & sorted$V1==start,5]))
averaged_tanh <- c(start, mean(sorted[sorted$V4=="tanh" & sorted$V1==start,5]))
averaged_relu <- c(start, mean(sorted[sorted$V4=="relu" & sorted$V1==start,5]))

for(i in (start+1):150) {
  averaged_identity <- rbind(averaged_identity, c(i, mean(sorted[sorted$V4=="identity" & sorted$V1==i,5])))
  averaged_logistic <- rbind(averaged_logistic, c(i, mean(sorted[sorted$V4=="logistic" & sorted$V1==i,5])))
  averaged_tanh <- rbind(averaged_tanh, c(i, mean(sorted[sorted$V4=="tanh" & sorted$V1==i,5])))
  averaged_relu <- rbind(averaged_relu, c(i, mean(sorted[sorted$V4=="relu" & sorted$V1==i,5])))
}

plot(averaged_logistic, col="red", type="l", main="Accuracy given number of hidden nodes", xlab="Number of Hidden Nodes", ylab="Accuracy")
lines(averaged_identity, col="blue")
lines(averaged_tanh, col="green")
lines(averaged_relu, col="orange")

legend("bottomright", legend=c("identity", "logistic", "tanh", "relu"), col=c("blue", "red", "green", "orange"), lty=1:2, cex=1.5)


cm <- matrix(c(492, 0, 4, 0, 0, 0, 25,445,1,0,0,0,4,17,399,0,0,0,0,2,0,442,47,0,0,0,0,29,503,0,0,0,0,0,9,528), nrow=6, ncol=6, byrow=TRUE)
cm <- data.frame(cm)
colnames(cm) <- c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")
rownames(cm) <- c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")
grid.arrange(tableGrob(cm))


cm <- matrix(c(0.94,0.99,0.97,496,
               0.96,0.94,0.95,471,
               0.99,0.95,0.97,420,
               0.94,0.90,0.92,491,
               0.90,0.95,0.92,532,
               1.00,0.98,0.99,537,
               0.95,0.95,0.95,2947), nrow=6, ncol=4, byrow=TRUE)
colnames(cm) <- c("Percision", "Recall", "F1-score", "Support")
rownames(cm) <- c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying", "avg / total")
grid.arrange(tableGrob(cm))

readline(prompt="Press [enter] to continue")




library(nnet)
library(caret)
library(gridExtra)
library(plotly)

# Settings
pca_number <- 3

# Load Data
print("Loading Data")

features <- read.table("./UCI HAR Dataset/features.txt")

training_data <- read.table("./UCI HAR Dataset/train/X_train.txt")
training_labels <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_training <- read.table("./UCI HAR Dataset/train/subject_train.txt")

test_data <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_labels <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

print("Data Loaded")

# Setting up data
print("Setting up feature labels")

colnames(training_data) <- unlist(lapply(features[2], as.character))
colnames(test_data) <- unlist(lapply(features[2], as.character))

pca <- prcomp(training_data, scale=TRUE)

training_labels <- data.frame(class=training_labels[,1])
subject_training <- data.frame(subject=subject_training[,1])
test_labels <- data.frame(class=test_labels[,1])
subject_test <- data.frame(class=subject_test[,1])

training_set <- cbind(training_data, training_labels)
test_set <- cbind(test_data, test_labels)

training_all <- cbind(training_set, subject_training)
test_all <- cbind(test_set, subject_test)

training_data.pca <- data.frame(pca$x)[1:pca_number]
test_data.pca <- data.frame(predict(pca, newdata = test_data))[1:pca_number]
training_set.pca <- cbind(training_data.pca, training_labels)
test_set.pca <- cbind(test_data.pca, test_labels)


t1 <- training_set[training_set$class==1,]
t2 <- training_set[training_set$class==2,]
t3 <- training_set[training_set$class==3,]
t4 <- training_set[training_set$class==4,]
t5 <- training_set[training_set$class==5,]
t6 <- training_set[training_set$class==6,]




print("plot PCA")
#plot_ly(training_data.pca)
plot_ly(training_data.pca, x = ~PC1, y = ~PC2, z = ~PC3)

readline(prompt="Press [enter] to continue")

plot(training_data.pca, pch=21, bg=c("green", "blue", "red", "yellow", "orange", "black")[unclass(training_set.pca$class)])
legend(1, 95, legend=c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying"),
              col=c("green", "blue", "red", "yellow", "orange", "black"), lty=1:2, cex=0.8)


ta1 <- training_all[training_all$class==1,]
ta2 <- training_all[training_all$class==2,]
ta3 <- training_all[training_all$class==3,]
ta4 <- training_all[training_all$class==4,]
ta5 <- training_all[training_all$class==5,]
ta6 <- training_all[training_all$class==6,]

training_set_count_table <- matrix(c(dim(t1)[1], dim(t2)[1], dim(t3)[1], dim(t4)[1], dim(t5)[1], dim(t6)[1]), ncol=6, byrow=TRUE)

subjects <- c()
for (i in 1:6) {
  s <- c()
  for (x in 1:30) {
    s <- rbind(s, dim(training_all[training_all$class==i & training_all$subject==x,])[1])
  }
  subjects <- cbind(subjects, s)
}

training_set_count_table <- rbind(training_set_count_table, subjects)

colnames(training_set_count_table) <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")

rownames(training_set_count_table) <- c("COUNT", factor(1:30))
training_set_count_table <- as.table(training_set_count_table)

# jpeg('activity_count.jpg')
par(mar=c(12,7,4,1))
barplot(training_set_count_table[1,], main="Training Set Count", xlab="Activity Label", ylab="Activity Count", las=2, col=c("blue"))
#dev.off()
# readline(prompt="Press [enter] to continue")
# barplot(training_set_count_table[-1,], main="Training Set Counts", xlab="Activity Label", ylab="Activity Count", legend = rownames(training_set_count_table), beside=TRUE)
print(table(training_set_count_table[-1,]))
colfunc <- colorRampPalette(c("blue", "red"))
barplot(
        training_set_count_table[-1,],
        main="Training Set Counts",
        xlab="Activity Label", 
        ylab="Activity Count",
        beside=TRUE,
        xlim=c(0, ncol(training_set_count_table[-1,]) * 32),
        col=colfunc(30),
        legend.text=TRUE,
        args.legend=list(
                         x=(ncol(training_set_count_table[-1,]) * 32) + 3,
                         #y=max(colSums(training_set_count_table[-1,])),
                         bty = "n"
                         )
        )
# readline(prompt="Press [enter] to continue")
ss <- tableGrob(training_set_count_table[-1,])
grid.arrange(ss)

print(sprintf("Class 1 dim: (%d, %d)", dim(t1)[1], dim(t1)[2]))
print(sprintf("Class 2 dim: (%d, %d)", dim(t2)[1], dim(t2)[2]))
print(sprintf("Class 3 dim: (%d, %d)", dim(t3)[1], dim(t3)[2]))
print(sprintf("Class 4 dim: (%d, %d)", dim(t4)[1], dim(t4)[2]))
print(sprintf("Class 5 dim: (%d, %d)", dim(t5)[1], dim(t5)[2]))
print(sprintf("Class 6 dim: (%d, %d)", dim(t6)[1], dim(t6)[2]))

pca.summary <- summary(pca)
variance <- pca.summary$importance[2,]
# for (i in 561:1) {
#     print(sprintf("%d: %f", i, sum(pca.summary$importance[2,1:i])))
# }

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


readline(prompt="Press [enter] to continue")
# library(neuralnet) 
# n <- names(training_set)
# a <- as.formula(paste("class ~", paste(n[!n %in% "class"], collapse = " + ")))
# nn <- neuralnet(f,data=training_set,hidden=c(5,3),linear.output=T)
# plot(nn)
# pr.nn <- compute(nn,test_data)

# set.seed(10)

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

# library(doParallel)
# library(dplyr)
# library(pROC)
#  registerDoParallel(4)
# getDoParWorkers()
# ga_ctrl <- gafsControl(functions = rfGA, method = "cv", genParallel=TRUE, allowParallel = TRUE, verbose = TRUE)
# set.seed(10)
# lev <- c("PS","WS")
# system.time(rf_ga3 <- gafs(x = training_data, y = unlist(training_labels), iters = 3, popSize = 20, levels = lev, gafsControl = ga_ctrl))
# Adding GA https://topepo.github.io/caret/feature-selection-using-genetic-algorithms.html
#ga_ctrl <- gafsControl(functions = rfGA, method = "repeatedcv", repeats = 5)

#set.seed(10)
#rf_ga <- gafs(x = training_data, y = unlist(training_labels), iters = 200, gafsControl = ga_ctrl)

# train <- cbind(training_set.pca[, 1:pca_number], class.ind(as.factor(training_set.pca$class)))
# names(train) <- c(names(training_set.pca)[1:pca_number],"l1","l2","l3", "l4", "l5", "l6")
# n <- names(train)
# f <- as.formula(paste("l1 + l2 + l3 + l4 + l5 + l6 ~", paste(n[!n %in% c("l1","l2","l3", "l4", "l5", "l6")], collapse = " + ")))
# nn <- neuralnet(f, data = train, hidden = c(50, 26), act.fct = "logistic", linear.output = FALSE, lifesign = "minimal")
# pr.nn <- compute(nn, test_data.pca)
# pr.nn_ <- pr.nn$net.result
# pr.nn_2 <- max.col(pr.nn_)
# mean(pr.nn_2 == test_labels)
# print(confusionMatrix(unlist(pr.nn_2), unlist(test_labels)))
# plot(nn)

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

