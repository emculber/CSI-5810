training_data <- read.table("./UCI HAR Dataset/train/X_train.txt")
training_labels <- read.table("./UCI HAR Dataset/train/y_train.txt")

test_data <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_labels <- read.table("./UCI HAR Dataset/test/y_test.txt")
print(dim(training_data))
print(dim(training_labels))
