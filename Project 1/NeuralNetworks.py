from sklearn.preprocessing import StandardScaler
from sklearn.neural_network import MLPClassifier
from sklearn.metrics import classification_report,confusion_matrix

def LoadDataset(filename):
    dataset = open(filename, "r").readlines()
    for i in range(len(dataset)):
        dataset[i] = dataset[i].split();
        for x in range(len(dataset[i])):
            dataset[i][x] = float(dataset[i][x])
    return dataset

def LoadLabelDataset(filename):
    dataset = open(filename, "r").readlines()
    for i in range(len(dataset)):
        dataset[i] = int(dataset[i]);
    return dataset

def SeparateByClass(dataset, labels):
    separated = {}
    for i in range(len(labels)):
        if(labels[i] not in separated):
            separated[labels[i]] = []
        separated[labels[i]].append(dataset[i])

    for key in separated.keys():
        print("Seperated instances: {0} Size: {1}").format(key, len(separated[key]))

    return separated

def Main():
    train_filename="/home/erik/programming/r/CSI-5810/Project 1/UCI HAR Dataset/train/X_train.txt"
    train_dataset = LoadDataset(train_filename)
    print("Loaded training data file {0} with {1} rows").format(train_filename, len(train_dataset))

    train_label_filename="/home/erik/programming/r/CSI-5810/Project 1/UCI HAR Dataset/train/y_train.txt"
    train_label_dataset = LoadLabelDataset(train_label_filename)
    print("Loaded training labels data file {0} with {1} rows").format(train_label_filename, len(train_label_dataset))

    test_filename="/home/erik/programming/r/CSI-5810/Project 1/UCI HAR Dataset/test/X_test.txt"
    test_dataset = LoadDataset(test_filename)
    print("Loaded training data file {0} with {1} rows").format(test_filename, len(test_dataset))

    test_label_filename="/home/erik/programming/r/CSI-5810/Project 1/UCI HAR Dataset/test/y_test.txt"
    test_label_dataset = LoadLabelDataset(test_label_filename)
    print("Loaded training labels data file {0} with {1} rows").format(test_label_filename, len(test_label_dataset))

    scaler = StandardScaler()
    scaler.fit(train_dataset)

    train_dataset = scaler.transform(train_dataset)
    test_dataset = scaler.transform(test_dataset)

    mlp = MLPClassifier(hidden_layer_sizes=(30,30,30))
    mlp.fit(train_dataset, train_label_dataset)

    predictions = mlp.predict(test_dataset)
    print(confusion_matrix(test_label_dataset,predictions))
    print(classification_report(test_label_dataset,predictions))

    print(len(mlp.coefs_))
    print(len(mlp.coefs_[0]))
    print(len(mlp.intercepts_[0]))


Main()
