from sklearn.preprocessing import StandardScaler
from sklearn.neural_network import MLPClassifier
from sklearn.metrics import classification_report,confusion_matrix,accuracy_score
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt

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

def pca(train, test, num):
	print "%d-Dim:"%num
	pca = PCA(n_components=num)
	pca.fit(train)
	train = pca.transform(train)
        test = pca.transform(test)

        return train, test

def Main():
    train_filename="UCI HAR Dataset/train/X_train.txt"
    train_dataset = LoadDataset(train_filename)
    print("Loaded training data file {0} with {1} rows").format(train_filename, len(train_dataset))

    train_label_filename="UCI HAR Dataset/train/y_train.txt"
    train_label_dataset = LoadLabelDataset(train_label_filename)
    print("Loaded training labels data file {0} with {1} rows").format(train_label_filename, len(train_label_dataset))

    test_filename="UCI HAR Dataset/test/X_test.txt"
    test_dataset = LoadDataset(test_filename)
    print("Loaded training data file {0} with {1} rows").format(test_filename, len(test_dataset))

    test_label_filename="UCI HAR Dataset/test/y_test.txt"
    test_label_dataset = LoadLabelDataset(test_label_filename)
    print("Loaded training labels data file {0} with {1} rows").format(test_label_filename, len(test_label_dataset))

    # train_dataset, test_dataset = pca(train_dataset, test_dataset, 178)
    acc_1=[]
    acc_2=[]
    acc_3=[]
    max_pred_1 = 0 
    num_node_1 = 0
    max_pred_2 = 0 
    num_node_2 = 0
    max_pred_3 = 0 
    num_node_3 = 0
    for i in range(1,500):
        print("Running: " + str(i) + "/500")
        mlp = MLPClassifier(hidden_layer_sizes=(i), random_state=0)
        mlp.fit(train_dataset, train_label_dataset)

        predictions = mlp.predict(test_dataset)
        # print(confusion_matrix(test_label_dataset,predictions))
        # print(classification_report(test_label_dataset,predictions))
        acc_1.append(accuracy_score(test_label_dataset,predictions))    
        if accuracy_score(test_label_dataset,predictions) > max_pred_1:
            max_pred_1 = accuracy_score(test_label_dataset,predictions)
            num_node_1 = i

    for x in range(1,500):
        print("Running: " + str(i) + "/500")
        for y in range(1,500):
            mlp = MLPClassifier(hidden_layer_sizes=(x, y), random_state=0)
            mlp.fit(train_dataset, train_label_dataset)

            predictions = mlp.predict(test_dataset)
            # print(confusion_matrix(test_label_dataset,predictions))
            # print(classification_report(test_label_dataset,predictions))
            acc_1.append(accuracy_score(test_label_dataset,predictions))    
            if accuracy_score(test_label_dataset,predictions) > max_pred_1:
                max_pred_1 = accuracy_score(test_label_dataset,predictions)
                num_node_1 = i

    for x in range(1,500):
        print("Running: " + str(i) + "/500")
        for y in range(1,500):
            for z in range(1,500):
                mlp = MLPClassifier(hidden_layer_sizes=(x, y, z), random_state=0)
                mlp.fit(train_dataset, train_label_dataset)

                predictions = mlp.predict(test_dataset)
                # print(confusion_matrix(test_label_dataset,predictions))
                # print(classification_report(test_label_dataset,predictions))
                acc_1.append(accuracy_score(test_label_dataset,predictions))    
                if accuracy_score(test_label_dataset,predictions) > max_pred_1:
                    max_pred_1 = accuracy_score(test_label_dataset,predictions)
                    num_node_1 = i
    # print(acc)
    plt.plot(acc_1)
    plt.plot(acc_2)
    plt.plot(acc_3)
    plt.show()
    print(max_pred_1)
    print(num_node_1)
    print(max_pred_2)
    print(num_node_2)
    print(max_pred_3)
    print(num_node_3)


Main()
