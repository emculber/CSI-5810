from sklearn.preprocessing import StandardScaler
from sklearn.neural_network import MLPClassifier
from sklearn.metrics import classification_report,confusion_matrix,accuracy_score,f1_score,precision_score,recall_score,mean_squared_error
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
import numpy as np


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

def NeuralNetwork(hidden_layer_sizes, train_dataset, train_label_dataset, test_dataset, random_state):
    mlp = MLPClassifier(hidden_layer_sizes=hidden_layer_sizes, random_state=random_state)
    mlp.fit(train_dataset, train_label_dataset)
        
    predictions = mlp.predict(test_dataset)

    return predictions

def NeuralNetworkResults(labels, predictions):
    accuracy = accuracy_score(labels,predictions)
    
    fscoreMicro = f1_score(labels, predictions, average='micro')
    fscoreMacro = f1_score(labels, predictions, average='macro')
    fscoreWeighted = f1_score(labels, predictions, average='weighted')
    
    precisioncoreMicro = precision_score(labels, predictions, average='micro')
    precisionscoreMacro = precision_score(labels, predictions, average='macro')
    precisionscoreWeighted = precision_score(labels, predictions, average='weighted')
    
    recallscoreMicro = recall_score(labels, predictions, average='micro')
    recallscoreMacro = recall_score(labels, predictions, average='macro')
    recallscoreWeighted = recall_score(labels, predictions, average='weighted')
    
    mse = mean_squared_error(labels, predictions)
    
    return accuracy, fscoreMicro, fscoreMacro, fscoreWeighted, recallscoreMicro, recallscoreMacro, recallscoreWeighted, mse


def Main():
    np.random.seed(0)
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
    acc=[]
    max_pred = 0
    num_node = 0
    # 1 Layer - 48 - 0.956905327452
    for i in range(3,100):
        print("Running: " + str(i) + "/100")
        predictions = NeuralNetwork(i, train_dataset, train_label_dataset, test_dataset, 0)
        # print(confusion_matrix(test_label_dataset,predictions))
        # print(classification_report(test_label_dataset,predictions))
        print((i, 0) + NeuralNetworkResults(test_label_dataset, predictions))
        acc.append((i, 0) + NeuralNetworkResults(test_label_dataset, predictions))
        #if accuracy_score(test_label_dataset,predictions) > max_pred:
            #max_pred = accuracy_score(test_label_dataset,predictions)
            #num_node = i

    # print(acc)
    plt.plot(acc_1)
    plt.show()
    print(max_pred_1)
    print(num_node_1)


Main()
