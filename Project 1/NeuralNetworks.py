from sklearn.preprocessing import StandardScaler
from sklearn.neural_network import MLPClassifier
from sklearn.metrics import classification_report,confusion_matrix,accuracy_score,f1_score,precision_score,recall_score,mean_squared_error
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
import numpy as np
import sys


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

def NeuralNetwork(hidden_layer_sizes, train_dataset, train_label_dataset, test_dataset, random_state, activation):
    mlp = MLPClassifier(hidden_layer_sizes=hidden_layer_sizes, random_state=random_state, activation=activation)
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

    orig_stdout = sys.stdout
    f = open('results/out4.txt', 'w')

    # 1 Layer - 48 - 0.956905327452
    for i in range(15,20):
        for x in range(0,26):
            print("Running: " + str(i) + "/600 : " + str(x) + "/25")
            predictions_identity = NeuralNetwork(i, train_dataset, train_label_dataset, test_dataset, x, "identity")
            predictions_logistic = NeuralNetwork(i, train_dataset, train_label_dataset, test_dataset, x, "logistic")
            predictions_tanh = NeuralNetwork(i, train_dataset, train_label_dataset, test_dataset, x, "tanh")
            predictions_relu = NeuralNetwork(i, train_dataset, train_label_dataset, test_dataset, x, "relu")
            # print(confusion_matrix(test_label_dataset,predictions))
            # print(classification_report(test_label_dataset,predictions))
            #print((i, 0) + NeuralNetworkResults(test_label_dataset, predictions))
            f.write(str((i, x, "identity") + NeuralNetworkResults(test_label_dataset, predictions_identity)) + '\n')
            f.write(str((i, x, "logistic") + NeuralNetworkResults(test_label_dataset, predictions_logistic)) + '\n')
            f.write(str((i, x, "tanh") + NeuralNetworkResults(test_label_dataset, predictions_tanh)) + '\n')
            f.write(str((i, x, "relu") + NeuralNetworkResults(test_label_dataset, predictions_relu)) + '\n')
            #if accuracy_score(test_label_dataset,predictions) > max_pred:
                #max_pred = accuracy_score(test_label_dataset,predictions)
                #num_node = i

    # print(acc)
    # plt.plot(acc)
    # plt.show()
    # print(max_pred_1)
    # print(num_node_1)

    #   for i in range(len(acc)):
    #    print acc[i]

    f.close()


Main()
