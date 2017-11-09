from sklearn.preprocessing import StandardScaler
from sklearn.neural_network import MLPClassifier
from sklearn.metrics import classification_report,confusion_matrix,accuracy_score,f1_score,precision_score,recall_score,mean_squared_error
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
import numpy as np
import sys
import multiprocessing as mp


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

def OneTest(q, train_dataset, train_label_dataset, test_dataset, test_label_dataset, i, x, pca_num):

    print("Running: " + str(i) + "/600 : " + str(x) + "/25 : " + str(pca_num) + "/561")

    if pca_num != 561:
        train_dataset, test_dataset = pca(train_dataset, test_dataset, pca_num)

    predictions_identity = NeuralNetwork(i, train_dataset, train_label_dataset, test_dataset, x, "identity")
    predictions_logistic = NeuralNetwork(i, train_dataset, train_label_dataset, test_dataset, x, "logistic")
    predictions_tanh = NeuralNetwork(i, train_dataset, train_label_dataset, test_dataset, x, "tanh")
    predictions_relu = NeuralNetwork(i, train_dataset, train_label_dataset, test_dataset, x, "relu")

    output = ""
    output += (str((i, x, pca_num, "identity") + NeuralNetworkResults(test_label_dataset, predictions_identity)) + '\n')
    output += (str((i, x, pca_num, "logistic") + NeuralNetworkResults(test_label_dataset, predictions_logistic)) + '\n')
    output += (str((i, x, pca_num, "tanh") + NeuralNetworkResults(test_label_dataset, predictions_tanh)) + '\n')
    output += (str((i, x, pca_num, "relu") + NeuralNetworkResults(test_label_dataset, predictions_relu)) + '\n')
    q.put(output)
    return str(output)

def listener(q):
    '''listens for messages on the q, writes to file. '''

    f = open('out.txt', 'wb') 
    while 1:
        m = q.get()
        if m == 'kill':
            break
        f.write(str(m) + '\n')
        f.flush()
    f.close()


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

    #f = open('out.txt', 'w')
    
    results = []

    pool = mp.Pool(processes=mp.cpu_count() + 2)
    #print(mp.cpu_count())
    manager = mp.Manager()
    q = manager.Queue()
    watcher = pool.apply_async(listener, (q,))

    # 1 Layer - 48 - 0.956905327452

    #for pca_num in range(1,562):
    #    results.append(pool.apply_async(OneTest, args=(train_dataset, train_label_dataset, test_dataset, test_label_dataset, i, 0, pca_num)))

    for i in range(1,601):
        for x in range(0,25):
            results.append(pool.apply_async(OneTest, args=(q, train_dataset, train_label_dataset, test_dataset, test_label_dataset, i, x, 561)))
    output = [p.get() for p in results]
    #for i in range(len(output)):
    #    print(output[i])
    #    f.write(output[i])
    #f.close()

    q.put('kill')
    pool.close()


Main()
