import math
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA as sklearnPCA
import numpy as np
import os

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

def Mean(values):
    return sum(values)/float(len(values))

def Stdev(values):
    average = Mean(values)
    variance = sum([pow(x-average, 2) for x in values])/float(len(values)-1)
    return math.sqrt(variance)

def Summarize(dataset):
    summaries = [(Mean(attribute), Stdev(attribute)) for attribute in zip(*dataset)]
    return summaries

def SummarizeByClass(dataset, labels):
    separated = SeparateByClass(dataset, labels)
    summaries = {}
    for classValue, instances in separated.iteritems():
        summaries[classValue] = Summarize(instances)
    return summaries

def CalculateProbability(x, mean, stdev):
    exponent = math.exp(-(math.pow(x-mean, 2) / (2*math.pow(stdev,2))))
    return (1 / (math.sqrt(2 * math.pi) * stdev)) * exponent

def CalculateClassProbabilities(summaries, inputVector):
    probabilities = {}
    for label, labelSummaries in summaries.iteritems():
        probabilities[label] = 1
        for i in range(len(labelSummaries)):
            mean, stdev = labelSummaries[i]
            x = inputVector[i]
            probabilities[label] *= CalculateProbability(x, mean, stdev)
            #print(x)
            #print(probabilities)
            #raw_input("Press the <ENTER> key to continue...")
    return probabilities

def Predict(summaries, inputVector):
    probabilities = CalculateClassProbabilities(summaries, inputVector)
    bestLabel, bestProb = None, -1
    for classValue, probability in probabilities.iteritems():
        if bestLabel is None or probability > bestProb:
            bestProb = probability
            bestLabel = classValue
    return bestLabel

def GetPredictions(summaries, testSet):
    predictions = []
    for i in range(len(testSet)):
        result = Predict(summaries, testSet[i])
        predictions.append(result)
    return predictions

def getAccuracy(testLabels, predictions):
    correct = 0
    for x in range(len(testLabels)):
        if testLabels[x] == predictions[x]:
            correct += 1
    return (correct/float(len(testLabels))) * 100.0

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

    #X_std = StandardScaler().fit_transform(train_dataset)
    #X2_std = StandardScaler().fit_transform(test_dataset)
    #sklearn_pca = sklearnPCA(n_components=25)
    #train_dataset = sklearn_pca.fit_transform(X_std)
    #test_dataset = sklearn_pca.fit_transform(X2_std)

    summaries = SummarizeByClass(train_dataset, train_label_dataset)
    #for key in summary.keys():
    #    print('Summary by class value: {0} Values: {1}').format(key, summary[key])
    
    predictions = GetPredictions(summaries, test_dataset)
    accuracy = getAccuracy(test_label_dataset, predictions)
    print('Accuracy: {0}%').format(accuracy)


Main()
