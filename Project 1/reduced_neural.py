import urllib
import numpy as np
from sklearn.decomposition import PCA
import pandas as pd

import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
from sklearn import neighbors, datasets

from sklearn.preprocessing import StandardScaler
from sklearn.neural_network import MLPClassifier
from sklearn.metrics import classification_report,confusion_matrix



def KNN(path_i, nn):
	
	n_neighbors = nn
	
	X = np.array(c1+c2)
	y = [0, 0, 0, 1, 1, 1, 1]

	h = .02
	knn=neighbors.KNeighborsClassifier()
	knn.fit(X, Y)

	# x_min, x_max = X[:,0].min() - .5, X[:,0].max() + .5
	# y_min, y_max = X[:,1].min() - .5, X[:,1].max() + .5
	# xx, yy = np.meshgrid(np.arange(x_min, x_max, h), np.arange(y_min, y_max, h))
	# Z = knn.predict(np.c_[xx.ravel(), yy.ravel()])




def generate_csv(path1, path2):
	f = open(path1, "r")
	fo = open(path2, "w")
	fo.write("features")
	for i in range(561):
		fo.write(",%s"%(i+1))
	fo.write("\n")

	count = 0
	for line in f:
		tmp_line = ""
		eles = line[:-1].split(" ")
		count += 1
		tmp_line += "%d"%count
		for ele in eles:
			if not ele=="":
				tmp_line += ",%s"%ele
		tmp_line += "\n"
		fo.write("%s"%tmp_line)

	f.close()
	fo.close()



def feature_PCA(path_i, path_o, num):
	tb_existing_url_csv = None
	local_tb_existing_file = path_i
	    
	existing_df = pd.read_csv(
	    local_tb_existing_file, 
	    index_col = 0, 
	    thousands  = ',')
	existing_df.index.names = ['feature']
	existing_df.columns.names = ['item']

	existing_df.head()

	print "Original data:"
	print existing_df

	print "\n--------------"
	print "%d-Dim:"%num
	pca = PCA(n_components=num)
	pca.fit(existing_df)
	existing_2d = pca.transform(existing_df)
	print existing_2d

	fo = open(path_o, "w")
	fo.write("features")
	for i in range(num):
		fo.write(",%s"%(i+1))
	fo.write("\n")
	count = 0
	for line in existing_2d:
		count += 1
		fo.write("%d"%count)
		for ele in line:
			# print ele
			fo.write(",%f"%ele)
		# print "\n"
		fo.write("\n")
	fo.close()

def load_data(path1, path2, path3, path4):
	f = open(path1, "r")
	count = 0
	train_data_x = []
	for line in f:
		if count==0:
			count += 1
			continue
		eles = line[:-1].split(",")[1:]
		f_eles = []
		for ele in eles:
			f_eles.append(float(ele))
		train_data_x.append(f_eles)
		count += 1
	f.close()

	f = open(path2, "r")
	train_data_y = []
	for line in f:
		ele = line[:-1].split(",")[0]
		train_data_y.append(int(ele))
	f.close()


	f = open(path3, "r")
	count = 0
	test_data_x = []
	for line in f:
		if count==0:
			count += 1
			continue
		eles = line[:-1].split(",")[1:]
		f_eles = []
		for ele in eles:
			f_eles.append(float(ele))
		test_data_x.append(f_eles)
		count += 1
	f.close()

	f = open(path4, "r")
	test_data_y = []
	for line in f:
		ele = line[:-1].split(",")[0]
		test_data_y.append(int(ele))
	f.close()


	print train_data_y
	print train_data_x[0]

	return (train_data_x, train_data_y, test_data_x, test_data_y)


def neu(train_dataset, train_label_dataset,  test_dataset, test_label_dataset):
	scaler = StandardScaler()
	scaler.fit(train_dataset)

	train_dataset = scaler.transform(train_dataset)
	test_dataset = scaler.transform(test_dataset)

	mlp = MLPClassifier(hidden_layer_sizes=(400,400,300,200,100))
	mlp.fit(train_dataset, train_label_dataset)

	predictions = mlp.predict(test_dataset)
	print(confusion_matrix(test_label_dataset,predictions))
	print(classification_report(test_label_dataset,predictions))

	print(len(mlp.coefs_))
	print(len(mlp.coefs_[0]))
	print(len(mlp.intercepts_[0]))




num = 10
path1 = "X_train.txt"
path1_test = "X_test.txt"
path2 = "features.csv"
path2_test = "features_test.csv"

path_y1 = "y_train.txt"
path_y2 = "y_test.txt"
path3 = "feature_%d.csv"%num
path4 = "test_%d.csv"%num

generate_csv(path1, path2)
generate_csv(path1_test, path2_test)

if num>0 and num<561:
	feature_PCA(path2, path3, num)
	feature_PCA(path2_test, path4, num)
	train_d,train_l, test_d, test_l = load_data(path3, path_y1, path4, path_y2)
	neu(train_d,train_l, test_d, test_l)


elif num==561:
	## PCA
	train_d,train_l, test_d, test_l = load_data(path2, path_y1, path2_test, path_y2)
	print len(train_d), len(train_d[0]), len(train_l)
	neu(train_d,train_l, test_d, test_l)


else:
	print "input a PCA dim in range of [1,561]"


