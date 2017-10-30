def generate_csv(path1, path2):
	f = open(path1, "r")
	fo = open(path2, "w")
	fo.write("features")
	for i in range(561):
		fo.write(",%s"%(i+1))
	fo.write("\n")

	count = 0
	
	for line in f:
		lie = 0
		tmp_line = ""
		eles = line.split(" ")
		count += 1
		tmp_line += "%d"%count
		for ele in eles:
			if not ele=="":
				tmp_line += ",%s"%ele
				lie += 1
		tmp_line += "\n"
		fo.write("%s"%tmp_line)

	print "\n%dx%d\n"%(count, lie)
	f.close()
	fo.close()
generate_csv("X_train.txt","features.csv")
