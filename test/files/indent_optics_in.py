def fact(x):
	ret = 1
	if x >= 1:
		for i in range(1,x+1):
			ret = ret * i
	return ret
