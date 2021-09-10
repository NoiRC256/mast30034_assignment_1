library(MASS)

# Read TC;
tc <- read.table(file ="TC.txt", header=FALSE)
TC = as.matrix(tc, 6, 240)

# Do SVD on TC;
svd = svd (TC, 6)
d = svd$d
u = svd$u
v = svd$v

# Save outputs;
#write.matrix(d,'eigen_values.txt',sep = "\t")
#write.matrix(u,'Z.txt',sep = "\t")
#write.matrix(v,'W.txt',sep = "\t")
