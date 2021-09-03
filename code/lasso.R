nsrcs = 6
N = 240
V = 441
SM_SIZE = 21

x1 = 21
x2 = 21

# TODO: Import X and TC from main.ipynb;
X <- read.table(file ="X1.txt", header=FALSE, sep=" ")
colnames(X) <- NULL
X = as.matrix(X)
dim(X)

TC <- read.table(file ="TC.txt", header=FALSE, sep=" ")
colnames(TC) <- NULL
TC = as.matrix(TC)
dim(TC)


# LASSO Regression;
# From Assignment 1 specs;
rho<- seq(from = 0, to = 1, by = 0.05)

step <- 1/(norm(TC %*% t(TC)) * 1.1)
thr <- rho*N*step
Ao <- matrix(0, nsrcs, 1)
A <- matrix(0, nsrcs, 1)
Alr <- matrix(0, nsrcs, x1*x2)

for (k in 1:(x1*x2)) {
  A <- Ao+step*(t(TC) %*% (X[,k]-(TC%*%Ao)))
  A <- (1/(1+thr)) * (sign(A)*pmax(replicate(nsrcs, 0), abs(A)-thr))
  
  for (i in 1:10) {
    Ao <- A
    A <- Ao+step * (t(TC)%*%(X[,k]-(TC%*%Ao)))
    A <- (1/(1+thr)) * (sign(A)*pmax(replicate(nsrcs, 0), abs(A)-thr))
  }
  Alr[,k] <- A
}

Ao+step*(t(TC) %*% (X[,k]-(TC%*%Ao)))
