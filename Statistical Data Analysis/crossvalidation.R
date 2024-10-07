setwd("~/Desktop/Parkinson")
dat <- read.csv("parkinson.csv")
y <- dat$status
X <- data.matrix(dat[ , -c(1,18)])
n <- dim(X)[1]
p <- dim(X)[2]
K <- 5
permutation <- sample (1:n, replace = FALSE)
# Split into K groups based on indices
test.index <- split(permutation, rep(1:K, length = n, each = n/K))
X.train <- X[-test.index[[K]], ]
y.train <- y[-test.index[[K]]]
X.test <- X[test.index[[K]], ]
y.test <- y[test.index[[K]]]
data.train <- cbind(y.train, X.train)
colnames(data.train)[1] <- "status"
data.test <- cbind(y.test, X.test)
colnames(data.test)[1] <- "status"
write.csv(data.train, "train.csv", row.names = FALSE)
write.csv(data.test, "test.csv", row.names = FALSE)