setwd("~/Desktop/Parkinson")

# Read the datasets
dat.train <- read.csv("train.csv")
dat.test <- read.csv("test.csv")

# Setting up our design matrix
X.train <- as.matrix(dat.train[ ,-1]) # 156 X 22
y.train <- dat.train$status
X.test <- dat.test[ ,-1]
y.test <- dat.test$status

## Logistic Regression with all features
# Estimated Beta
beta.train <- matrix (solve(t(scale(X.train)) %*% scale(X.train)) 
                      %*% t(scale(X.train))  %*% y.train, ncol = 1)

# Estimated probabilities of classification
pi.hat <- exp(scale(X.train) %*% beta.train)/
  (1 + exp(scale(X.train) %*% beta.train) )
qi.hat <- 1 - pi.hat
predict.probs.train <- data.frame(pi.hat ,qi.hat)
names(predict.probs.train) <- c("Class 1", "Class 0")

# Choosing for optimal threshold via grid search
threshold.vec <- seq(0, 1, length = 1e3)
sensitivity.vec <- numeric(length = 1e3)
specificity.vec <- numeric(length = 1e3)
J.stat <- numeric(length = 1e3)
for (i in 1:length(threshold.vec))
{
  threshold <- threshold.vec[i]
  predict.y <- ifelse(pi.hat > threshold, 1, 0)
  foo <- data.frame(y.train, predict.y)
  tp <- sum(foo[ ,1] == 1 & foo[ ,2] == 1)
  fn <- sum(foo[ ,1] == 1 & foo[ ,2] == 0)
  fp <- sum(foo[ ,1] == 0 & foo[ ,2] == 1)
  tn <- sum(foo[ ,1 ] == 0 & foo[, 2] == 0)
  sensitivity.vec[i] <- tp/(tp + fn)
  specificity.vec[i] <- 1 - (fp/(fp + tn))
  # Youden’s J Statistic
  J.stat[i] <- sensitivity.vec[i] + specificity.vec[i] - 1
}
optimal.idx <- which.max(J.stat)
optimal.threshold <- threshold.vec[optimal.idx]
cat("Optimal threshold:", optimal.threshold, "\n")


dat <- data.frame(threshold.vec, specificity.vec, sensitivity.vec)
plot( dat[c(2,3)], type="l", col = "red", xlab = "Specificity",
      ylab = "Sensitivity", main = "ROC Curve", xlim = c(0,1), ylim = c(0,1), lwd = 2)
lines(x = specificity.vec, y = specificity.vec, lty = 2, col = "black", lwd = 2)
legend ("bottom", legend = c("ROC Curve","Random Guess Curve"),
        col = c("red","black"), lty = c(1,2), lwd = c(2,2), cex=0.7)
plot( dat[c(2,3)], type ="l", xlab = "Specificity",
      ylab = "Sensitivity", main = "AUC Curve")
polygon(x = c(min(dat[c(2,3)]$specificity.vec), dat[c(2,3)]$specificity.vec,
              max(dat[c(2,3)]$specificity.vec)),
        y = c(0, dat[c(2,3)]$sensitivity.vec, 0), col = "violet")





# Confusion Matrix & Model Accuracy on Train Data
threshold <- optimal.threshold
predict.y <- ifelse(pi.hat > threshold, 1, 0)
foo <- data.frame(y.train, predict.y)
tp <- sum(foo[ ,1] == 1 & foo[ ,2] == 1)
fn <- sum(foo[ ,1] == 1 & foo[ ,2] == 0)
fp <- sum(foo[ ,1] == 0 & foo[ ,2] == 1)
tn <- sum(foo[ ,1 ] == 0 & foo[, 2] == 0)
confusion.matrix <- matrix(c(tp, fn, fp, tn), nrow = 2, ncol = 2, byrow=TRUE)
colnames(confusion.matrix) <- c("Predicted: Class 1", "Predicted: Class 0")
rownames(confusion.matrix) <- c("Actual: Class 1", "Actual: Class 0")
confusion.matrix
(model.accuracy <- round((((tp+tn)/dim(foo[1])[1]) * 100),3) )

# Confusion Matrix & Model Accuracy on Test Data
# Using  same set of linear transformation on Test Data like the Train Data
X.test.scaled <- scale(X.test, center = attr(scale(X.train), "scaled:center"), 
                       scale = attr(scale(X.train), "scaled:scale"))
pi.hat <- exp(X.test.scaled %*% beta.train) / 
  (1 + exp(X.test.scaled %*% beta.train))
qi.hat <- 1 - pi.hat
predict.probs.test <- data.frame(pi.hat ,qi.hat)
names(predict.probs.test) <- c("Class 1", "Class 0")
predict.y <- ifelse(pi.hat > threshold, 1, 0)
foo <- data.frame(y.test, predict.y)
tp <- sum(foo[ ,1] == 1 & foo[ ,2] == 1)
fn <- sum(foo[ ,1] == 1 & foo[ ,2] == 0)
fp <- sum(foo[ ,1] == 0 & foo[ ,2] == 1)
tn <- sum(foo[ ,1 ] == 0 & foo[, 2] == 0)
confusion.matrix <- matrix(c(tp, fn, fp, tn), nrow = 2, ncol = 2, byrow=TRUE)
colnames(confusion.matrix) <- c("Predicted: Class 1", "Predicted: Class 0")
rownames(confusion.matrix) <- c("Actual: Class 1", "Actual: Class 0")
confusion.matrix
(model.accuracy <- round((((tp+tn)/dim(foo[1])[1]) * 100),3) )


importance_df <- data.frame(Feature = colnames(X.train), Coefficient = beta.train)

# Sort the data frame by absolute coefficient values in descending order
importance_df <- importance_df[order(abs(importance_df$Coefficient), decreasing = TRUE), ]

# Print the sorted feature importances
print(importance_df)



## Principal Component Analysis + Logistic Regression
n.pcomponent <- 18
# Perform PCA on X.train
pca.model <- prcomp(X.train, scale. = TRUE)
X.pca.train <- pca.model$x[, 1:n.pcomponent] 
# Estimated Beta
beta.train <- matrix (solve(t(X.pca.train) %*% X.pca.train) 
                      %*% t(X.pca.train) %*% y.train, ncol = 1)

# Estimated probabilities of classification
pi.hat <- exp(X.pca.train %*% beta.train)/(1 + exp(X.pca.train %*% beta.train) )
qi.hat <- 1 - pi.hat
predict.probs.train <- data.frame(pi.hat ,qi.hat)
names(predict.probs.train) <- c("Class 1", "Class 0")

# Choosing for optimal threshold via grid search
threshold.vec <- seq(0, 1, length = 1e3)
sensitivity.vec <- numeric(length = 1e3)
specificity.vec <- numeric(length = 1e3)
J.stat <- numeric(length = 1e3)
for (i in 1:length(threshold.vec))
{
  threshold <- threshold.vec[i]
  predict.y <- ifelse(pi.hat > threshold, 1, 0)
  foo <- data.frame(y.train, predict.y)
  tp <- sum(foo[ ,1] == 1 & foo[ ,2] == 1)
  fn <- sum(foo[ ,1] == 1 & foo[ ,2] == 0)
  fp <- sum(foo[ ,1] == 0 & foo[ ,2] == 1)
  tn <- sum(foo[ ,1 ] == 0 & foo[, 2] == 0)
  sensitivity.vec[i] <- tp/(tp + fn)
  specificity.vec[i] <- 1 - (fp/(fp + tn))
  # Youden’s J Statistic
  J.stat[i] <- sensitivity.vec[i] + specificity.vec[i] - 1
}
optimal.idx <- which.max(J.stat)
optimal.threshold <- threshold.vec[optimal.idx]
cat("Optimal threshold:", optimal.threshold, "\n")

dat <- data.frame(threshold.vec, specificity.vec, sensitivity.vec)
plot( dat[c(2,3)], type="l", col = "red", xlab = "Specificity",
      ylab = "Sensitivity", main = "ROC Curve", xlim = c(0,1), ylim = c(0,1), lwd = 2)
lines(x = specificity.vec, y = specificity.vec, lty = 2, col = "black", lwd = 2)
legend ("bottom", legend = c("ROC Curve","Random Guess Curve"),
        col = c("red","black"), lty = c(1,2), lwd = c(2,2), cex=0.7)
plot( dat[c(2,3)], type ="l", xlab = "Specificity",
      ylab = "Sensitivity", main = "AUC Curve")
polygon(x = c(min(dat[c(2,3)]$specificity.vec), dat[c(2,3)]$specificity.vec,
              max(dat[c(2,3)]$specificity.vec)),
        y = c(0, dat[c(2,3)]$sensitivity.vec, 0), col = "violet")



# Confusion Matrix & Model Accuracy on Train Data
threshold <- optimal.threshold
predict.y <- ifelse(pi.hat > threshold, 1, 0)
foo <- data.frame(y.train, predict.y)
tp <- sum(foo[ ,1] == 1 & foo[ ,2] == 1)
fn <- sum(foo[ ,1] == 1 & foo[ ,2] == 0)
fp <- sum(foo[ ,1] == 0 & foo[ ,2] == 1)
tn <- sum(foo[ ,1 ] == 0 & foo[, 2] == 0)
confusion.matrix <- matrix(c(tp, fn, fp, tn), nrow = 2, ncol = 2, byrow=TRUE)
colnames(confusion.matrix) <- c("Predicted: Class 1", "Predicted: Class 0")
rownames(confusion.matrix) <- c("Actual: Class 1", "Actual: Class 0")
confusion.matrix
(model.accuracy <- round((((tp+tn)/dim(foo[1])[1]) * 100),3) )

# Confusion Matrix & Model Accuracy on Test Data
# Using  same set of linear transformation on Test Data like the Train Data
X.pca.test <- predict(pca.model, newdata = X.test)[, 1:n.pcomponent]
pi.hat <- exp(X.pca.test %*% beta.train)/(1 + exp(X.pca.test %*% beta.train) )
qi.hat <- 1 - pi.hat
predict.probs.test <- data.frame(pi.hat ,qi.hat)
names(predict.probs.test) <- c("Class 1", "Class 0")
predict.y <- ifelse(pi.hat > threshold, 1, 0)
foo <- data.frame(y.test, predict.y)
tp <- sum(foo[ ,1] == 1 & foo[ ,2] == 1)
fn <- sum(foo[ ,1] == 1 & foo[ ,2] == 0)
fp <- sum(foo[ ,1] == 0 & foo[ ,2] == 1)
tn <- sum(foo[ ,1 ] == 0 & foo[, 2] == 0)
confusion.matrix <- matrix(c(tp, fn, fp, tn), nrow = 2, ncol = 2, byrow=TRUE)
colnames(confusion.matrix) <- c("Predicted: Class 1", "Predicted: Class 0")
rownames(confusion.matrix) <- c("Actual: Class 1", "Actual: Class 0")
confusion.matrix
(model.accuracy <- round((((tp+tn)/dim(foo[1])[1]) * 100),3) )


library(GA)
X.train <- scale(X.train)
X.test <- scale(X.test)
# Define the fitness function: Here we minimize the cross-entropy loss
logistic_fitness <- function(coefficients){
  # Compute the linear predictor
  linear.predictor <- X.train %*% coefficients
  
  # Compute the estimated probabilities (logistic function)
  pi.hat <- exp(linear.predictor) / (1 + exp(linear.predictor))
  
  # Binary cross-entropy loss
  cross_entropy <- -sum(y.train * log(pi.hat + 1e-10) + (1 - y.train) * log(1 - pi.hat + 1e-10))
  
  return(-cross_entropy)  # Minimize cross-entropy
}

# Set up GA parameters
GA_model <- ga(
  type = "real-valued",        # We're optimizing real-valued coefficients
  fitness = logistic_fitness,  # Fitness function defined above
  lower = rep(-10, ncol(X.train)),  # Lower bounds for coefficients
  upper = rep(10, ncol(X.train)),   # Upper bounds for coefficients
  popSize = 100,                # Population size
  maxiter = 2000,              # Maximum number of generations
  pmutation = 0.2,             # Mutation probability
  elitism = 5                  # Number of elite individuals retained
)

# Retrieve the best set of coefficients
best_coefficients <- GA_model@solution
best_coefficients <- as.vector(best_coefficients)
# Use the best coefficients for prediction
pi.hat <- exp(X.test %*% best_coefficients) / (1 + exp(X.test %*% best_coefficients))
predict.y <- ifelse(pi.hat > optimal.threshold, 1, 0)

# Evaluate the model on the test set
confusion.matrix <- table(Predicted = predict.y, Actual = y.test)
print(confusion.matrix)

# Calculate test accuracy
test.accuracy <- sum(diag(confusion.matrix)) / sum(confusion.matrix)
print(paste("Test Accuracy:", round(test.accuracy * 100, 3), "%"))