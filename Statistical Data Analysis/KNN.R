library(ROSE)
library(caret)
library(class)
library(MASS)
library(tidyverse)


# Load and prepare the data
parkinson <- read.csv("parkinson_no_outlier.csv")[, -1]
parkinson$status <- as.factor(parkinson$status) # levels: 0, 1
table(parkinson$status)
# Separate features and target variable
X <- parkinson[, !(colnames(parkinson) %in% c("status"))]
y <- parkinson$status

data_balanced <- ovun.sample(status ~ ., data = parkinson[,-1], 
                             method = "over", N = max(table(y)) * 2)$data
folds <- createFolds(data_balanced$status, k = 5) # Randomization Step
table(data_balanced$status)
# Initialize matrix to store accuracy for each fold
accuracy <- matrix(0, nrow = 5, ncol = 1)
k.vec <- 1:1
for (k in 1:length(k.vec)){
  for (i in 1:5) {
    # Split data into training and testing sets for the i-th fold
    test_indices <- folds[[i]]
    test <- data_balanced[test_indices, ]
    train <- data_balanced[-test_indices, ]
    
    # Split features and response
    feature.train <- train[, colnames(train) != "status"]
    response.train <- train[, colnames(train) == "status"]
    feature.test <- test[, colnames(test) != "status"]
    response.test <- test[, colnames(test) == "status"]
    
    # Scale the features
    train.scale <- scale(feature.train)
    means <- attr(train.scale, "scaled:center")
    sds <- attr(train.scale, "scaled:scale")
    train.scale <- as.data.frame(train.scale)
    
    test.scale <- scale(feature.test, center = means, scale = sds)
    test.scale <- as.data.frame(test.scale)
    
    # Make knn predictions on the test set
    knn.test.predictions <- knn(train = train.scale, 
                                test = test.scale, 
                                cl = train$status, 
                                k = k) 
    
    # Store model accuracies
    accuracy[i,k] <- mean(knn.test.predictions==response.test)
    print(table(Predicted = knn.test.predictions, Actual = response.test ))
  }
}
  
# Print the accuracy for each fold and the average accuracy
print(accuracy*100)
accuracy_means <- colMeans(accuracy) * 100
print(accuracy_means)
plot(x = k.vec, y = 100-accuracy_means, type = "l", col="red", 
     lwd = 2, ylab = "Misclassification Error Rate expressed in (%)",
     xlab = "Number of Neighbors (K)")


