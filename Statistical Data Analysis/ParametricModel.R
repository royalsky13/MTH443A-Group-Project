library(caret)
library(MASS)
library(tidyverse)
library(e1071)

# Load and prepare the data
parkinson <- read.csv("no_outlier_data.csv")[, -1]
parkinson$status <- as.factor(parkinson$status) # levels: 0, 1

# Save scale values for the holdout set
data.scale <- scale(parkinson[, -c(17)])
means <- attr(data.scale, "scaled:center")
sds <- attr(data.scale, "scaled:scale")


# Separate features and target variable
X <- parkinson[, !(colnames(parkinson) %in% c("status"))]
y <- parkinson$status

data_balanced <- ovun.sample(status ~ ., data = parkinson[,-1], 
                             method = "over", N = max(table(y)) * 2)$data
folds <- createFolds(data_balanced$status, k = 5) # Randomization Step


# Define Bayes Model functions
kde.prior.estimate <- function(x.vec) {
  group0.x.vec <- 1
  group1.x.vec <- 1
  for (i in 1:length(x.vec)) {
    group0.x.vec <- group0.x.vec * density(group0.train.scale[[i]], from = x.vec[i], to = x.vec[i], n = 1)$y
    group1.x.vec <- group1.x.vec * density(group1.train.scale[[i]], from = x.vec[i], to = x.vec[i], n = 1)$y
  }
  
  return(as.vector(prior * c(group0.x.vec, group1.x.vec))) # Apply priors
}

predict.fun <- function(feature.mat) {
  n <- nrow(feature.mat)
  y.predict <- c()
  for (i in 1:n) {
    x.vec <- unlist(unname(feature.mat[i, ]))
    y.predict <- c(y.predict, which.max(kde.prior.estimate(x.vec)) - 1)
  }
  return(y.predict)
}

# Initialize matrix to store accuracy for each fold
accuracy <- matrix(0, nrow = 3, ncol = 5)

# Estimate prior from training data
prior <- as.vector(prop.table(table(data_balanced$status)))

# Loop over models
model <- 1:3
for (m in model) {
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
    
    # Define new variables for Bayes Model
    group0.train <- train[train$status == 0, !(colnames(train) == "status")]
    group1.train <- train[train$status == 1, !(colnames(train) == "status")]
    group0.train.scale <- as.data.frame(scale(group0.train, center = means, scale = sds))
    group1.train.scale <- as.data.frame(scale(group1.train, center = means, scale = sds))
    
    # Train LDA and QDA models on training data
    lda.model <- lda(status ~ ., data = cbind(train.scale, status = response.train), 
                     prior = prior)
    qda.model <- qda(status ~ ., data = cbind(train.scale, status = response.train), 
                     prior = prior)
    
    # Make predictions for each model on the test set
    lda.test.predictions <- predict(lda.model, test.scale)$class
    qda.test.predictions <- predict(qda.model, test.scale)$class
    bayes.test.predictions <- predict.fun(test.scale) # Bayes model predictions
    
    # Store model accuracies
    if (m == 1) {
      accuracy[m, i] <- mean(lda.test.predictions == response.test)
      print(paste("LDA Fold",i))
      print(table(Predicted = lda.test.predictions, Actual = response.test ))
    } else if (m == 2) {
      accuracy[m, i] <- mean(qda.test.predictions == response.test)
      print(paste("QDA Fold",i))
      print(table(Predicted = qda.test.predictions, Actual = response.test ))
    } else {
      accuracy[m, i] <- mean(bayes.test.predictions == response.test)
      print(paste("Bayes Fold",i))
      print(table(Predicted = bayes.test.predictions, Actual = response.test ))
    }
  }
}

# Print the accuracy for each fold and the average accuracy
print(accuracy*100)
accuracy_means <- rowMeans(accuracy) * 100
print(accuracy_means)

###########################################################################
# Choosing priors by grid search
library(ROSE)
library(caret)
library(class)
library(MASS)
library(tidyverse)
library(e1071)

# Load and prepare the data
parkinson <- read.csv("no_outlier_data.csv")[, -1]
parkinson$status <- as.factor(parkinson$status) # levels: 0, 1
# Separate features and target variable
X <- parkinson[, !(colnames(parkinson) %in% c("status"))]
y <- parkinson$status

data_balanced <- ovun.sample(status ~ ., data = parkinson[,-1], 
                             method = "over", N = max(table(y)) * 2)$data
folds <- createFolds(data_balanced$status, k = 5) # Randomization Step

# Define Bayes Model functions
kde.prior.estimate <- function(x.vec, prior) {
  group0.x.vec <- 1
  group1.x.vec <- 1
  for (i in 1:length(x.vec)) {
    group0.x.vec <- group0.x.vec * density(group0.train.scale[[i]], from = x.vec[i], to = x.vec[i], n = 1)$y
    group1.x.vec <- group1.x.vec * density(group1.train.scale[[i]], from = x.vec[i], to = x.vec[i], n = 1)$y
  }
  
  return(as.vector(prior * c(group0.x.vec, group1.x.vec))) # Apply priors
}

predict.fun <- function(feature.mat) {
  n <- nrow(feature.mat)
  y.predict <- c()
  for (i in 1:n) {
    x.vec <- unlist(unname(feature.mat[i, ]))
    y.predict <- c(y.predict, which.max(kde.prior.estimate(x.vec, prior)) - 1)
  }
  return(y.predict)
}

# Initialize matrix to store accuracy for each fold
accuracy <- array(0, dim=c(3, 5, 10))
prior.vec <- seq(0.01, 0.98, length.out = 10)
model <- 1:3
# Loop over models
for (p in 1:length(prior.vec))
{
  print(p)
  prior <- c(1-prior.vec[p],prior.vec[p])
  for (m in model) {
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
      
      # Define new variables for Bayes Model
      group0.train <- train[train$status == 0, !(colnames(train) == "status")]
      group1.train <- train[train$status == 1, !(colnames(train) == "status")]
      group0.train.scale <- as.data.frame(scale(group0.train, center = means, scale = sds))
      group1.train.scale <- as.data.frame(scale(group1.train, center = means, scale = sds))
      
      # Train LDA and QDA models on training data
      lda.model <- lda(status ~ ., data = cbind(train.scale, status = response.train), 
                       prior = prior)
      qda.model <- qda(status ~ ., data = cbind(train.scale, status = response.train), 
                       prior = prior)
      
      # Make predictions for each model on the test set
      lda.test.predictions <- predict(lda.model, test.scale)$class
      qda.test.predictions <- predict(qda.model, test.scale)$class
      bayes.test.predictions <- predict.fun(test.scale) # Bayes model predictions
      
      # Store model accuracies
      if (m == 1) {
        accuracy[m, i, p] <- mean(lda.test.predictions == response.test)
      } else if (m == 2) {
        accuracy[m, i, p] <- mean(qda.test.predictions == response.test)
      } else {
        accuracy[m, i, p] <- mean(bayes.test.predictions == response.test)
      }
    }
  }
}

accuracy <- accuracy * 100
plot(prior.vec, 100 - apply(accuracy, c(1, 3), mean)[1,], type = "l", col = "red", 
     xlab = "Prior probability", 
     ylab = "Misclassification Error Rate expressed in percentage (%)", 
     ylim = c(0,100), lwd = 2)
lines(prior.vec, 100 - apply(accuracy, c(1, 3), mean)[2,], type = "l", col = "blue", lwd = 2)
lines(prior.vec, 100 - apply(accuracy, c(1, 3), mean)[3,], type = "l", col = "green", lwd = 2)

# Add a legend
legend("topright", legend = c("LDA", "QDA", "Naive Bayes"), 
       col = c("red", "blue", "green"), lty = 1, lwd = 2)

##########################################################################
library(caret)
library(MASS)
library(tidyverse)
library(e1071)

# Load and prepare the data
parkinson <- read.csv("holdout.csv")[, -1]
parkinson$status <- as.factor(parkinson$status) # levels: 0, 1

# Separate features and target variable
X <- parkinson[, !(colnames(parkinson) %in% c("status"))]
y <- parkinson$status

data_balanced <- ovun.sample(status ~ ., data = parkinson[,-1], 
                             method = "over", N = max(table(y)) * 2)$data
# Scale values
means <- c(1.536656e+02, 1.937716e+02, 1.155311e+02, 5.677892e-03, 4.085542e-05, 2.963554e-03, 
           3.149096e-03, 8.891506e-03, 2.848524e-02, 2.672530e-01, 1.501512e-02, 1.720596e-02, 
           2.291446e-02, 4.504560e-02, 2.041970e-02, 2.215804e+01, 4.984123e-01, 7.172807e-01, 
           -5.730320e+00, 2.248932e-01, 2.365124e+00, 2.029419e-01)
sds <- c(4.141671e+01, 8.445930e+01, 4.295171e+01, 3.353257e-03, 2.724441e-05, 1.989284e-03, 
         1.932015e-03, 5.966776e-03, 1.597303e-02, 1.580951e-01, 8.696783e-03, 1.041497e-02, 
         1.357052e-02, 2.608949e-02, 2.544993e-02, 4.111064e+00, 1.046986e-01, 5.637857e-02, 
         1.061771e+00, 8.471568e-02, 3.739626e-01, 8.592111e-02)

holdout.feature <- data_balanced [, colnames(train) != "status"]
holdout.response <- data_balanced [, colnames(train) == "status"]

holdout.feature <- scale(holdout.feature, center = means, scale =sds)
holdout.feature <- as.data.frame(holdout.feature)


# Define Bayes Model functions
kde.prior.estimate <- function(x.vec) {
  group0.x.vec <- 1
  group1.x.vec <- 1
  for (i in 1:length(x.vec)) {
    group0.x.vec <- group0.x.vec * density(group0.train.scale[[i]], from = x.vec[i], to = x.vec[i], n = 1)$y
    group1.x.vec <- group1.x.vec * density(group1.train.scale[[i]], from = x.vec[i], to = x.vec[i], n = 1)$y
  }
  
  return(as.vector(prior * c(group0.x.vec, group1.x.vec))) # Apply priors
}

predict.fun <- function(feature.mat) {
  n <- nrow(feature.mat)
  y.predict <- c()
  for (i in 1:n) {
    x.vec <- unlist(unname(feature.mat[i, ]))
    y.predict <- c(y.predict, which.max(kde.prior.estimate(x.vec)) - 1)
  }
  return(y.predict)
}

# Initialize matrix to store accuracy for each fold
accuracy <- matrix(0, nrow = 3, ncol = 1)

# Estimate prior from training data
prior <- c(0.5,0.5)

# Loop over models
model <- 1:3
for (m in model) {
    
    # Define new variables for Bayes Model
    group0.train <- holdout.feature[train$status == 0, !(colnames(train) == "status")]
    group1.train <- train[train$status == 1, !(colnames(train) == "status")]
    group0.train.scale <- as.data.frame(scale(group0.train, center = means, scale = sds))
    group1.train.scale <- as.data.frame(scale(group1.train, center = means, scale = sds))
    
    # Train LDA and QDA models on training data
    lda.model <- lda(status ~ ., data = cbind(train.scale, status = response.test), 
                     prior = prior)
    qda.model <- qda(status ~ ., data = cbind(train.scale, status = response.test), 
                     prior = prior)
    
    # Make predictions for each model on the test set
    lda.test.predictions <- predict(lda.model, test.scale)$class
    qda.test.predictions <- predict(qda.model, test.scale)$class
    bayes.test.predictions <- predict.fun(test.scale) # Bayes model predictions
    
    # Store model accuracies
    if (m == 1) {
      accuracy[m, 1] <- mean(lda.test.predictions == response.test)
      print(table(Predicted = lda.test.predictions, Actual = response.test ))
    } else if (m == 2) {
      accuracy[m, 1] <- mean(qda.test.predictions == response.test)
      print(table(Predicted = qda.test.predictions, Actual = response.test ))
    } else {
      accuracy[m, 1] <- mean(bayes.test.predictions == response.test)
      print(table(Predicted = bayes.test.predictions, Actual = response.test ))
    }
  }

# Print the accuracy for each fold and the average accuracy
print(accuracy*100)
accuracy_means <- rowMeans(accuracy) * 100
print(accuracy_means)
