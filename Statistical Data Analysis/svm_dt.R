# Load necessary libraries
library(e1071)  # For SVM
library(rpart)  # For Decision Tree
library(caret)  # For train-test split and evaluation

# Load the dataset
data <- read.csv("concat.csv")

# Ensure 'status' is a factor for classification
data$status <- as.factor(data$status)

# Split the data into training (70%) and testing (30%) sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data$status, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Apply Support Vector Machine (SVM)
svm_model <- svm(status ~ ., data = trainData, kernel = "radial", cost = 1, scale = TRUE)
svm_predictions <- predict(svm_model, testData)

# Evaluate SVM
confusionMatrix(svm_predictions, testData$status, positive = "1")

# Apply Decision Tree
tree_model <- rpart(status ~ ., data = trainData, method = "class")
tree_predictions <- predict(tree_model, testData, type = "class")

# Evaluate Decision Tree
confusionMatrix(tree_predictions, testData$status, positive = "1")

# Optional: Plot the Decision Tree
library(rpart.plot)
rpart.plot(tree_model, main = "Decision Tree")
