# Load necessary libraries
library(e1071)
library(caret)

# Load the dataset
# Update with the actual file path
data <- read.csv("parkinson_no_outlier.csv")
data <- data[,-1]


# View the structure of the dataset
str(data)

# Ensure 'status' is a factor
data$status <- as.factor(data$status)

# Split the dataset into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data$status, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Train the SVM model
svm_model <- svm(status ~ ., data = train_data, kernel = "radial", cost = 1, gamma = 0.1)

# Print the model summary
summary(svm_model)

# Make predictions on the test set
predictions <- predict(svm_model, newdata = test_data)

# Evaluate the model performance
conf_matrix <- confusionMatrix(predictions, test_data$status)
print(conf_matrix)
