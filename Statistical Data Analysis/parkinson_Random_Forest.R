# Load necessary libraries
library(randomForest)
library(caret)


# Load the dataset

data <- read.csv("parkinson_no_outlier.csv")
data <- data[,-1]


# Ensure 'status' is a factor
data$status <- as.factor(data$status)

# Split the dataset into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data$status, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Train the Random Forest model
rf_model <- randomForest(status ~ ., data = train_data, ntree = 500, mtry = 3, importance = TRUE)

# Print the model summary
print(rf_model)

# Save plot as PNG
png("variable_importance.png", width = 800, height = 600)
varImpPlot(rf_model)
dev.off()


# Make predictions on the test set
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model performance
rf_conf_matrix <- confusionMatrix(rf_predictions, test_data$status)
print(rf_conf_matrix)
