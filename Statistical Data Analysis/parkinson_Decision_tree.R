# Load necessary libraries
library(rpart)
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

# Train the Decision Tree model
dt_model <- rpart(status ~ ., data = train_data, method = "class")

# Print the model summary
print(dt_model)

# Save the plot as a PNG file
png("decision_tree_plot.png", width = 800, height = 600)
plot(dt_model)
text(dt_model, use.n = TRUE, all = TRUE, cex = 0.8)
dev.off()

# Make predictions on the test set
dt_predictions <- predict(dt_model, newdata = test_data, type = "class")

# Evaluate the model performance
dt_conf_matrix <- confusionMatrix(dt_predictions, test_data$status)
print(dt_conf_matrix)


