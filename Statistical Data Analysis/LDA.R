# Load necessary libraries
library(MASS)  # For LDA and QDA
library(caret) # For train-test split and confusion matrix

# Load the data
data <- read.csv("concat.csv")


# Ensure 'status' is a factor in the dataset
data$status <- as.factor(data$status)

# Re-split the data to ensure correct factor levels are preserved
set.seed(123)
trainIndex <- createDataPartition(data$status, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Apply LDA
lda_model <- lda(status ~ ., data = trainData)
lda_predictions <- predict(lda_model, testData)
lda_class <- lda_predictions$class

# Explicitly convert 'lda_class' and 'testData$status' to factors
lda_class <- factor(lda_class, levels = levels(trainData$status))
testData$status <- factor(testData$status, levels = levels(trainData$status))

# Evaluate LDA
confusionMatrix(lda_class, testData$status, positive = "1")

