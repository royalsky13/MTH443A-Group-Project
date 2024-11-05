setwd("~/Desktop/Parkinson")
parkinson <- read.csv("parkinson.csv")
# Label Encoding


samp <- sample(c(TRUE, FALSE), replace = TRUE, nrow(parkinson), prob = c(0.9,0.1))

# Checking for Imbalance 
prop.table(table(parkinson$status)) #imbalanced
parkinson$status <- as.factor(parkinson$status)  #


# Tried Cross Validation (incorrect prior prob)
n <- nrow(parkinson)
p.vec <- seq(0.1, 0.9, by = 0.1)
test.size <- floor (n/5)
permutation <- sample (1:n, replace = FALSE)
K <- 5
test.index <- split(permutation, rep(1:K, length = n, each = n/K))

CV.error <- numeric (length = length(p.vec))
for (i in 1:length(p.vec))
{
  foo <- 0
  p <- p.vec[i]
  print(p)
  for (k in 1:K)
  {
    test <- parkinson[test.index[[k]], ] 
    train <- parkinson[-test.index[[k]], ]
    
    feature.train <- train[, setdiff(colnames(train), c("status", "name"))] #
    response.train <- train$status #
    feature.test <- test[, setdiff(colnames(test), c("status", "name"))] #
    response.test <- test$status #
    
    train.scale <- scale(feature.train) # outputs matrix (has only feature vectors)
    means <- attr(train.scale, "scaled:center")
    sds <- attr(train.scale, "scaled:scale")
    train.scale <- as.data.frame(train.scale) # need in df format so that lda() works
    
    
    test.scale <- scale(feature.test, center = means, scale =sds)
    test.scale <- as.data.frame(test.scale)
    
    
    # fitted on train
    lda.model <- lda(status~., cbind(train.scale, #
                                     status = response.train), prior = c(1-p,p)) #

    lda.train.predictions <- (lda.model %>% predict(train.scale))$class # on train
    
    
    
    foo <- foo + sum(lda.train.predictions==response.train)
    
  }
  CV.error[i] <- foo/n
}
CV.error
(chosen.p <- p.vec[which.min(CV.error)])






























samp <- sample(c(TRUE, FALSE), replace = TRUE, nrow(iris), prob = c(0.9,0.1)) #

# Checking for Imbalance 
prop.table(table(parkinson$status)) # imbalanced

test <- parkinson[!samp, ] #
train <- parkinson[samp, ] #

feature.train <- train[, setdiff(colnames(train), c("status", "name"))] #
response.train <- train$status #
feature.test <- test[, setdiff(colnames(test), c("status", "name"))] #
response.test <- test$status #

train.scale <- scale(feature.train) # outputs matrix (has only feature vectors)
means <- attr(train.scale, "scaled:center")
sds <- attr(train.scale, "scaled:scale")
train.scale <- as.data.frame(train.scale) # need in df format so that lda() works


test.scale <- scale(feature.test, center = means, scale =sds)
test.scale <- as.data.frame(test.scale)


# fitted on train
lda.model <- lda(status~., cbind(train.scale, #
                                  status = response.train)) #
qda.model <- qda(status~., cbind(train.scale, #
                                 status = response.train)) #

bayes.model <- naiveBayes(status ~ ., data = cbind(train.scale,# 
                                                   status = response.train))#
# predictions
lda.train.predictions <- (lda.model %>% predict(train.scale))$class # on train
lda.test.predictions <- (lda.model %>% predict(test.scale))$class # on test

qda.train.predictions <- (qda.model %>% predict(train.scale))$class # on train
qda.test.predictions <- (qda.model %>% predict(test.scale))$class # on test

bayes.train.predictions <- bayes.model %>% predict(train.scale)
bayes.test.predictions <- bayes.model %>% predict(test.scale)
# Model accuracy of LDA
mean(lda.train.predictions==response.train) # train
mean(lda.test.predictions==response.test) # test
# Apparent Error Rate
mean(lda.train.predictions!=response.train) 
mean(lda.test.predictions!=response.test) 
# Confusion Matrix
table(Predicted = lda.train.predictions, Actual = response.train)
table(Predicted = lda.test.predictions, Actual = response.test)


# Model accuracy of QDA
mean(qda.train.predictions==response.train) #train
mean(qda.test.predictions==response.test) #test
# Apparent Error Rate
mean(qda.train.predictions!=response.train) 
mean(qda.test.predictions!= response.test)
# Confusion Matrix
table(Predicted = qda.train.predictions, Actual = response.train)
table(Predicted = qda.test.predictions, Actual = response.test)



# Model accuracy of Naive Bayes
# Confusion Matrix
table(Predicted = bayes.train.predictions, Actual = response.train)
table(Predicted = bayes.test.predictions, Actual = response.test)

# Model Evaluation
confusionMatrix(table(Predicted = bayes.test.predictions, Actual = response.test))
mean(bayes.test.predictions==response.test) #test
