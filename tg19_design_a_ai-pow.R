# Load necessary libraries
library(caret)
library(dplyr)
library(ggplot2)

# Define a function to load and preprocess dataset
load_data <- function(file_path) {
  data <- read.csv(file_path)
  data <- data %>% 
    mutate_if(is.factor, as.numeric) %>% 
    select(-c(date, time)) %>% 
    na.omit()
  return(data)
}

# Load dataset
data <- load_data("machine_learning_models_data.csv")

# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$target, p = .8, 
                                 list = FALSE, 
                                 times = 1)
train_set <- data[ trainIndex,]
test_set  <- data[-trainIndex,]

# Train a random forest model
rf_model <- caret::train(target ~ ., data = train_set, 
                        method = "rf", 
                        tuneGrid = data.frame(mtry = 2), 
                        trControl = trainControl(method = "cv", 
                                                 number = 5))

# Analyze model performance using cross-validation
rf_model_cv <- caret::train(target ~ ., data = train_set, 
                           method = "rf", 
                           tuneGrid = data.frame(mtry = 2), 
                           trControl = trainControl(method = "cv", 
                                                    number = 5, 
                                                    savePredictions = "all"))

# Plot cross-validation results
plot(rf_model_cv)

# Predict on test set
rf_pred <- predict(rf_model, newdata = test_set, type = "prob")
rf_pred_class <- ifelse(rf_pred > 0.5, 1, 0)

# Evaluate model performance
conf_mat <- table(test_set$target, rf_pred_class)
print(conf_mat)
accuracy(rf_model, test_set$target, rf_pred_class)