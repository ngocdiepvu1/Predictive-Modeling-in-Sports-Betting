# Clear workspace
rm(list = ls())
library(GGally)
library(car)
library(corrplot)
library(randomForest)
library(caret)
library(dplyr)

df <- read.csv("C:/Users/Yousef/OneDrive - University of Miami/Master/7-Spring24/Project/data/soccerdata.csv")

dim(df)

str(df)



df$Date <- as.Date(df$Date, format="%m/%d/%Y")
df$FTR <-as.factor(df$FTR)

# Adding the new column 'Result'
df$Result <- ifelse(df$FTR == 0, "Won", "Lost")

# Convert 'HomeWin' to a factor
df$Result <- factor(df$Result)

summary(df)

# Calculate min and max dates for each division
result <- df %>%
  group_by(Div) %>%
  summarise(
    Min_Date = min(Date, na.rm = TRUE),
    Max_Date = max(Date, na.rm = TRUE)
  )

# View the result
print(result)


# Counting occurrences and calculating percentages
result <- df %>%
  count(FTR) %>%
  mutate(Percentage = n / sum(n) * 100)

# Renaming the levels for clarity
result$FTR <- factor(result$FTR, levels = c(0, 1, 2), labels = c("Win", "Draw", "Loss"))

# Viewing the result
print(result)

# Drop 'HomeTeam' and 'AwayTeam' using subset
df <- subset(df, select = -c(Div, HomeTeam, AwayTeam))


str(df)

correlations_clean <- cor(df[, sapply(df, is.numeric)])
corrplot(correlations_clean, method = "circle")




# Correcting the split code to use the correct column name 'Date'
train_data <- df[df$Date < as.Date("2023-07-01"), ]
test_data <- df[df$Date >= as.Date("2023-07-01"), ]

# Output the size of each dataset
cat("Size of Training Data:", nrow(train_data), "\n")
cat("Size of Testing Data:", nrow(test_data), "\n")

# Assuming the response variable is 'Result' and it's already a factor
# We'll exclude 'HomeTeam' and 'AwayTeam' from the model
train_model <- glm(Result ~ . -Date -FTR, data=train_data, family=binomial(link="logit"))
# Summary of the model to see coefficients and goodness of fit
summary(train_model)

# Predict on the test data
predictions <- predict(train_model, test_data, type = "response")
test_data$predicted_result <- ifelse(predictions > 0.5, "Won", "Lost")


# Predicted and actual results
predicted <- test_data$predicted_result
actual <- test_data$Result

# Create a confusion matrix and calculate accuracy
conf_matrix <- confusionMatrix(as.factor(predicted), as.factor(actual))

# Print the confusion matrix table
print(conf_matrix$table)

# Calculate accuracy
accuracy <- conf_matrix$overall['Accuracy']
cat("Accuracy:", accuracy, "\n")

# Calculate AUC
predictions_prob <- predict(train_model, test_data, type = "response")  # get probabilities
roc_curve <- roc(actual, predictions_prob, plot = TRUE)
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")

# F1 Score, Precision, and Recall
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Precision:", precision, "\nRecall:", recall, "\nF1 Score:", f1_score, "\n")

# Define control using 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

# Train the logistic regression model using cross-validation with interaction terms
model_cv <- train(Result ~ .-Date, 
                  data = train_data, 
                  method = "glm", 
                  family = binomial(link = "logit"), 
                  trControl = train_control,
                  metric = "ROC")

# Print out the results from cross-validation
print(model_cv)



# Train the model
ftr_model <- randomForest(Result ~ .-Date-FTR, data = train_data, promaxmity = TRUE)
ftr_model


# Plot Variable Importance
importance <- importance(ftr_model)
varImpPlot(ftr_model)

# Make predictions
predictions <- predict(ftr_model, test_data)

# It's crucial that both predictions and actual outcomes have the same levels for accurate comparison
levels(predictions) <- levels(test_data$Result)

# Calculate Accuracy
accuracy <- mean(predictions == test_data$Result)
cat("Accuracy:", accuracy)


# Confusion Matrix
confusionMatrix <- confusionMatrix(predictions, test_data$Result)
print(confusionMatrix)


# Manually calculating precision and recall
cm_table <- confusionMatrix$table
precision_manual <- diag(cm_table) / rowSums(cm_table)
recall_manual <- diag(cm_table) / colSums(cm_table)

# Calculating F1 scores manually
f1_scores_manual <- 2 * (precision_manual * recall_manual) / (precision_manual + recall_manual)
macro_f1_manual <- mean(f1_scores_manual, na.rm = TRUE)

cat("Manual Macro F1 Score:", macro_f1_manual, "\n")


# Load pROC if not already loaded
library(pROC)

# Assuming binary classification (adjust if it's not)
roc_response <- roc(test_data$Result, as.numeric(predictions))
plot(roc_response)
cat("AUC:", auc(roc_response))



# Define control using a 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Train the model using cv
model_cv <- train(Result ~ .-Date, data = train_data, method = "rf", trControl = train_control, ntree = 100)

# Print out the results
print(model_cv)


set.seed(12)  # for reproducibility


# Fit the model on the training data
train_model <- glm(Result ~  AvgH + AvgA, family = binomial(link = "logit"), data = train_data)
train_model

# Predict on the test data
predictions <- predict(train_model, test_data, type = "response")
test_data$predicted_result <- ifelse(predictions > 0.5, "Won", "Lost")


# Predicted and actual results
predicted <- test_data$predicted_result
actual <- test_data$Result

# Create a confusion matrix and calculate accuracy
conf_matrix <- confusionMatrix(as.factor(predicted), as.factor(actual))

# Print the confusion matrix table
print(conf_matrix$table)

# Calculate accuracy
accuracy <- conf_matrix$overall['Accuracy']

# Print accuracy
cat("Accuracy:", accuracy, "\n")

# Additional Evaluations
# Calculate AUC
predictions_prob <- predict(train_model, test_data, type = "response")  # get probabilities
roc_curve <- roc(actual, predictions_prob, plot = TRUE)
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")

# F1 Score, Precision, and Recall
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Precision:", precision, "Recall:", recall, "F1 Score:", f1_score, "\n")

# Define control using 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

# Train the logistic regression model using cross-validation
model_cv <- train(Result ~ AvgH + AvgA, 
                  data = train_data, 
                  method = "glm", 
                  family = binomial(link = "logit"), 
                  trControl = train_control,
                  metric = "ROC")

# Print out the results from cross-validation
print(model_cv)



