library(ggplot2)
library(reshape2)
library(corrplot)
library(caret)
library(randomForest)
library(glmnet)
library(rpart)
library(class)
library(FNN)


# Select only numeric columns for correlation matrix
numeric_columns <- data_combined[, sapply(data_combined, is.numeric)]

# Compute the correlation matrix
cor_matrix <- cor(numeric_columns, use = "complete.obs")

# Print the correlation matrix
print(cor_matrix)

# Plot the heatmap using corrplot
corrplot(cor_matrix, method = "color", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", # Display correlation coefficients
         number.cex = 0.7, 
         col = colorRampPalette(c("blue", "white", "red"))(200))


colnames(data_combined)
str(data_combined)

####################################################################################
# Select numeric variables (exclude categorical columns)
numeric_columns <- sapply(data_combined, is.numeric)
data_numeric <- data_combined[, numeric_columns]
####################################################################################

# Split the data into 80% training and 20% testing
set.seed(123)
train_index <- createDataPartition(data_numeric$freight_cost_usd, p = 0.80, list = FALSE)
train_data <- data_numeric[train_index, ]
test_data <- data_numeric[-train_index, ]
####################################################################################

# Fit a Linear Regression Model
lr_model <- lm(freight_cost_usd ~ ., data = train_data)

# Print the summary of the Linear Regression Model
cat("\nLinear Regression Model Summary:\n")
summary(lr_model)

# Predictions and evaluation on the test set
predictions_lr <- predict(lr_model, newdata = test_data)
rmse_lr <- sqrt(mean((predictions_lr - test_data$freight_cost_usd)^2))
r_squared_lr <- 1 - sum((predictions_lr - test_data$freight_cost_usd)^2) / sum((mean(test_data$freight_cost_usd) - test_data$freight_cost_usd)^2)
mae_lr <- mean(abs(predictions_lr - test_data$freight_cost_usd))

cat("Linear Regression - RMSE:", rmse_lr, "\n")
cat("Linear Regression - R-squared:", r_squared_lr, "\n")
cat("Linear Regression - MAE:", mae_lr, "\n")
####################################################################################

# Prepare the data for Ridge Regression
X_train <- as.matrix(train_data[, -which(names(train_data) == "freight_cost_usd")])
y_train <- train_data$freight_cost_usd

# Fit Ridge Regression model
ridge_model <- cv.glmnet(X_train, y_train, alpha = 0)

# Print the summary of Ridge Regression Model
cat("\nRidge Regression Model Summary:\n")
print(ridge_model)

# Predictions on the test set
X_test <- as.matrix(test_data[, -which(names(test_data) == "freight_cost_usd")])
predictions_ridge <- predict(ridge_model, s = "lambda.min", newx = X_test)

# Calculate RMSE, R-squared, and MAE for Ridge Regression
rmse_ridge <- sqrt(mean((predictions_ridge - test_data$freight_cost_usd)^2))
r_squared_ridge <- 1 - sum((predictions_ridge - test_data$freight_cost_usd)^2) / sum((mean(test_data$freight_cost_usd) - test_data$freight_cost_usd)^2)
mae_ridge <- mean(abs(predictions_ridge - test_data$freight_cost_usd))

cat("Ridge Regression - RMSE:", rmse_ridge, "\n")
cat("Ridge Regression - R-squared:", r_squared_ridge, "\n")
cat("Ridge Regression - MAE:", mae_ridge, "\n")
####################################################################################

# Fit Lasso Regression model
lasso_model <- cv.glmnet(X_train, y_train, alpha = 1)

# Print the summary of Lasso Regression Model
cat("\nLasso Regression Model Summary:\n")
print(lasso_model)

# Predictions on the test set
predictions_lasso <- predict(lasso_model, s = "lambda.min", newx = X_test)

# Calculate RMSE, R-squared, and MAE for Lasso Regression
rmse_lasso <- sqrt(mean((predictions_lasso - test_data$freight_cost_usd)^2))
r_squared_lasso <- 1 - sum((predictions_lasso - test_data$freight_cost_usd)^2) / sum((mean(test_data$freight_cost_usd) - test_data$freight_cost_usd)^2)
mae_lasso <- mean(abs(predictions_lasso - test_data$freight_cost_usd))

cat("Lasso Regression - RMSE:", rmse_lasso, "\n")
cat("Lasso Regression - R-squared:", r_squared_lasso, "\n")
cat("Lasso Regression - MAE:", mae_lasso, "\n")
####################################################################################

# Fit a Decision Tree Model
tree_model <- rpart(freight_cost_usd ~ ., data = train_data)

# Print the summary of the Decision Tree Model
cat("\nDecision Tree Model Summary:\n")
summary(tree_model)

# Predictions on the test set
predictions_tree <- predict(tree_model, newdata = test_data)

# Calculate RMSE, R-squared, and MAE for Decision Tree
rmse_tree <- sqrt(mean((predictions_tree - test_data$freight_cost_usd)^2))
r_squared_tree <- 1 - sum((predictions_tree - test_data$freight_cost_usd)^2) / sum((mean(test_data$freight_cost_usd) - test_data$freight_cost_usd)^2)
mae_tree <- mean(abs(predictions_tree - test_data$freight_cost_usd))

cat("Decision Tree - RMSE:", rmse_tree, "\n")
cat("Decision Tree - R-squared:", r_squared_tree, "\n")
cat("Decision Tree - MAE:", mae_tree, "\n")
####################################################################################

# Normalize the data for KNN
train_data_knn <- scale(train_data[, -which(names(train_data) == "freight_cost_usd")])
test_data_knn <- scale(test_data[, -which(names(test_data) == "freight_cost_usd")])

# Fit KNN Model (k = 5)
knn_model <- knn.reg(train = train_data_knn, test = test_data_knn, y = train_data$freight_cost_usd, k = 5)

# Print the KNN Model Summary
cat("\nKNN Model Summary:\n")
print(knn_model)

# Predictions on the test set
predictions_knn <- knn_model$pred

# Calculate RMSE, R-squared, and MAE for KNN
rmse_knn <- sqrt(mean((predictions_knn - test_data$freight_cost_usd)^2))
r_squared_knn <- 1 - sum((predictions_knn - test_data$freight_cost_usd)^2) / sum((mean(test_data$freight_cost_usd) - test_data$freight_cost_usd)^2)
mae_knn <- mean(abs(predictions_knn - test_data$freight_cost_usd))

cat("KNN - RMSE:", rmse_knn, "\n")
cat("KNN - R-squared:", r_squared_knn, "\n")
cat("KNN - MAE:", mae_knn, "\n")
####################################################################################

# Fit a Random Forest Model
rf_model <- randomForest(freight_cost_usd ~ ., data = train_data)

# Predictions on the test set
predictions_rf <- predict(rf_model, newdata = test_data)

# Calculate RMSE, R-squared, and MAE for Random Forest
rmse_rf <- sqrt(mean((predictions_rf - test_data$freight_cost_usd)^2))
r_squared_rf <- 1 - sum((predictions_rf - test_data$freight_cost_usd)^2) / sum((mean(test_data$freight_cost_usd) - test_data$freight_cost_usd)^2)
mae_rf <- mean(abs(predictions_rf - test_data$freight_cost_usd))

cat("Random Forest - RMSE:", rmse_rf, "\n")
cat("Random Forest - R-squared:", r_squared_rf, "\n")
cat("Random Forest - MAE:", mae_rf, "\n")
####################################################################################

# Create a data frame with the comparison of models
model_comparison <- data.frame(
  Model = c("Linear Regression", "Ridge Regression", "Lasso Regression", "Decision Tree", 
            "KNN", "Random Forest"),
  RMSE = c(rmse_lr, rmse_ridge, rmse_lasso, rmse_tree, rmse_knn, rmse_rf),
  R_squared = c(r_squared_lr, r_squared_ridge, r_squared_lasso, r_squared_tree, 
                r_squared_knn, r_squared_rf),
  MAE = c(mae_lr, mae_ridge, mae_lasso, mae_tree, mae_knn, mae_rf)
)

# Print the model comparison
print(model_comparison)
####################################################################################

# Fit a Linear Regression Model to predict 'total_cost', excluding 'line.item.insurance..usd.'
lr_model_total_cost <- lm(total_cost ~ . - line.item.insurance..usd., data = train_data)

# Print the summary of the Linear Regression Model
cat("\nLinear Regression Model Summary for Total Cost (excluding 'line.item.insurance..usd.'):\n")
summary(lr_model_total_cost)

# Predictions and evaluation on the test set
predictions_lr_total_cost <- predict(lr_model_total_cost, newdata = test_data)
rmse_lr_total_cost <- sqrt(mean((predictions_lr_total_cost - test_data$total_cost)^2))
r_squared_lr_total_cost <- 1 - sum((predictions_lr_total_cost - test_data$total_cost)^2) / sum((mean(test_data$total_cost) - test_data$total_cost)^2)
mae_lr_total_cost <- mean(abs(predictions_lr_total_cost - test_data$total_cost))

cat("Linear Regression (Total Cost) - RMSE:", rmse_lr_total_cost, "\n")
cat("Linear Regression (Total Cost) - R-squared:", r_squared_lr_total_cost, "\n")
cat("Linear Regression (Total Cost) - MAE:", mae_lr_total_cost, "\n")
####################################################################################

# Fit a Linear Regression Model to predict 'total_cost', using selected predictors
lr_model_selected <- lm(total_cost ~ freight_cost_usd + line.item.insurance..usd. + 
                          line.item.quantity + weight_kilograms, data = train_data)

# Print the summary of the Linear Regression Model
cat("\nLinear Regression Model Summary (Selected Predictors):\n")
summary(lr_model_selected)

# Predictions and evaluation on the test set
predictions_lr_selected <- predict(lr_model_selected, newdata = test_data)
rmse_lr_selected <- sqrt(mean((predictions_lr_selected - test_data$total_cost)^2))
r_squared_lr_selected <- 1 - sum((predictions_lr_selected - test_data$total_cost)^2) / sum((mean(test_data$total_cost) - test_data$total_cost)^2)
mae_lr_selected <- mean(abs(predictions_lr_selected - test_data$total_cost))

cat("Linear Regression (Selected Predictors) - RMSE:", rmse_lr_selected, "\n")
cat("Linear Regression (Selected Predictors) - R-squared:", r_squared_lr_selected, "\n")
cat("Linear Regression (Selected Predictors) - MAE:", mae_lr_selected, "\n")
####################################################################################

# Prepare the data for Ridge Regression (using selected predictors)
X_train_ridge <- as.matrix(train_data[, c("freight_cost_usd", "line.item.insurance..usd.", "line.item.quantity", "weight_kilograms")])
y_train_ridge <- train_data$total_cost

# Fit Ridge Regression model (alpha = 0 for ridge)
ridge_model <- cv.glmnet(X_train_ridge, y_train_ridge, alpha = 0)

# Print the summary of Ridge Regression Model
cat("\nRidge Regression Model Summary:\n")
print(ridge_model)

# Predictions on the test set
X_test_ridge <- as.matrix(test_data[, c("freight_cost_usd", "line.item.insurance..usd.", "line.item.quantity", "weight_kilograms")])
predictions_ridge <- predict(ridge_model, s = "lambda.min", newx = X_test_ridge)

# Calculate RMSE, R-squared, and MAE for Ridge Regression
rmse_ridge <- sqrt(mean((predictions_ridge - test_data$total_cost)^2))
r_squared_ridge <- 1 - sum((predictions_ridge - test_data$total_cost)^2) / sum((mean(test_data$total_cost) - test_data$total_cost)^2)
mae_ridge <- mean(abs(predictions_ridge - test_data$total_cost))

cat("Ridge Regression - RMSE:", rmse_ridge, "\n")
cat("Ridge Regression - R-squared:", r_squared_ridge, "\n")
cat("Ridge Regression - MAE:", mae_ridge, "\n")
####################################################################################

# Prepare the data for Lasso Regression (using selected predictors)
X_train_lasso <- as.matrix(train_data[, c("freight_cost_usd", "line.item.insurance..usd.", "line.item.quantity", "weight_kilograms")])
y_train_lasso <- train_data$total_cost

# Fit Lasso Regression model (alpha = 1 for lasso)
lasso_model <- cv.glmnet(X_train_lasso, y_train_lasso, alpha = 1)

# Print the summary of Lasso Regression Model
cat("\nLasso Regression Model Summary:\n")
print(lasso_model)

# Predictions on the test set
X_test_lasso <- as.matrix(test_data[, c("freight_cost_usd", "line.item.insurance..usd.", "line.item.quantity", "weight_kilograms")])
predictions_lasso <- predict(lasso_model, s = "lambda.min", newx = X_test_lasso)

# Calculate RMSE, R-squared, and MAE for Lasso Regression
rmse_lasso <- sqrt(mean((predictions_lasso - test_data$total_cost)^2))
r_squared_lasso <- 1 - sum((predictions_lasso - test_data$total_cost)^2) / sum((mean(test_data$total_cost) - test_data$total_cost)^2)
mae_lasso <- mean(abs(predictions_lasso - test_data$total_cost))

cat("Lasso Regression - RMSE:", rmse_lasso, "\n")
cat("Lasso Regression - R-squared:", r_squared_lasso, "\n")
cat("Lasso Regression - MAE:", mae_lasso, "\n")
####################################################################################

# Prepare the data for Random Forest (using selected predictors)
rf_model <- randomForest(total_cost ~ freight_cost_usd + line.item.insurance..usd. + 
                           line.item.quantity + weight_kilograms, data = train_data)

# Print the summary of the Random Forest Model
cat("\nRandom Forest Model Summary:\n")
print(rf_model)

# Predictions on the test set
predictions_rf <- predict(rf_model, newdata = test_data)

# Calculate RMSE, R-squared, and MAE for Random Forest
rmse_rf <- sqrt(mean((predictions_rf - test_data$total_cost)^2))
r_squared_rf <- 1 - sum((predictions_rf - test_data$total_cost)^2) / sum((mean(test_data$total_cost) - test_data$total_cost)^2)
mae_rf <- mean(abs(predictions_rf - test_data$total_cost))

cat("Random Forest - RMSE:", rmse_rf, "\n")
cat("Random Forest - R-squared:", r_squared_rf, "\n")
cat("Random Forest - MAE:", mae_rf, "\n")
####################################################################################

# Normalize the data (only numeric predictors)
train_data_knn <- scale(train_data[, c("freight_cost_usd", "line.item.insurance..usd.", "line.item.quantity", "weight_kilograms")])
test_data_knn <- scale(test_data[, c("freight_cost_usd", "line.item.insurance..usd.", "line.item.quantity", "weight_kilograms")])

# Extract the target variable
y_train_knn <- train_data$total_cost
# Fit KNN Model (k = 5 by default)
knn_model <- knn.reg(train = train_data_knn, test = test_data_knn, y = y_train_knn, k = 5)

# Predictions from the KNN Model
predictions_knn <- knn_model$pred

# Calculate RMSE, R-squared, and MAE for KNN Regression
rmse_knn <- sqrt(mean((predictions_knn - test_data$total_cost)^2))
r_squared_knn <- 1 - sum((predictions_knn - test_data$total_cost)^2) / sum((mean(test_data$total_cost) - test_data$total_cost)^2)
mae_knn <- mean(abs(predictions_knn - test_data$total_cost))

cat("KNN - RMSE:", rmse_knn, "\n")
cat("KNN - R-squared:", r_squared_knn, "\n")
cat("KNN - MAE:", mae_knn, "\n")









