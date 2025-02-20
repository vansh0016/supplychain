# Correlation matrix for numeric variables
cor_matrix <- cor(data_combined[, c("weight_kilograms", "line.item.quantity", "line.item.value")], use = "complete.obs")
print(cor_matrix)

# Load necessary libraries
library(ggplot2)
library(reshape2)
library(corrplot)

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




# Perform linear regression using label-encoded variables
model_encoded <- lm(line.item.insurance..usd. ~ freight_cost_usd + shipment_mode_encoded + 
                      country_encoded + weight_kilograms + line.item.value, data = data_combined)

# View the summary of the regression model
summary(model_encoded)


# Model evaluation: R-squared and RMSE
r_squared_lr <- summary(model_encoded)$r.squared
rmse_lr <- sqrt(mean(model_encoded$residuals^2))

# Print performance metrics
print(paste("Linear Regression - R-squared: ", r_squared_lr))
print(paste("Linear Regression - RMSE: ", rmse_lr))

# Ridge Regression using glmnet
library(glmnet)

# Prepare the data
x <- as.matrix(data_combined[, c("shipment_mode_encoded", "country_encoded", "product_group_encoded", 
                                 "sub_classification_encoded", "weight_kilograms", 
                                 "line.item.quantity", "line.item.value", "pack.price")])
y <- data_combined$total_cost

# Fit the ridge regression model
ridge_model <- cv.glmnet(x, y, alpha = 0)

summary(ridge_model)

# Best lambda from cross-validation
best_lambda_ridge <- ridge_model$lambda.min

# Predict and calculate RMSE and R-squared
pred_ridge <- predict(ridge_model, s = best_lambda_ridge, newx = x)
rmse_ridge <- sqrt(mean((pred_ridge - y)^2))
r_squared_ridge <- 1 - sum((pred_ridge - y)^2) / sum((mean(y) - y)^2)

# Print performance metrics
print(paste("Ridge Regression - R-squared: ", r_squared_ridge))
print(paste("Ridge Regression - RMSE: ", rmse_ridge))



# Lasso Regression using glmnet
lasso_model <- cv.glmnet(x, y, alpha = 1)

# Best lambda from cross-validation
best_lambda_lasso <- lasso_model$lambda.min

# Predict and calculate RMSE and R-squared
pred_lasso <- predict(lasso_model, s = best_lambda_lasso, newx = x)
rmse_lasso <- sqrt(mean((pred_lasso - y)^2))
r_squared_lasso <- 1 - sum((pred_lasso - y)^2) / sum((mean(y) - y)^2)

# Print performance metrics
print(paste("Lasso Regression - R-squared: ", r_squared_lasso))
print(paste("Lasso Regression - RMSE: ", rmse_lasso))




# Random Forest Regression
library(randomForest)
rf_model <- randomForest(total_cost ~ shipment_mode_encoded + country_encoded + product_group_encoded +
                           sub_classification_encoded + weight_kilograms + line.item.quantity + 
                           line.item.value + pack.price, data = data_combined)


importance_matrix <- importance(rf_model)
print(importance_matrix)

varImpPlot(rf_model, main = "Variable Importance in Random Forest")


# Predictions and performance metrics
pred_rf <- predict(rf_model, newdata = data_combined)
rmse_rf <- sqrt(mean((pred_rf - data_combined$total_cost)^2))
r_squared_rf <- 1 - sum((pred_rf - data_combined$total_cost)^2) / sum((mean(data_combined$total_cost) - data_combined$total_cost)^2)

# Print performance metrics
print(paste("Random Forest - R-squared: ", r_squared_rf))
print(paste("Random Forest - RMSE: ", rmse_rf))


model_comparison <- data.frame(
  Model = c("Linear Regression", "Ridge Regression", "Lasso Regression", "Random Forest"),
  R_squared = c(r_squared_lr, r_squared_ridge, r_squared_lasso, r_squared_rf),
  RMSE = c(rmse_lr, rmse_ridge, rmse_lasso, rmse_rf)
)


# View comparison
print(model_comparison)



# Linear Regression Model using highly correlated predictors
model_strong_corr <- lm(line.item.insurance..usd. ~ freight_cost_usd + shipment_mode_encoded + 
                          country_encoded + weight_kilograms + line.item.value, 
                        data = data_combined)

# Model summary
summary(model_strong_corr)


# Random Forest Model using the same predictors
library(randomForest)

rf_model_strong_corr <- randomForest(line.item.insurance..usd. ~ freight_cost_usd + shipment_mode_encoded + 
                                       country_encoded + weight_kilograms + line.item.value, 
                                     data = data_combined)

# Print Random Forest model summary
print(rf_model_strong_corr)

# Plot variable importance
importance(rf_model_strong_corr)


# Predictions from Linear Model
predictions_lr <- predict(model_strong_corr, newdata = data_combined)
rmse_lr <- sqrt(mean((predictions_lr - data_combined$line.item.insurance..usd.)^2))
r_squared_lr <- 1 - sum((predictions_lr - data_combined$line.item.insurance..usd.)^2) / sum((mean(data_combined$line.item.insurance..usd.) - data_combined$line.item.insurance..usd.)^2)

# Predictions from Random Forest Model
predictions_rf <- predict(rf_model_strong_corr, newdata = data_combined)
rmse_rf <- sqrt(mean((predictions_rf - data_combined$line.item.insurance..usd.)^2))
r_squared_rf <- 1 - sum((predictions_rf - data_combined$line.item.insurance..usd.)^2) / sum((mean(data_combined$line.item.insurance..usd.) - data_combined$line.item.insurance..usd.)^2)

# Print Model Comparison
model_comparison <- data.frame(
  Model = c("Linear Regression", "Random Forest"),
  R_squared = c(r_squared_lr, r_squared_rf),
  RMSE = c(rmse_lr, rmse_rf)
)

print(model_comparison)





# Load necessary libraries
library(caret)
library(randomForest)

# Split the data into training (80%) and testing (20%) sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(data_combined$line.item.insurance..usd., p = 0.80, list = FALSE)
train_data <- data_combined[train_index, ]
test_data <- data_combined[-train_index, ]


# Linear Regression Model on Training Data
lr_model <- lm(line.item.insurance..usd. ~ freight_cost_usd + shipment_mode_encoded + 
                 country_encoded + weight_kilograms + line.item.value, 
               data = train_data)

summary(lr_model)

# Random Forest Model on Training Data
rf_model <- randomForest(line.item.insurance..usd. ~ freight_cost_usd + shipment_mode_encoded + 
                           country_encoded + weight_kilograms + line.item.value, 
                         data = train_data)

# Make Predictions on Testing Data
predictions_lr_test <- predict(lr_model, newdata = test_data)
predictions_rf_test <- predict(rf_model, newdata = test_data)


# Calculate RMSE and R-squared for Linear Regression
rmse_lr_test <- sqrt(mean((predictions_lr_test - test_data$line.item.insurance..usd.)^2))
r_squared_lr_test <- 1 - sum((predictions_lr_test - test_data$line.item.insurance..usd.)^2) / sum((mean(test_data$line.item.insurance..usd.) - test_data$line.item.insurance..usd.)^2)

# Calculate RMSE and R-squared for Random Forest
rmse_rf_test <- sqrt(mean((predictions_rf_test - test_data$line.item.insurance..usd.)^2))
r_squared_rf_test <- 1 - sum((predictions_rf_test - test_data$line.item.insurance..usd.)^2) / sum((mean(test_data$line.item.insurance..usd.) - test_data$line.item.insurance..usd.)^2)

# Model Comparison
model_comparison_test <- data.frame(
  Model = c("Linear Regression", "Random Forest"),
  R_squared = c(r_squared_lr_test, r_squared_rf_test),
  RMSE = c(rmse_lr_test, rmse_rf_test)
)

# Print model comparison
print(model_comparison_test)

# Load the required libraries
library(caret)

# Set up the training control with 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Fit the Linear Regression model with 10-fold cross-validation
lr_cv_model <- train(line.item.insurance..usd. ~ freight_cost_usd + shipment_mode_encoded + 
                       country_encoded + weight_kilograms + line.item.value, 
                     data = data_combined, 
                     method = "lm", 
                     trControl = train_control)
summary(lr_cv_model)
# Print the summary of the Linear Regression model
cat("\nLinear Regression - Cross-Validation Results:\n")
print(lr_cv_model)

# Results (RMSE, R-squared, and MAE)
cat("Linear Regression - RMSE:", lr_cv_model$results$RMSE, "\n")
cat("Linear Regression - R-squared:", lr_cv_model$results$Rsquared, "\n")
cat("Linear Regression - MAE:", lr_cv_model$results$MAE, "\n")


# Set up the training control with 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 2)

# Fit the Random Forest model with 10-fold cross-validation
rf_cv_model <- train(line.item.insurance..usd. ~ freight_cost_usd + shipment_mode_encoded + 
                       country_encoded + weight_kilograms + line.item.value, 
                     data = data_combined, 
                     method = "rf", 
                     trControl = train_control)

# Print the summary of the Random Forest model
cat("\nRandom Forest - Cross-Validation Results:\n")
print(rf_cv_model)

# Results (RMSE, R-squared, and MAE)
cat("Random Forest - RMSE:", rf_cv_model$results$RMSE, "\n")
cat("Random Forest - R-squared:", rf_cv_model$results$Rsquared, "\n")
cat("Random Forest - MAE:", rf_cv_model$results$MAE, "\n")


