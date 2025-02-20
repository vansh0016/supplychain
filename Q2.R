# Load required libraries
library(caret)
library(randomForest)

# 1. Define thresholds for categorization (e.g., quantiles for low, medium, high)
data_combined$insurance_category <- cut(
  data_combined$line.item.insurance..usd.,
  breaks = quantile(data_combined$line.item.insurance..usd., probs = c(0, 0.33, 0.67, 1), na.rm = TRUE),
  labels = c("low", "medium", "high"),
  include.lowest = TRUE
)

# Convert the category into a factor
data_combined$insurance_category <- as.factor(data_combined$insurance_category)

# 2. Prepare data for classification
classification_features <- c("shipment_mode_encoded", "country_encoded", "product_group_encoded")
target_variable <- "insurance_category"

# Split the data into training (80%) and testing (20%) sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(data_combined[[target_variable]], p = 0.80, list = FALSE)
train_data <- data_combined[train_index, ]
test_data <- data_combined[-train_index, ]

# 3. Fit a Decision Tree model
dt_model <- train(
  insurance_category ~ shipment_mode_encoded + country_encoded + freight_cost_usd + weight_kilograms + line.item.value,
  data = train_data,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10)  # 10-fold cross-validation
)
summary(dt_model)

# Print Decision Tree model summary
cat("\nDecision Tree Model Summary:\n")
print(dt_model)

# 4. Fit a Random Forest model
rf_model <- randomForest(
  insurance_category ~ shipment_mode_encoded + country_encoded + freight_cost_usd + weight_kilograms + line.item.value,
  data = train_data,
  ntree = 500
)

# Print Random Forest model summary
cat("\nRandom Forest Model Summary:\n")
print(rf_model)

# 5. Evaluate Models on Test Data
dt_predictions <- predict(dt_model, newdata = test_data)
rf_predictions <- predict(rf_model, newdata = test_data)

# Confusion Matrix for Decision Tree
dt_cm <- confusionMatrix(dt_predictions, test_data$insurance_category)
cat("\nDecision Tree - Confusion Matrix:\n")
print(dt_cm)

# Confusion Matrix for Random Forest
rf_cm <- confusionMatrix(rf_predictions, test_data$insurance_category)
cat("\nRandom Forest - Confusion Matrix:\n")
print(rf_cm)

# 6. Model Performance Metrics
cat("\nDecision Tree Accuracy:", dt_cm$overall['Accuracy'], "\n")
cat("Random Forest Accuracy:", rf_cm$overall['Accuracy'], "\n")
