library(dplyr)
data_combined3 <- data %>%
  dplyr::select(
    freight_cost_usd,
    `line.item.insurance..usd.`,  
    shipment.mode,             
    country,                   
    product.group,             
    sub.classification,        
    line.item.quantity,        
    line.item.value,           
    weight_kilograms,
    pack.price,
    dosage,
    scheduled.delivery.date,
    delivered.to.client.date
  )
str(data_combined3$dosage)
data_combined3$dosage
data_combined3 <- data_combined3 %>%
  mutate(
    shipment_mode_encoded = as.numeric(factor(shipment.mode)),
    country_encoded = as.numeric(factor(country)),
    product_group_encoded = as.numeric(factor(product.group)),
    sub_classification_encoded = as.numeric(factor(sub.classification))
  )


# Define a function to remove outliers using percentiles
remove_outliers_percentile <- function(data, column, lower_percentile = 0.01, upper_percentile = 0.99) {
  lower_bound <- quantile(data[[column]], lower_percentile, na.rm = TRUE)
  upper_bound <- quantile(data[[column]], upper_percentile, na.rm = TRUE)
  
  # Filter rows within bounds
  data <- data[data[[column]] >= lower_bound & data[[column]] <= upper_bound, ]
  return(data)
}

# Define the columns in data_combined to clean for outliers
columns_to_clean <- c("freight_cost_usd", "weight_kilograms", "line.item.quantity", "line.item.value")

# Apply outlier removal to each column in data_combined
for (col in columns_to_clean) {
  data_combined3 <- remove_outliers_percentile(data_combined3, col, 0.01, 0.99)
}

# Check the summary of the cleaned data_combined
summary(data_combined3)



# Define a function to extract the first numeric value
extract_first_dosage <- function(dosage) {
  # Remove units (e.g., "mg", "g") and split by "/"
  values <- unlist(strsplit(gsub("[^0-9/]", "", dosage), "/"))
  
  # Return the first value as numeric, or NA if no numeric values are found
  if (length(values) > 0) {
    return(as.numeric(values[1]))
  } else {
    return(NA)  # Return NA if no numeric values are found
  }
}

# Apply the function to clean the dosage column
data_combined3 <- data_combined3 %>%
  mutate(dosage = sapply(dosage, extract_first_dosage))

# Verify the cleaned dosage column
summary(data_combined3$dosage)

# Check for missing values
sum(is.na(data_combined3$dosage))


# Step 1: Replace NA values in the dosage column with 0
data_combined3 <- data_combined3 %>%
  mutate(dosage = ifelse(is.na(dosage), 0, dosage))

# Verify that there are no more missing values
sum(is.na(data_combined3$dosage))  # Should return 0

# Step 2: Define a function to remove outliers using percentiles
remove_outliers_percentile <- function(data, column, lower_percentile = 0.01, upper_percentile = 0.99) {
  lower_bound <- quantile(data[[column]], lower_percentile, na.rm = TRUE)
  upper_bound <- quantile(data[[column]], upper_percentile, na.rm = TRUE)
  
  # Filter rows within bounds
  data <- data[data[[column]] >= lower_bound & data[[column]] <= upper_bound, ]
  return(data)
}

# Step 3: Apply outlier removal to the dosage column
data_combined3 <- remove_outliers_percentile(data_combined3, "dosage", 0.01, 0.99)

# Step 4: Verify the cleaned data
summary(data_combined3$dosage)
str(data_combined3$dosage)
data_combined3$dosage
str(data$scheduled.delivery.date)
str(data$delivered.to.client.date)



library(dplyr)

# Convert the date columns in data_combined3
data_combined3 <- data_combined3 %>%
  mutate(
    scheduled_delivery_date = as.Date(scheduled.delivery.date, format = "%d-%b-%y"),
    delivered_to_client_date = as.Date(delivered.to.client.date, format = "%d-%b-%y")
  )

# Verify the structure of the new date columns
str(data_combined3$scheduled_delivery_date)
str(data_combined3$delivered_to_client_date)

# Calculate the delivery time (in days)
data_combined3 <- data_combined3 %>%
  mutate(delivery_time = as.numeric(delivered_to_client_date - scheduled_delivery_date))

# Check summary of the calculated delivery_time
summary(data_combined3$delivery_time)

# Verify the cleaned delivery_time column
summary(data_combined3$delivery_time)

# Optional: Check the first few rows
head(data_combined3[, c("scheduled_delivery_date", "delivered_to_client_date", "delivery_time")])


library(dplyr)

data_combined3 <- data_combined3 %>%
  mutate(
    delivery_category = case_when(
      delivery_time < 0 ~ "Early",
      delivery_time == 0 ~ "On Time",
      delivery_time > 0 ~ "Late"
    )
  )

# Check the distribution of delivery categories
table(data_combined3$delivery_category)

table(data_combined3$shipment_mode[data_combined3$delivery_category == "Early"])

table(data_combined3$country[data_combined3$delivery_category == "Early"])


colnames(data_combined3)

head(data_combined3)



# Assuming your DataFrame is named `data_combined`
write.csv(data_combined3, file = "data_combined3.csv", row.names = FALSE)






# Encode 'delivery_category' as binary (On Time: 1, Delayed: 0)
data_combined3$delivery_category_encoded <- ifelse(data_combined3$delivery_category == "On Time", 1, 0)

# Select predictor columns
predictors <- c('freight_cost_usd', 'line.item.insurance..usd.', 'line.item.quantity', 
                'line.item.value', 'weight_kilograms', 'pack.price', 'dosage', 'delivery_time')

# Subset data for relevant predictors and target variable
data_subset <- data_combined3[, c(predictors, "delivery_category_encoded")]

# Split the data into training and testing sets (80/20 split)
set.seed(42)
train_index <- createDataPartition(data_subset$delivery_category_encoded, p = 0.8, list = FALSE)
train_data <- data_subset[train_index, ]
test_data <- data_subset[-train_index, ]

# Backward selection using AIC
log_model_full <- glm(delivery_category_encoded ~ ., data = train_data, family = binomial)
log_model_backward <- stepAIC(log_model_full, direction = "backward")

# Display the final model
summary(log_model_backward)

# Extract selected variables
selected_variables <- names(coef(log_model_backward))
selected_variables <- selected_variables[selected_variables != "(Intercept)"]  # Exclude intercept

# Fit logistic regression using only the selected variables
formula <- as.formula(paste("delivery_category_encoded ~", paste(selected_variables, collapse = " + ")))
final_model <- glm(delivery_category_encoded ~ line.item.insurance..usd. + line.item.value +
                     pack.price + dosage + delivery_time, data = train_data, family = binomial)

# Make predictions on the test set
test_data$predicted_prob <- predict(final_model, newdata = test_data, type = "response")
test_data$predicted_class <- ifelse(test_data$predicted_prob > 0.5, 1, 0)

# Generate confusion matrix and performance metrics
conf_matrix <- confusionMatrix(factor(test_data$predicted_class), 
                               factor(test_data$delivery_category_encoded),
                               positive = "1")

# Print the results
print(conf_matrix)
