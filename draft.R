data <- read.csv("Supply_Chain_Shipment_Pricing_Dataset.csv")

colnames(data)

library(dplyr)

# Clean the freight cost column
data <- data %>%
  mutate(freight_cost_usd = ifelse(grepl("[a-zA-Z]", `freight.cost..usd.`), NA, `freight.cost..usd.`), # Set textual entries to NA
         freight_cost_usd = gsub("[^0-9.]", "", freight_cost_usd), # Remove non-numeric characters
         freight_cost_usd = as.numeric(freight_cost_usd)) # Convert to numeric


# Check summary of cleaned freight cost column
summary(data$freight_cost_usd)



# Clean the weight..kilograms. column
data <- data %>%
  mutate(weight_kilograms = ifelse(grepl("[a-zA-Z]", `weight..kilograms.`), NA, `weight..kilograms.`), # Set textual entries to NA
         weight_kilograms = gsub("[^0-9.]", "", weight_kilograms), # Remove non-numeric characters
         weight_kilograms = as.numeric(weight_kilograms)) # Convert to numeric


# Check summary of cleaned weight column
summary(data$weight_kilograms)

# Drop rows with NA values from the entire dataset
# data_clean_all <- na.omit(data)

# Fill NA values in freight_cost_usd with the median
data <- data %>%
  mutate(freight_cost_usd = ifelse(is.na(freight_cost_usd), median(freight_cost_usd, na.rm = TRUE), freight_cost_usd))

# Verify the updated summary
summary(data$freight_cost_usd)



data <- data %>%
  mutate(weight_kilograms = ifelse(is.na(weight_kilograms), median(weight_kilograms, na.rm = TRUE), weight_kilograms))

summary(data$weight_kilograms)

unique(data$shipment.mode)

# Correctly reference the column and count empty strings
empty_values_count <- sum(data$`shipment.mode` == "")
print(empty_values_count)

# Remove rows with empty shipment_mode
data <- data[data$shipment.mode != "", ]

unique(data$country)


summary(data$line.item.quantity)
summary(data$line.item.value)
summary(data$line.item.insurance..usd)



boxplot(data$line.item.quantity, main = "Line Item Quantity")
boxplot(data$line.item.value, main = "Line Item Value")
boxplot(data$line.item.insurance..usd, main = "Line Item Insurance (USD)")


# Remove rows where line.item.insurance..usd is NA
data <- data[!is.na(data$line.item.insurance..usd), ]

# Verify that missing values have been removed
sum(is.na(data$line.item.insurance..usd))



unique(data$product.group)
unique(data$sub.classification)

sum(is.na(data$product.group))
sum(is.na(data$sub.classification))








# Create a new data frame for both research questions
data_combined <- data %>%
  dplyr::select(
    freight_cost_usd,
    line.item.insurance..usd.,  
    shipment.mode,             
    country,                   
    product.group,             
    sub.classification,        
    line.item.quantity,        
    line.item.value,           
    weight_kilograms,
    pack.price
  )

# View the new data frame
head(data_combined)



# Convert categorical columns to factors
data_combined <- data_combined %>%
  mutate(
    shipment_mode = factor(shipment.mode),
    country = factor(country),
    product_group = factor(product.group),
    sub_classification = factor(sub.classification)
  )

# View the structure of the data to verify
str(data_combined)



# Check the summary of categorical variables
summary(data_combined)



# Label encoding for categorical variables and store in new columns
data_combined <- data_combined %>%
  mutate(
    shipment_mode_encoded = as.numeric(factor(shipment.mode)),
    country_encoded = as.numeric(factor(country)),
    product_group_encoded = as.numeric(factor(product.group)),
    sub_classification_encoded = as.numeric(factor(sub.classification))
  )

# View the new columns
head(data_combined[, c("shipment_mode_encoded", "country_encoded", "product_group_encoded", "sub_classification_encoded")])

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
  data_combined <- remove_outliers_percentile(data_combined, col, 0.01, 0.99)
}

# Check the summary of the cleaned data_combined
summary(data_combined)






