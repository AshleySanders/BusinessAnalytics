# Libraries
library(here)
library(readr)
library(tidyverse)
library(car)
library(caret)
library(ggplot2)
library(corrplot)
library(randomForest)

here()

# Load data
telco <- read_csv(here("r_tidy_ex", "data","tidy_data", "Telco-Customer-Churn.csv"))
telco <-strings2factors(telco) # ensure that categorical data is read as factors in order to run classification algorithms.

summary(telco) # Examine variables
# We have an unbalanced data set with respect to the response variable (churn)

## Data Pre-processing ##

# Convert Senior Citizen to factor
telco$SeniorCitizen <- as.factor(ifelse(telco$SeniorCitizen == 1, "Yes", "No"))

sum(is.na(telco)) # Check for NAs

# Remove customerID column since we won't use it for predictions
telco <- telco %>% select(-customerID)

# Fix data types
telco$TotalCharges <- as.numeric(telco$TotalCharges)
telco$TotalCharges[is.na(telco$TotalCharges)] <- median(telco$TotalCharges, na.rm = TRUE)

## Exploratory Data Analysis ##
# What are the key demographic factors contributing to churn?
ggplot(telco, aes(Partner, fill = Churn)) +
  geom_bar(position = "fill") +
  labs(title = "Churn by Partner", y = "Proportion")

ggplot(telco, aes(SeniorCitizen, fill = Churn)) +
  geom_bar(position = "fill") +
  labs(title = "Churn by Senior Citizen Status", y = "Proportion")

# What service-related factors correlate with higher churn rates?
# InternetService
ggplot(telco, aes(InternetService, fill = Churn)) +
  geom_bar(position = "fill") +
  labs(title = "Churn by Internet Service Type", y = "Proportion")

# PhoneService
ggplot(telco, aes(PhoneService, fill = Churn)) +
  geom_bar(position = "fill") +
  labs(title = "Churn by Phone Service", y = "Proportion")

# Impact of Contract and Payment Method on Churn:
# Contract Type
ggplot(telco, aes(Contract, fill = Churn)) +
  geom_bar(position = "fill") +
  labs(title = "Churn by Contract Type", y = "Proportion")

# Payment Method
ggplot(telco, aes(PaymentMethod, fill = Churn)) +
  geom_bar(position = "fill") +
  labs(title = "Churn by Payment Method", y = "Proportion") +
  coord_flip()

# Identifying high-risk customer segments:
# High-risk segments
# (Month-to-month contract & Fiber optic Internet)
high_risk <- telco %>%
  filter(Contract == "Month-to-month", InternetService == "Fiber optic")

prop.table(table(high_risk$Churn))

## Feature Engineering & Correlation Analysis ##
# Check numeric correlations:
numeric_features <- telco %>% select_if(is.numeric)
correlations <- cor(numeric_features)
corrplot(correlations, method = "ellipse")

boxplot(telco$tenure ~ telco$Churn)

## Developing a Predictive Model ##
# Partition data
set.seed(123)
trainIndex <- createDataPartition(telco$Churn, p = 0.75, list = FALSE)
train <- telco[trainIndex,]
test <- telco[-trainIndex,]

# Random Forest - train the model
set.seed(123)
rf_model <- randomForest(Churn ~ ., data = train, importance = TRUE)
rf_model

# Examine the importance of variables
varImpPlot(rf_model)

## Tuning the model ##
# Define parameter grid:
tune_grid <- expand.grid(.mtry = c(2, 4, 6, 8, 10))

# Cross-validation setup:
control <- trainControl(method = "cv", number = 5, search = "grid")

# Train with caret:
set.seed(123)
rf_tuned <- train(
  Churn ~ .,
  data = train,
  method = "rf",
  metric = "Accuracy",
  tuneGrid = tune_grid,
  trControl = control,
  ntree = 200,
  nodesize = 5
)

# Best tuned model:
print(rf_tuned)
plot(rf_tuned)

# This confirms the parameters of the original model.

## Evaluate & validate model ##
predictions <- predict(rf_model, newdata = test)
confusionMatrix(predictions, test$Churn, positive = "Yes")

# Compute ROC/AUC for performance:Compute ROC/AUC for performance:
library(pROC)
pred_prob <- predict(rf_model, newdata = test, type = "prob")
roc_curve <- roc(response = test$Churn, predictor = pred_prob[, "Yes"])
plot(roc_curve, main = "ROC Curve for Random Forest")
auc(roc_curve)

## Prepare data and model for Shiny Dashboard ##

# Load dataset
# Save processed data and model
saveRDS(telco, "telco_clean.rds")
saveRDS(rf_model, "rf_model.rds")


library(shiny)
runApp(here("telco_churn_dashboard"))
