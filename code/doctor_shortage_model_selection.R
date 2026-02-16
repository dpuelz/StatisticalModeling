# =============================================================================
# Doctor Shortage Example: Model Selection
# 
# This script implements the doctor shortage example from the model selection
# slides. It demonstrates:
# - Best subsets regression
# - Backward stepwise regression  
# - Training/test set split
# - Comparing full vs reduced models using RMSE
# =============================================================================

library(tidyverse)
library(leaps)  # for regsubsets()
library(car)    # for some diagnostic functions

# -----------------------------------------------------------------------------
# 1. Load and explore the data
# -----------------------------------------------------------------------------
# Data from all counties in Texas with 10,000+ residents
# Goal: Model what factors predict the number of physicians per 10,000 people
# Outcome: PhysiciansPer10000
# Predictors: LandArea, PctRural, MedianIncome, Population, PctUnder18,
#             PctOver65, PctPoverty, PctUninsured, PctSomeCollege, PctUnemployed
counties <- read.csv("../data/counties.csv")

# -----------------------------------------------------------------------------
# 2. Best subsets regression
# -----------------------------------------------------------------------------
# Finds the best model for each number of predictors
# Uses Adjusted R^2 as the selection criterion
best_subsets <- regsubsets(PhysiciansPer10000 ~ . - County, data = counties)

# Plot showing Adjusted R^2 for each model size
plot(best_subsets, scale = "adjr2", col = "orange", 
     main = "Best Subsets Regression: Adjusted R^2")

# -----------------------------------------------------------------------------
# 3. Backward stepwise regression
# -----------------------------------------------------------------------------
# Start with full model and remove variables to optimize AIC
full <- lm(PhysiciansPer10000 ~ LandArea + PctRural +
             MedianIncome + Population + PctUnder18 +
             PctOver65 + PctPoverty + PctUninsured +
             PctSomeCollege + PctUnemployed,
           data = counties)

# Backward stepwise selection (variables removed if they improve AIC)
# Capture stepwise path by running with trace and extracting AIC values
stepwise_result <- step(full, direction = "backward", trace = 0)

# Extract stepwise path: track AIC at each step through full backward elimination
aic_path <- numeric()
model_sizes <- numeric()
removed_vars <- character()
current_model <- full
aic_path[1] <- AIC(current_model)
model_sizes[1] <- length(coef(current_model)) - 1  # number of predictors
removed_vars[1] <- "Start (full model)"

# Continue backward stepwise until only intercept remains
# Track the path even when AIC increases
while(TRUE) {
  current_aic <- AIC(current_model)
  predictors <- names(coef(current_model))[-1]  # exclude intercept
  
  if(length(predictors) == 0) break
  
  # Try removing each predictor and see which gives best (lowest) AIC
  best_aic <- Inf
  best_model <- NULL
  removed_var <- NULL
  
  for(var in predictors) {
    # Create formula without this variable
    if(length(predictors) == 1) {
      formula_str <- "PhysiciansPer10000 ~ 1"
    } else {
      formula_str <- paste("PhysiciansPer10000 ~", 
                          paste(setdiff(predictors, var), collapse = " + "))
    }
    test_model <- lm(as.formula(formula_str), data = counties)
    test_aic <- AIC(test_model)
    
    if(test_aic < best_aic) {
      best_aic <- test_aic
      best_model <- test_model
      removed_var <- var
    }
  }
  
  # Record this step (even if AIC increased)
  aic_path <- c(aic_path, best_aic)
  model_sizes <- c(model_sizes, length(coef(best_model)) - 1)
  removed_vars <- c(removed_vars, removed_var)
  current_model <- best_model
}

# Plot stepwise path showing full trajectory
par(mfrow = c(1, 1), mar = c(5, 4, 3, 1))
plot(model_sizes, aic_path, type = "b", 
     xlab = "Number of Predictors", ylab = "AIC",
     main = "Backward Stepwise Regression Path\n(Full elimination path)",
     pch = 16, col = "darkblue", lwd = 2)
# Highlight the minimum AIC point
min_idx <- which.min(aic_path)
points(model_sizes[min_idx], aic_path[min_idx], 
       pch = 21, bg = "red", cex = 1.5, col = "red")
abline(v = model_sizes[min_idx], lty = 2, col = "red", lwd = 1)
text(model_sizes[min_idx], max(aic_path), 
     paste("Min AIC at", model_sizes[min_idx], "predictors"), 
     pos = 3, col = "red", cex = 0.8)
grid()

# Reduced model (typically includes: PctRural, PctUnder18, PctOver65, 
# PctSomeCollege, PctUnemployed)
reduced <- lm(PhysiciansPer10000 ~ PctRural + PctUnder18 +
                PctOver65 + PctSomeCollege + PctUnemployed,
              data = counties)

# -----------------------------------------------------------------------------
# 4. Training and test sets
# -----------------------------------------------------------------------------
# Split data into training (70%) and test (30%) sets
# n = 168, 30% ≈ 50
set.seed(42)
test_cases <- sample(1:nrow(counties), 50)
test_set <- counties[test_cases, ]
training_set <- counties[-test_cases, ]

# -----------------------------------------------------------------------------
# 5. Fit models on training set
# -----------------------------------------------------------------------------
# Full model on training set
full_train <- lm(PhysiciansPer10000 ~ LandArea + PctRural +
                   MedianIncome + Population + PctUnder18 +
                   PctOver65 + PctPoverty + PctUninsured +
                   PctSomeCollege + PctUnemployed,
                 data = training_set)

# Reduced model on training set
reduced_train <- lm(PhysiciansPer10000 ~ PctRural + PctUnder18 +
                      PctOver65 + PctSomeCollege + PctUnemployed,
                    data = training_set)

# -----------------------------------------------------------------------------
# 6. Calculate RMSE on training and test sets
# -----------------------------------------------------------------------------
# RMSE function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Full model RMSE
full_train_rmse <- rmse(training_set$PhysiciansPer10000, predict(full_train))
full_test_rmse <- rmse(test_set$PhysiciansPer10000, predict(full_train, newdata = test_set))

# Reduced model RMSE
reduced_train_rmse <- rmse(training_set$PhysiciansPer10000, predict(reduced_train))
reduced_test_rmse <- rmse(test_set$PhysiciansPer10000, predict(reduced_train, newdata = test_set))

# Comparison table
comparison <- data.frame(
  Model = c("Full model (10 predictors)", "Reduced model (5 predictors)"),
  Training_RMSE = c(full_train_rmse, reduced_train_rmse),
  Test_RMSE = c(full_test_rmse, reduced_test_rmse)
)

# The reduced model typically does worse on training set but better on test set
# This suggests the full model is overfitting the training data

# -----------------------------------------------------------------------------
# 7. Additional diagnostics
# -----------------------------------------------------------------------------
# Model statistics for comparison
full_r2 <- summary(full_train)$r.squared
full_adj_r2 <- summary(full_train)$adj.r.squared
full_aic <- AIC(full_train)

reduced_r2 <- summary(reduced_train)$r.squared
reduced_adj_r2 <- summary(reduced_train)$adj.r.squared
reduced_aic <- AIC(reduced_train)

# -----------------------------------------------------------------------------
# 8. Visualization: Predictions vs Actual
# -----------------------------------------------------------------------------
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

# Full model predictions
plot(test_set$PhysiciansPer10000, predict(full_train, newdata = test_set),
     xlab = "Actual PhysiciansPer10000",
     ylab = "Predicted PhysiciansPer10000",
     main = "Full Model (10 predictors)\nTest Set Predictions",
     pch = 16, col = "steelblue")
abline(0, 1, col = "red", lwd = 2)
text(x = min(test_set$PhysiciansPer10000) + 2,
     y = max(predict(full_train, newdata = test_set)) - 1,
     labels = paste("RMSE =", round(full_test_rmse, 2)),
     cex = 0.9)

# Reduced model predictions
plot(test_set$PhysiciansPer10000, predict(reduced_train, newdata = test_set),
     xlab = "Actual PhysiciansPer10000",
     ylab = "Predicted PhysiciansPer10000",
     main = "Reduced Model (5 predictors)\nTest Set Predictions",
     pch = 16, col = "darkgreen")
abline(0, 1, col = "red", lwd = 2)
text(x = min(test_set$PhysiciansPer10000) + 2,
     y = max(predict(reduced_train, newdata = test_set)) - 1,
     labels = paste("RMSE =", round(reduced_test_rmse, 2)),
     cex = 0.9)
