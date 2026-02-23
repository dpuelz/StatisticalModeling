# =============================================================================
# REGRESSION TREES AND ENSEMBLE METHODS — Student Walkthrough
# Statistical Modeling, University of Austin
# =============================================================================
#
# This script walks through the key methods covered in the lecture:
#   1. Single regression trees
#   2. Pruning
#   3. Bagging (bootstrap aggregation)
#   4. Random forests
#   5. Gradient boosting
#
# Dataset: Boston housing (506 census tracts, 13 predictors)
# Response: medv — median home value in $1000s
# =============================================================================

# --- Setup -------------------------------------------------------------------

library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)

# Load data — adjust path to wherever you saved Boston.csv
# You can also use: boston <- MASS::Boston
boston <- read.csv("../../StatisticalModeling/data/Boston.csv")
str(boston)

# Train/test split (70/30)
set.seed(42)
n <- nrow(boston)
train_idx <- sample(1:n, round(0.7 * n))
train <- boston[train_idx, ]
test  <- boston[-train_idx, ]

cat("Training set:", nrow(train), "observations\n")
cat("Test set:    ", nrow(test), "observations\n")


# =============================================================================
# 1. SINGLE REGRESSION TREE
# =============================================================================

# Fit a tree with default settings
tree1 <- rpart(medv ~ ., data = train)

# Visualize the tree
rpart.plot(tree1, type = 4, extra = 1)

# How to read the output:
#   - Each internal node shows a splitting rule (e.g., lstat < 9.7)
#   - Left branch = "Yes" (condition is true)
#   - Right branch = "No" (condition is false)
#   - Leaf nodes show the predicted value (average of Y in that region)

# Predictions on the test set
pred_tree <- predict(tree1, test)

# Test RMSE
rmse_tree <- sqrt(mean((pred_tree - test$medv)^2))
cat("Single tree test RMSE:", round(rmse_tree, 2), "\n")


# =============================================================================
# 2. PRUNING: FINDING THE RIGHT TREE SIZE
# =============================================================================

# Step 1: Grow a very large tree (small cp allows many splits)
big_tree <- rpart(medv ~ ., data = train,
                  control = rpart.control(cp = 0.001, minsplit = 5))

cat("Full tree has", sum(big_tree$frame$var == "<leaf>"), "terminal nodes\n")

# Step 2: Look at the cross-validation results
printcp(big_tree)
rpart.plot(big_tree)
plotcp(big_tree)

# Step 3: Find the best cp using the 1-SE rule
#   - Find the tree size with minimum CV error
#   - Then pick the simplest tree within 1 SE of that minimum
cp_table <- big_tree$cptable
min_idx   <- which.min(cp_table[, "xerror"])
threshold <- cp_table[min_idx, "xerror"] + cp_table[min_idx, "xstd"]
best_idx  <- min(which(cp_table[, "xerror"] <= threshold))
best_cp   <- cp_table[best_idx, "CP"]

cat("Best cp (1-SE rule):", round(best_cp, 4), "\n")

# Step 4: Prune the tree
pruned <- prune(big_tree, cp = best_cp)
cat("Pruned tree has", sum(pruned$frame$var == "<leaf>"), "terminal nodes\n")

rpart.plot(pruned, type = 4, extra = 1)

# Compare test performance
pred_pruned <- predict(pruned, test)
rmse_pruned <- sqrt(mean((pred_pruned - test$medv)^2))
cat("Pruned tree test RMSE:", round(rmse_pruned, 2), "\n")
cat("Full tree test RMSE:  ", round(rmse_tree, 2), "\n")


# =============================================================================
# 3. BAGGING (Bootstrap Aggregation)
# =============================================================================

# Bagging = random forest with mtry = p (all predictors at each split)
# In randomForest(), set mtry = number of predictors
p <- ncol(train) - 1  # number of predictors (13 minus response)

bag <- randomForest(medv ~ ., data = train,
                    mtry = p,        # use ALL predictors = bagging
                    ntree = 500,
                    importance = TRUE)

# Print summary
print(bag)

# OOB error over trees (see convergence)
plot(1:500, sqrt(bag$mse), type = "l",
     xlab = "Number of trees", ylab = "OOB RMSE",
     main = "Bagging: OOB Error")

# Test RMSE
pred_bag <- predict(bag, test)
rmse_bag <- sqrt(mean((pred_bag - test$medv)^2))
cat("Bagging test RMSE:", round(rmse_bag, 2), "\n")


# =============================================================================
# 4. RANDOM FORESTS
# =============================================================================

# Default mtry for regression: p/3
# For classification: sqrt(p)
rf <- randomForest(medv ~ ., data = train,
                   mtry = floor(p / 3),  # random subset of predictors
                   ntree = 500,
                   importance = TRUE)

print(rf)

# Test RMSE
pred_rf <- predict(rf, test)
rmse_rf <- sqrt(mean((pred_rf - test$medv)^2))
cat("Random forest test RMSE:", round(rmse_rf, 2), "\n")

# Variable importance
importance(rf)
varImpPlot(rf, main = "Variable Importance (Random Forest)")

# Key insight: lstat and rm dominate


# =============================================================================
# 5. GRADIENT BOOSTING
# =============================================================================

boost <- gbm(medv ~ ., data = train,
             distribution = "gaussian",
             n.trees = 5000,
             interaction.depth = 4,
             shrinkage = 0.01,
             cv.folds = 5,
             verbose = FALSE)

# Find optimal number of trees via CV
best_n <- gbm.perf(boost, method = "cv")
cat("Optimal number of trees:", best_n, "\n")

# Test RMSE using optimal number of trees
pred_boost <- predict(boost, test, n.trees = best_n)
rmse_boost <- sqrt(mean((pred_boost - test$medv)^2))
cat("Boosting test RMSE:", round(rmse_boost, 2), "\n")

# Variable importance
summary(boost, plotit = TRUE)


# =============================================================================
# 6. COMPARISON OF ALL METHODS
# =============================================================================

# Linear regression baseline
lm_fit <- lm(medv ~ ., data = train)
pred_lm <- predict(lm_fit, test)
rmse_lm <- sqrt(mean((pred_lm - test$medv)^2))

cat("\n========================================\n")
cat("  Method Comparison (Test RMSE)\n")
cat("========================================\n")
cat("Linear Regression:", round(rmse_lm, 2), "\n")
cat("Single Tree:      ", round(rmse_tree, 2), "\n")
cat("Pruned Tree:      ", round(rmse_pruned, 2), "\n")
cat("Bagging:          ", round(rmse_bag, 2), "\n")
cat("Random Forest:    ", round(rmse_rf, 2), "\n")
cat("Boosting:         ", round(rmse_boost, 2), "\n")
cat("========================================\n")


# =============================================================================
# EXERCISES
# =============================================================================
#
# 1. Try different values of mtry in randomForest (e.g., 2, 4, 6, 9, 13).
#    How does test RMSE change? When is mtry = p (bagging) vs mtry < p better?
#
# 2. For boosting, try different shrinkage rates (0.001, 0.01, 0.1, 0.5).
#    How does the optimal number of trees change?
#
# 3. Fit a random forest to a different dataset (e.g., counties.csv).
#    Which variables are most important? Does the variable importance
#    ordering match your intuition?
#
# 4. Compare the partial dependence plots for lstat from the random forest
#    with a simple scatterplot of lstat vs medv. What does the partial
#    dependence plot tell you that the scatterplot doesn't?
#
# 5. Try interaction.depth = 1 (stumps) in gbm. How does this affect
#    performance? What does depth = 1 mean for the boosted model?
