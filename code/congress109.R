# Penalized logistic regression: Predicting party from congressional speech
# Required packages: glmnet
# Run from the StatisticalModeling/code directory
# Data: congress109.csv (phrase counts), congress109members.csv (party, etc.)

library(glmnet)

##
# Load data
##
countdata <- read.csv("../data/congress109.csv", header = TRUE, row.names = 1)
memberdata <- read.csv("../data/congress109members.csv", header = TRUE, row.names = 1)

# Rows should align (same members in both files)
stopifnot(all(rownames(countdata) == rownames(memberdata)))

# Exclude Independents for clean binary classification (R vs D)
keep <- memberdata$party %in% c("R", "D")
countdata <- countdata[keep, ]
memberdata <- memberdata[keep, ]
cat("Using", sum(keep), "members (R + D only)\n")
table(memberdata$party)

# Outcome: 1 = Republican, 0 = Democrat
y <- as.numeric(memberdata$party == "R")

# Predictors: document-term matrix (phrase counts in speeches)
# Each column is a phrase; each row is a member; values are counts
X <- as.matrix(countdata)
cat("Dimensions:", nrow(X), "members x", ncol(X), "phrases\n")

# Center and scale predictors (important for penalized regression)
X_scaled <- scale(X)

# Remove any constant columns
const_cols <- apply(X_scaled, 2, function(z) sd(z, na.rm = TRUE) == 0)
if (any(const_cols)) {
  X_scaled <- X_scaled[, !const_cols]
  cat("Removed", sum(const_cols), "constant columns\n")
}

##
# Train/test split
##
set.seed(42)
n <- nrow(X_scaled)
train_idx <- sample(n, floor(0.7 * n))
test_idx <- setdiff(1:n, train_idx)

X_train <- X_scaled[train_idx, ]
y_train <- y[train_idx]
X_test <- X_scaled[test_idx, ]
y_test <- y[test_idx]

##
# Ridge regression (alpha = 0): shrinks coefficients but keeps all
##
ridge_cv <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 0,
                      type.measure = "class", nfolds = 10)
ridge_fit <- glmnet(X_train, y_train, family = "binomial", alpha = 0,
                    lambda = ridge_cv$lambda.min)
ridge_pred <- predict(ridge_fit, newx = X_test, type = "class")
cat("\n--- Ridge ---\n")
cat("Test accuracy:", round(mean(as.numeric(ridge_pred) == y_test), 4), "\n")
cat("Nonzero coefficients:", sum(coef(ridge_fit)[-1] != 0), "(Ridge keeps all)\n")

##
# LASSO (alpha = 1): can set coefficients exactly to zero (variable selection)
##
lasso_cv <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1,
                      type.measure = "class", nfolds = 10)
cat("\n--- LASSO ---\n")
cat("Lambda min:", round(lasso_cv$lambda.min, 6), "\n")
cat("Lambda 1se:", round(lasso_cv$lambda.1se, 6), "\n")

lasso_fit_min <- glmnet(X_train, y_train, family = "binomial", alpha = 1,
                        lambda = lasso_cv$lambda.min)
lasso_fit_1se <- glmnet(X_train, y_train, family = "binomial", alpha = 1,
                        lambda = lasso_cv$lambda.1se)

cat("At lambda_min: nonzero coefs =", sum(coef(lasso_fit_min)[-1] != 0), "\n")
cat("At lambda_1se: nonzero coefs =", sum(coef(lasso_fit_1se)[-1] != 0), "\n")

lasso_pred_min <- predict(lasso_fit_min, newx = X_test, type = "class")
lasso_pred_1se <- predict(lasso_fit_1se, newx = X_test, type = "class")
cat("Test accuracy (lambda_min):", round(mean(as.numeric(lasso_pred_min) == y_test), 4), "\n")
cat("Test accuracy (lambda_1se):", round(mean(as.numeric(lasso_pred_1se) == y_test), 4), "\n")

##
# Coefficient paths: how do coefficients change as lambda varies?
##
plot(lasso_cv$glmnet.fit, xvar = "lambda", main = "LASSO coefficient paths")
abline(v = log(lasso_cv$lambda.1se), lty = 2, col = "gray")

##
# Interpretable output: which phrases predict R vs D?
# Positive coef = more likely Republican; negative = more likely Democrat
##
beta <- coef(lasso_fit_1se)
beta_vec <- as.vector(beta[-1])
names(beta_vec) <- colnames(X_scaled)
nonzero <- beta_vec != 0

cat("\n--- Top phrases predictive of Republican (positive coef) ---\n")
pos <- sort(beta_vec[nonzero & beta_vec > 0], decreasing = TRUE)[1:15]
print(names(pos))

cat("\n--- Top phrases predictive of Democrat (negative coef) ---\n")
neg <- sort(beta_vec[nonzero & beta_vec < 0])[1:15]
print(names(neg))

##
# Summary
##
cat("\n--- Summary ---\n")
cat("Ridge test accuracy:", round(mean(as.numeric(ridge_pred) == y_test), 4), "\n")
cat("LASSO (lambda_1se) test accuracy:", round(mean(as.numeric(lasso_pred_1se) == y_test), 4), "\n")
cat("LASSO selects", sum(coef(lasso_fit_1se)[-1] != 0), "phrases from", ncol(X_scaled), "total\n")
