# Penalized Regression Example: Texas physicians per capita
# Ridge, the LASSO, and choosing lambda by cross-validation.
# Uses counties.csv
#
# Companion to the slides: penalized-regression.pdf
# (and the end of model-selection.pdf, which this picks up from)

library(tidyverse)
library(glmnet)   # install.packages("glmnet") if you do not have it

# ============================================================================
# Data Description
# ============================================================================
# counties.csv: the 168 Texas counties with 10,000+ residents.
#   Response:   PhysiciansPer10000 -- physicians per 10,000 people
#   Predictors: LandArea, PctRural, MedianIncome, Population, PctUnder18,
#               PctOver65, PctPoverty, PctUninsured, PctSomeCollege,
#               PctUnemployed
#
# Reading the county names in as row names means every formula below can be
# simply  PhysiciansPer10000 ~ .  (the dot means "every other column").

counties <- read.csv("../data/counties.csv", row.names = "County")

dim(counties)
head(counties)

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))

# ============================================================================
# 1. Give OLS too much freedom and watch it overfit
# ============================================================================

# .^2 means every main effect plus every pairwise interaction:
# 10 + 45 = 55 predictors for 168 counties.
X   <- model.matrix(PhysiciansPer10000 ~ .,   data = counties)[, -1]
X55 <- model.matrix(PhysiciansPer10000 ~ .^2, data = counties)[, -1]
y   <- counties$PhysiciansPer10000
ncol(X)
ncol(X55)

# Assign every county to one of 10 folds. Each fold takes one turn as the
# test set while the model is fit on the other nine.
set.seed(1)
folds <- sample(rep(1:10, length.out = nrow(counties)))

cv.ols <- function(Xmat) {
  sapply(1:10, function(f) {
    train <- folds != f
    fit   <- lm(y[train] ~ Xmat[train, ])
    pred  <- cbind(1, Xmat[!train, ]) %*% coef(fit)
    rmse(y[!train], pred)
  })
}

# In-sample, the 55-predictor model looks better...
rmse(y, fitted(lm(y ~ X)))
rmse(y, fitted(lm(y ~ X55)))

# ...but on held-out folds it is much worse.
mean(cv.ols(X))
mean(cv.ols(X55))

# ============================================================================
# 2. Ridge regression
# ============================================================================

# glmnet wants a numeric matrix X and a vector y, not a formula.
# alpha = 0 is ridge; alpha = 1 is the LASSO.
# glmnet standardizes the predictors for you (standardize = TRUE by default),
# so the penalty treats dollars and percentages fairly.
fit.ridge <- glmnet(X, y, alpha = 0)

# The coefficient path: each line is one predictor. Read right to left --
# as the penalty loosens, coefficients grow toward their OLS values.
# Every coefficient shrinks; none hits exactly zero.
plot(fit.ridge, xvar = "lambda", label = TRUE)

# ============================================================================
# 3. The LASSO
# ============================================================================

fit.lasso <- glmnet(X, y, alpha = 1)

# Now coefficients hit exactly zero and stay there: the LASSO selects
# variables as it shrinks. The numbers along the top count nonzero terms.
plot(fit.lasso, xvar = "lambda", label = TRUE)

# Coefficients at one particular lambda (dots are exact zeros)
coef(fit.lasso, s = 0.1)

# ============================================================================
# 4. Choosing lambda by cross-validation
# ============================================================================

set.seed(1)
cv.lasso <- cv.glmnet(X, y, alpha = 1)   # 10-fold CV over a grid of lambdas

# The CV curve, with +/- 1 standard error bars. The two dashed lines are
# lambda.min (lowest CV error) and lambda.1se (simplest model within one
# standard error of the best).
plot(cv.lasso)

cv.lasso$lambda.min
cv.lasso$lambda.1se

coef(cv.lasso, s = "lambda.min")
coef(cv.lasso, s = "lambda.1se")   # fewer predictors, almost the same error

# Predictions work like predict() for lm, with newx instead of newdata
predict(cv.lasso, newx = X[1:5, ], s = "lambda.min")

# Ridge, chosen the same way
set.seed(1)
cv.ridge <- cv.glmnet(X, y, alpha = 0)
coef(cv.ridge, s = "lambda.min")

# Elastic net: alpha between 0 and 1 blends the two penalties.
set.seed(1)
cv.enet <- cv.glmnet(X, y, alpha = 0.5)
coef(cv.enet, s = "lambda.min")

# ============================================================================
# 5. The showdown: back to the 55 predictors
# ============================================================================

# For an honest comparison, lambda must be chosen INSIDE each training fold
# (cv.glmnet runs its own CV on the nine training folds), so the held-out
# fold never influences the model that predicts it.
cv.penalized <- function(alpha) {
  sapply(1:10, function(f) {
    train <- folds != f
    fit   <- cv.glmnet(X55[train, ], y[train], alpha = alpha)
    rmse(y[!train], predict(fit, X55[!train, ], s = "lambda.min"))
  })
}

results <- data.frame(
  ols10 = cv.ols(X),
  ols55 = cv.ols(X55),
  ridge = cv.penalized(0),
  lasso = cv.penalized(1)
)
colMeans(results)

# The same 55 predictors that wrecked OLS now beat even the 10-predictor OLS.
results %>%
  pivot_longer(everything(), names_to = "model", values_to = "rmse") %>%
  ggplot(aes(x = rmse, y = model)) +
  geom_point(alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", size = 4, shape = 18, colour = "red") +
  labs(x = "test RMSE in each of 10 folds (red = mean)", y = NULL)

# ============================================================================
# Try it yourself
# ============================================================================
# 1. Which predictors does the LASSO keep at lambda.1se on the 55-predictor
#    matrix X55? Do the surviving interactions make sense to you?
# 2. Re-run section 5 with alpha = 0.5. Where does the elastic net land?
# 3. Change set.seed() and re-run. How much do the CV numbers move?
