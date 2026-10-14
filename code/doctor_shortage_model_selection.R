# Model Selection Example: Texas physicians per capita
# Best subsets, backward stepwise, training/test sets, and cross-validation.
# Uses counties.csv
#
# Companion to the slides: model-selection.pdf

library(tidyverse)
library(leaps)   # for regsubsets(); install.packages("leaps") if needed

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
# 1. R^2 always rewards more predictors
# ============================================================================

full <- lm(PhysiciansPer10000 ~ ., data = counties)
summary(full)$r.squared       # with all 10 predictors
summary(full)$adj.r.squared   # adjusted R^2 charges for each one
AIC(full)                     # lower is better; only differences matter

# ============================================================================
# 2. Best subsets regression
# ============================================================================

# Fits every possible model and keeps the best one of each size.
subsets <- regsubsets(PhysiciansPer10000 ~ ., data = counties, nvmax = 10)
best <- summary(subsets)

best$rsq     # R^2 of the best model of each size: it never goes down
best$adjr2   # adjusted R^2: rises, peaks, then falls
which.max(best$adjr2)

# Which predictors are in the best model of each size?
best$which

# The leaps package's own picture of the same table
plot(subsets, scale = "adjr2")

# ============================================================================
# 3. Backward stepwise regression
# ============================================================================

# Start from the full model; repeatedly drop the predictor whose removal
# lowers AIC the most; stop when no removal helps. trace = 1 prints each step.
step.model <- step(full, trace = 1)
formula(step.model)

# Note: adjusted R^2 (best subsets) and AIC (stepwise) need not agree on
# the number of predictors. Different criteria, different answers.

# ============================================================================
# 4. Training and test sets
# ============================================================================

# Hold out 50 counties (about 30%) that the model never sees while fitting.
set.seed(2026)
test.cases   <- sample(1:nrow(counties), 50)
test.set     <- counties[test.cases, ]
training.set <- counties[-test.cases, ]

# IMPORTANT: run the variable selection on the TRAINING set too. Choosing
# variables using all the data and then "testing" on part of it leaks the
# test set into the model.
full.train <- lm(PhysiciansPer10000 ~ ., data = training.set)
step.train <- step(full.train, trace = 0)

# Training RMSE: the full model always wins here
rmse(training.set$PhysiciansPer10000, fitted(full.train))
rmse(training.set$PhysiciansPer10000, fitted(step.train))

# Test RMSE: the honest comparison
rmse(test.set$PhysiciansPer10000, predict(full.train, test.set))
rmse(test.set$PhysiciansPer10000, predict(step.train, test.set))

# ============================================================================
# 5. One split is noisy: repeat it 300 times
# ============================================================================

set.seed(1)
many <- map_dfr(1:300, function(i) {
  te <- sample(nrow(counties), 50)
  f  <- lm(PhysiciansPer10000 ~ ., data = counties[-te, ])
  r  <- step(f, trace = 0)
  data.frame(full = rmse(counties$PhysiciansPer10000[te], predict(f, counties[te, ])),
             step = rmse(counties$PhysiciansPer10000[te], predict(r, counties[te, ])))
})

# How often does the stepwise model beat the full model on the test set?
mean(many$step < many$full)

ggplot(many, aes(x = full - step)) +
  geom_histogram(binwidth = 0.01) +
  geom_vline(xintercept = 0, colour = "red") +
  labs(x = "test RMSE of full model minus test RMSE of stepwise model",
       y = "splits")

# ============================================================================
# 6. 10-fold cross-validation
# ============================================================================

# Every county is in the test fold exactly once. Stepwise is re-run inside
# each fold, so the held-out fold never influences which variables are chosen.
set.seed(1)
folds <- sample(rep(1:10, length.out = nrow(counties)))

cv <- sapply(1:10, function(f) {
  train <- counties[folds != f, ]
  test  <- counties[folds == f, ]
  m     <- lm(PhysiciansPer10000 ~ ., data = train)
  c(full = rmse(test$PhysiciansPer10000, predict(m, test)),
    step = rmse(test$PhysiciansPer10000, predict(step(m, trace = 0), test)))
})
rowMeans(cv)

# Essentially a tie: with 168 counties and 10 predictors there is little room
# to overfit. See penalized_regression_counties.R for what happens with 55.

# ============================================================================
# Try it yourself
# ============================================================================
# 1. Run step() with direction = "forward", starting from
#    lm(PhysiciansPer10000 ~ 1, data = counties) and scope = formula(full).
#    Does it land on the same model as backward elimination?
# 2. Change the seed in section 4. Does the test-set winner change?
# 3. Add PctRural^2 (I(PctRural^2)) to the full model. Does stepwise keep it?
