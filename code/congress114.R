# Penalized logistic regression: Predicting party from congressional speech (114th Congress)
# Identical structure to congress109.R but uses 114th Congress data (2015-2017)
# Required packages: glmnet, plotmo
# Run from the StatisticalModeling/code directory
# Data: congress114.csv (phrase counts), congress114members.csv (party, etc.)
#
# To obtain data: See data/congress114_README.md for instructions on downloading
# and processing the Stanford Congressional Record dataset (43rd-114th Congresses).

library(glmnet)
library(plotmo)

##
# Load data
##
countdata <- read.csv("../data/congress114.csv", header = TRUE, row.names = 1)
memberdata <- read.csv("../data/congress114members.csv", header = TRUE, row.names = 1)

stopifnot(all(rownames(countdata) == rownames(memberdata)))

# Exclude Independents for clean binary classification (R vs D)
keep <- memberdata$party %in% c("R", "D")
countdata <- countdata[keep, ]
memberdata <- memberdata[keep, ]
table(memberdata$party)

# Outcome: 1 = Republican, 0 = Democrat
y <- as.numeric(memberdata$party == "R")

# Predictors: document-term matrix (phrase counts in speeches)
X <- as.matrix(countdata)
dim(X)

# Center and scale predictors
X_scaled <- scale(X)
const_cols <- apply(X_scaled, 2, function(z) sd(z, na.rm = TRUE) == 0)
if (any(const_cols)) X_scaled <- X_scaled[, !const_cols]

##
# cv.glmnet does 10-fold CV internally to select lambda. No separate train/test
# needed---the CV error is our performance estimate.
##
ridge_cv <- cv.glmnet(X_scaled, y, family = "binomial", alpha = 0,
                      type.measure = "class", nfolds = 10)
ridge_fit <- glmnet(X_scaled, y, family = "binomial", alpha = 0,
                    lambda = ridge_cv$lambda.min)
min(ridge_cv$cvm)
sum(coef(ridge_fit)[-1] != 0)

lasso_cv <- cv.glmnet(X_scaled, y, family = "binomial", alpha = 1,
                      type.measure = "class", nfolds = 10)
lasso_cv$lambda.min
lasso_cv$lambda.1se

lasso_fit_min <- glmnet(X_scaled, y, family = "binomial", alpha = 1,
                        lambda = lasso_cv$lambda.min)
lasso_fit_1se <- glmnet(X_scaled, y, family = "binomial", alpha = 1,
                        lambda = lasso_cv$lambda.1se)
sum(coef(lasso_fit_min)[-1] != 0)
sum(coef(lasso_fit_1se)[-1] != 0)
min(lasso_cv$cvm)

##
# Coefficient paths
##
par(mar = c(4, 4, 2, 10), cex.main = 0.9)
plot_glmnet(ridge_cv$glmnet.fit, xvar = "lambda", main = "Ridge: coefficients vs log(lambda)")
plot_glmnet(lasso_cv$glmnet.fit, xvar = "lambda", main = "LASSO: coefficients vs log(lambda)")
abline(v = log(lasso_cv$lambda.1se), lty = 2, col = "gray")

##
# Top phrases predictive of R vs D (positive = Republican, negative = Democrat)
##
beta <- coef(lasso_fit_1se)
beta_vec <- as.vector(beta[-1])
names(beta_vec) <- colnames(X_scaled)
nonzero <- beta_vec != 0

sort(beta_vec[nonzero & beta_vec > 0], decreasing = TRUE)[1:15]
sort(beta_vec[nonzero & beta_vec < 0])[1:15]
