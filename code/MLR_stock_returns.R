# Multiple Linear Regression Example: Stock Returns
# This script demonstrates how controlling for confounding variables
# (in this case, the market, SPY) changes the interpretation of coefficients
# in multiple regression.

# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Load the data
stock_returns <- read.csv("../data/stock_returns.csv")

# ============================================================================
# PART 1: Fit the regression models
# ============================================================================

# Model 1: Simple regression of GOOGL on KO
model1_simple <- lm(GOOGL ~ KO, data = stock_returns)
summary(model1_simple)

# Model 2: Multiple regression of GOOGL on KO and SPY
model2_multiple <- lm(GOOGL ~ KO + SPY, data = stock_returns)
summary(model2_multiple)

# ============================================================================
# PART 2: Compare the coefficients
# ============================================================================

cat("\n=== COEFFICIENT COMPARISON ===\n")
cat("Simple regression (GOOGL ~ KO):\n")
cat("  KO coefficient:", round(coef(model1_simple)["KO"], 3), "\n")
cat("\nMultiple regression (GOOGL ~ KO + SPY):\n")
cat("  KO coefficient:", round(coef(model2_multiple)["KO"], 3), "\n")
cat("  SPY coefficient:", round(coef(model2_multiple)["SPY"], 3), "\n")

cat("\n=== INTERPRETATION ===\n")
cat("In the simple regression, the KO coefficient is", 
    ifelse(coef(model1_simple)["KO"] > 0, "positive", "negative"), 
    "(", round(coef(model1_simple)["KO"], 3), "),\n")
cat("suggesting that when KO returns increase, GOOGL returns also increase.\n\n")

cat("However, in the multiple regression, when we control for SPY (market returns),\n")
cat("the KO coefficient becomes", 
    ifelse(coef(model2_multiple)["KO"] > 0, "positive", "negative"), 
    "(", round(coef(model2_multiple)["KO"], 3), ").\n")
cat("This change occurs because KO and SPY are correlated - both stocks\n")
cat("tend to move with the overall market. Once we control for market movements,\n")
cat("the relationship between KO and GOOGL changes.\n\n")

# ============================================================================
# PART 3: Visualize the relationships
# ============================================================================

# Create bins for SPY values to color points
# We'll divide SPY into approximately equal-sized groups
stock_returns$SPY_group <- cut(stock_returns$SPY, 
                               breaks = quantile(stock_returns$SPY, 
                                                 probs = seq(0, 1, length = 4)),
                               labels = c("Low SPY", "Medium SPY", "High SPY"),
                               include.lowest = TRUE)

# Plot 1: KO vs SPY (showing their correlation)
plot1 <- ggplot(stock_returns, aes(x = KO, y = SPY, color = SPY_group)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  labs(title = "KO vs SPY",
       subtitle = "Showing correlation between KO and market returns",
       x = "Coca-Cola (KO) Returns",
       y = "S&P 500 ETF (SPY) Returns",
       color = "SPY Level") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 2: GOOGL vs KO, colored by SPY levels
plot2 <- ggplot(stock_returns, aes(x = KO, y = GOOGL, color = SPY_group)) +
  geom_point(alpha = 0.7, size = 2) +
  # Add regression lines for each SPY group to show the relationship
  geom_smooth(method = "lm", se = FALSE, aes(group = SPY_group)) +
  # Add overall regression line from simple model (for comparison)
  geom_abline(intercept = coef(model1_simple)["(Intercept)"], 
              slope = coef(model1_simple)["KO"],
              color = "black", linetype = "dashed", linewidth = 1) +
  labs(title = "GOOGL vs KO (colored by SPY level)",
       subtitle = paste("Simple regression line (dashed).",
                        "Controlling for SPY, KO coefficient changes from",
                        round(coef(model1_simple)["KO"], 3), "to",
                        round(coef(model2_multiple)["KO"], 3)),
       x = "Coca-Cola (KO) Returns",
       y = "Google (GOOGL) Returns",
       color = "SPY Level") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display plots side by side
grid.arrange(plot1, plot2, ncol = 2)

# ============================================================================
# PART 4: Additional insights
# ============================================================================

cat("\n=== CORRELATION ANALYSIS ===\n")
cat("Correlation between KO and SPY:", 
    round(cor(stock_returns$KO, stock_returns$SPY), 3), "\n")
cat("Correlation between GOOGL and SPY:", 
    round(cor(stock_returns$GOOGL, stock_returns$SPY), 3), "\n")
cat("Correlation between KO and GOOGL:", 
    round(cor(stock_returns$KO, stock_returns$GOOGL), 3), "\n")

cat("\nThe positive correlation between KO and SPY (", 
    round(cor(stock_returns$KO, stock_returns$SPY), 3), 
    ") means both stocks\n")
cat("tend to move with the market. This confounds the relationship between\n")
cat("KO and GOOGL in the simple regression.\n")
