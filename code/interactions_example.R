# Interactions Example: Stock Returns
# Demonstrates how the effect of one stock's returns on another 
# depends on market conditions (SPY returns)
# Uses the stock_returns.csv dataset

library(ggplot2)

# ============================================================================
# Data Description
# ============================================================================
# This dataset contains monthly stock returns for:
# - SPY: S&P 500 ETF (market proxy)
# - GOOGL: Google/Alphabet
# - AMZN: Amazon
#
# Research question: Does the relationship between tech stocks
# (e.g., GOOGL and AMZN) depend on market conditions? 
#
# We'll examine whether the effect of AMZN returns on GOOGL returns
# differs depending on whether the market (SPY) is up or down.
# This is an interaction: AMZN × SPY on GOOGL returns.

# ============================================================================
# Load the data
# ============================================================================

stock_returns <- read.csv("../data/stock_returns.csv")

# Create a binary indicator for market conditions
# High SPY = 1 (bull market), Low SPY = 0 (bear market)
median_spy <- median(stock_returns$SPY)
stock_returns$market_condition <- ifelse(stock_returns$SPY >= median_spy, 
                                         "Bull Market (High SPY)", 
                                         "Bear Market (Low SPY)")
stock_returns$market_binary <- ifelse(stock_returns$SPY >= median_spy, 1, 0)

# ============================================================================
# Model 1: No interaction (parallel lines)
# ============================================================================
model1 <- lm(GOOGL ~ AMZN + market_binary, data = stock_returns)
summary(model1)

# ============================================================================
# Model 2: With interaction (non-parallel lines)
# ============================================================================
model2 <- lm(GOOGL ~ AMZN * market_binary, data = stock_returns)
summary(model2)

# ============================================================================
# Visualization: Compare models
# ============================================================================

# Create prediction data for plotting fitted lines
amzn_range <- seq(min(stock_returns$AMZN), max(stock_returns$AMZN), length.out = 100)

# Predictions for Model 1 (no interaction - parallel lines)
pred_data1_bear <- data.frame(AMZN = amzn_range, market_binary = 0, 
                              market_condition = "Bear Market (Low SPY)")
pred_data1_bull <- data.frame(AMZN = amzn_range, market_binary = 1, 
                              market_condition = "Bull Market (High SPY)")
pred_data1_bear$GOOGL <- predict(model1, newdata = pred_data1_bear)
pred_data1_bull$GOOGL <- predict(model1, newdata = pred_data1_bull)
pred_data1 <- rbind(pred_data1_bear, pred_data1_bull)

# Predictions for Model 2 (with interaction - non-parallel lines)
pred_data2_bear <- data.frame(AMZN = amzn_range, market_binary = 0, 
                              market_condition = "Bear Market (Low SPY)")
pred_data2_bull <- data.frame(AMZN = amzn_range, market_binary = 1, 
                              market_condition = "Bull Market (High SPY)")
pred_data2_bear$GOOGL <- predict(model2, newdata = pred_data2_bear)
pred_data2_bull$GOOGL <- predict(model2, newdata = pred_data2_bull)
pred_data2 <- rbind(pred_data2_bear, pred_data2_bull)

# Plot 1: Model without interaction (parallel lines)
plot1 <- ggplot(stock_returns, aes(x = AMZN, y = GOOGL, color = market_condition)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_line(data = pred_data1, aes(x = AMZN, y = GOOGL, color = market_condition), 
            linewidth = 1.5) +
  labs(title = "Model 1: No Interaction (Parallel Lines)",
       subtitle = paste("AMZN coefficient: ", round(coef(model1)["AMZN"], 4),
                        " for both market conditions", sep = ""),
       x = "AMZN Returns",
       y = "GOOGL Returns",
       color = "Market Condition") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Bull Market (High SPY)" = "#E63946", 
                                "Bear Market (Low SPY)" = "#457B9D"))

print(plot1)

# Plot 2: Model with interaction (non-parallel lines)
# Calculate slopes for each market condition
bear_slope <- coef(model2)["AMZN"]
bull_slope <- coef(model2)["AMZN"] + coef(model2)["AMZN:market_binary"]

plot2 <- ggplot(stock_returns, aes(x = AMZN, y = GOOGL, color = market_condition)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_line(data = pred_data2, aes(x = AMZN, y = GOOGL, color = market_condition), 
            linewidth = 1.5) +
  labs(title = "Model 2: With Interaction (Non-Parallel Lines)",
       subtitle = paste("AMZN effect: Bull Market = ", round(bull_slope, 4),
                        ", Bear Market = ", round(bear_slope, 4), sep = ""),
       x = "AMZN Returns",
       y = "GOOGL Returns",
       color = "Market Condition") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Bull Market (High SPY)" = "#E63946", 
                                "Bear Market (Low SPY)" = "#457B9D"))

print(plot2)

# ============================================================================
# Model Comparison and Interpretation
# ============================================================================

# Model 1 coefficients (no interaction - parallel lines)
coef(model1)

# Model 2 coefficients (with interaction - non-parallel lines)
coef(model2)

# R-squared comparison: Does the interaction improve model fit?
summary(model1)$r.squared
summary(model2)$r.squared

# Interaction term: Is it statistically significant?
summary(model2)$coefficients["AMZN:market_binary", ]
