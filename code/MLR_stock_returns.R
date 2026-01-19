# Multiple Linear Regression Example: Stock Returns
# Demonstrates how controlling for SPY (market) changes the GOOGL-KO relationship
# Run this code line by line to see SPY buckets colored in progressively

library(ggplot2)
library(gridExtra)

# Load the data
stock_returns <- read.csv("../data/stock_returns.csv")

# Fit models
model1_simple <- lm(GOOGL ~ KO, data = stock_returns)
model2_multiple <- lm(GOOGL ~ KO + SPY, data = stock_returns)

# ============================================================================
# Create SPY buckets (groups of similar SPY values)
# ============================================================================

# Divide SPY into buckets (e.g., 5 buckets)
n_buckets <- 5
stock_returns$SPY_bucket <- cut(stock_returns$SPY, 
                                breaks = quantile(stock_returns$SPY, 
                                                  probs = seq(0, 1, length = n_buckets + 1)),
                                labels = paste("Bucket", 1:n_buckets),
                                include.lowest = TRUE)

# Get bucket levels
bucket_levels <- levels(stock_returns$SPY_bucket)

# Color palette for buckets
bucket_colors <- c("steelblue", "darkgreen", "orange", "purple", "red")

# ============================================================================
# PLOT: Progressive addition of SPY buckets
# Run each section separately to see buckets added one by one
# ============================================================================

# Bucket 1: First SPY bucket
bucket1_data <- stock_returns[stock_returns$SPY_bucket == bucket_levels[1], ]

# Left plot: SPY vs KO
plot1_left <- ggplot(stock_returns, aes(x = KO, y = SPY)) +
  geom_point(alpha = 0.2, color = "gray80", size = 1) +
  geom_point(data = bucket1_data, aes(color = SPY_bucket), alpha = 0.7, size = 2) +
  geom_hline(yintercept = mean(bucket1_data$SPY), 
             linetype = "dashed", color = "black", linewidth = 1, alpha = 0.5) +
  scale_color_manual(values = bucket_colors[1], name = "SPY Bucket") +
  labs(title = "SPY vs KO",
       subtitle = "Bucket 1",
       x = "KO Returns",
       y = "SPY Returns") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Right plot: GOOGL vs KO
plot1_right <- ggplot(stock_returns, aes(x = KO, y = GOOGL)) +
  geom_point(alpha = 0.2, color = "gray80", size = 1) +
  geom_point(data = bucket1_data, aes(color = SPY_bucket), alpha = 0.7, size = 2) +
  geom_smooth(data = bucket1_data, method = "lm", se = FALSE, 
              aes(color = SPY_bucket), linewidth = 1.2) +
  geom_abline(intercept = coef(model1_simple)["(Intercept)"], 
              slope = coef(model1_simple)["KO"],
              color = "black", linetype = "dashed", linewidth = 1, alpha = 0.5) +
  scale_color_manual(values = bucket_colors[1], name = "SPY Bucket") +
  labs(title = "GOOGL vs KO",
       subtitle = paste("Bucket 1. Multiple regression KO coefficient:", 
                        round(coef(model2_multiple)["KO"], 3)),
       x = "KO Returns",
       y = "GOOGL Returns") +
  theme_minimal() +
  theme(legend.position = "bottom")

plot1 <- grid.arrange(plot1_left, plot1_right, ncol = 2)
print(plot1)

# Bucket 2: Add second bucket
bucket2_data <- stock_returns[stock_returns$SPY_bucket == bucket_levels[2], ]
plot_data2 <- rbind(bucket1_data, bucket2_data)

# Left plot: SPY vs KO
plot2_left <- ggplot(stock_returns, aes(x = KO, y = SPY)) +
  geom_point(alpha = 0.2, color = "gray80", size = 1) +
  geom_point(data = plot_data2, aes(color = SPY_bucket), alpha = 0.7, size = 2) +
  scale_color_manual(values = bucket_colors[1:2], name = "SPY Bucket") +
  labs(title = "SPY vs KO",
       subtitle = "Buckets 1-2",
       x = "KO Returns",
       y = "SPY Returns") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Right plot: GOOGL vs KO
plot2_right <- ggplot(stock_returns, aes(x = KO, y = GOOGL)) +
  geom_point(alpha = 0.2, color = "gray80", size = 1) +
  geom_point(data = plot_data2, aes(color = SPY_bucket), alpha = 0.7, size = 2) +
  geom_smooth(data = plot_data2, method = "lm", se = FALSE, 
              aes(color = SPY_bucket), linewidth = 1.2) +
  geom_abline(intercept = coef(model1_simple)["(Intercept)"], 
              slope = coef(model1_simple)["KO"],
              color = "black", linetype = "dashed", linewidth = 1, alpha = 0.5) +
  scale_color_manual(values = bucket_colors[1:2], name = "SPY Bucket") +
  labs(title = "GOOGL vs KO",
       subtitle = paste("Buckets 1-2. Multiple regression KO coefficient:", 
                        round(coef(model2_multiple)["KO"], 3)),
       x = "KO Returns",
       y = "GOOGL Returns") +
  theme_minimal() +
  theme(legend.position = "bottom")

plot2 <- grid.arrange(plot2_left, plot2_right, ncol = 2)
print(plot2)

# Bucket 3: Add third bucket
bucket3_data <- stock_returns[stock_returns$SPY_bucket == bucket_levels[3], ]
plot_data3 <- rbind(bucket1_data, bucket2_data, bucket3_data)

# Left plot: SPY vs KO
plot3_left <- ggplot(stock_returns, aes(x = KO, y = SPY)) +
  geom_point(alpha = 0.2, color = "gray80", size = 1) +
  geom_point(data = plot_data3, aes(color = SPY_bucket), alpha = 0.7, size = 2) +
  scale_color_manual(values = bucket_colors[1:3], name = "SPY Bucket") +
  labs(title = "SPY vs KO",
       subtitle = "Buckets 1-3",
       x = "KO Returns",
       y = "SPY Returns") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Right plot: GOOGL vs KO
plot3_right <- ggplot(stock_returns, aes(x = KO, y = GOOGL)) +
  geom_point(alpha = 0.2, color = "gray80", size = 1) +
  geom_point(data = plot_data3, aes(color = SPY_bucket), alpha = 0.7, size = 2) +
  geom_smooth(data = plot_data3, method = "lm", se = FALSE, 
              aes(color = SPY_bucket), linewidth = 1.2) +
  geom_abline(intercept = coef(model1_simple)["(Intercept)"], 
              slope = coef(model1_simple)["KO"],
              color = "black", linetype = "dashed", linewidth = 1, alpha = 0.5) +
  scale_color_manual(values = bucket_colors[1:3], name = "SPY Bucket") +
  labs(title = "GOOGL vs KO",
       subtitle = paste("Buckets 1-3. Multiple regression KO coefficient:", 
                        round(coef(model2_multiple)["KO"], 3)),
       x = "KO Returns",
       y = "GOOGL Returns") +
  theme_minimal() +
  theme(legend.position = "bottom")

plot3 <- grid.arrange(plot3_left, plot3_right, ncol = 2)
print(plot3)

# Bucket 4: Add fourth bucket
bucket4_data <- stock_returns[stock_returns$SPY_bucket == bucket_levels[4], ]
plot_data4 <- rbind(bucket1_data, bucket2_data, bucket3_data, bucket4_data)

# Left plot: SPY vs KO
plot4_left <- ggplot(stock_returns, aes(x = KO, y = SPY)) +
  geom_point(alpha = 0.2, color = "gray80", size = 1) +
  geom_point(data = plot_data4, aes(color = SPY_bucket), alpha = 0.7, size = 2) +
  scale_color_manual(values = bucket_colors[1:4], name = "SPY Bucket") +
  labs(title = "SPY vs KO",
       subtitle = "Buckets 1-4",
       x = "KO Returns",
       y = "SPY Returns") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Right plot: GOOGL vs KO
plot4_right <- ggplot(stock_returns, aes(x = KO, y = GOOGL)) +
  geom_point(alpha = 0.2, color = "gray80", size = 1) +
  geom_point(data = plot_data4, aes(color = SPY_bucket), alpha = 0.7, size = 2) +
  geom_smooth(data = plot_data4, method = "lm", se = FALSE, 
              aes(color = SPY_bucket), linewidth = 1.2) +
  geom_abline(intercept = coef(model1_simple)["(Intercept)"], 
              slope = coef(model1_simple)["KO"],
              color = "black", linetype = "dashed", linewidth = 1, alpha = 0.5) +
  scale_color_manual(values = bucket_colors[1:4], name = "SPY Bucket") +
  labs(title = "GOOGL vs KO",
       subtitle = paste("Buckets 1-4. Multiple regression KO coefficient:", 
                        round(coef(model2_multiple)["KO"], 3)),
       x = "KO Returns",
       y = "GOOGL Returns") +
  theme_minimal() +
  theme(legend.position = "bottom")

plot4 <- grid.arrange(plot4_left, plot4_right, ncol = 2)
print(plot4)

# Bucket 5: Add fifth bucket (all buckets)
bucket5_data <- stock_returns[stock_returns$SPY_bucket == bucket_levels[5], ]
plot_data5 <- rbind(bucket1_data, bucket2_data, bucket3_data, bucket4_data, bucket5_data)

# Left plot: SPY vs KO
plot5_left <- ggplot(stock_returns, aes(x = KO, y = SPY)) +
  geom_point(alpha = 0.2, color = "gray80", size = 1) +
  geom_point(data = plot_data5, aes(color = SPY_bucket), alpha = 0.7, size = 2) +
  scale_color_manual(values = bucket_colors[1:5], name = "SPY Bucket") +
  labs(title = "SPY vs KO",
       subtitle = "All buckets",
       x = "KO Returns",
       y = "SPY Returns") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Right plot: GOOGL vs KO
plot5_right <- ggplot(stock_returns, aes(x = KO, y = GOOGL)) +
  geom_point(alpha = 0.2, color = "gray80", size = 1) +
  geom_point(data = plot_data5, aes(color = SPY_bucket), alpha = 0.7, size = 2) +
  geom_smooth(data = plot_data5, method = "lm", se = FALSE, 
              aes(color = SPY_bucket), linewidth = 1.2) +
  geom_abline(intercept = coef(model1_simple)["(Intercept)"], 
              slope = coef(model1_simple)["KO"],
              color = "black", linetype = "dashed", linewidth = 1, alpha = 0.5) +
  scale_color_manual(values = bucket_colors[1:5], name = "SPY Bucket") +
  labs(title = "GOOGL vs KO",
       subtitle = paste("All buckets. Multiple regression KO coefficient:", 
                        round(coef(model2_multiple)["KO"], 3)),
       x = "KO Returns",
       y = "GOOGL Returns") +
  theme_minimal() +
  theme(legend.position = "bottom")

plot5 <- grid.arrange(plot5_left, plot5_right, ncol = 2)
print(plot5)

# ============================================================================
# Summary: Show slopes for each bucket
# ============================================================================

cat("\n=== SLOPES FOR EACH SPY BUCKET ===\n")
for(i in 1:n_buckets) {
  bucket_data <- stock_returns[stock_returns$SPY_bucket == bucket_levels[i], ]
  if(nrow(bucket_data) > 1) {
    bucket_model <- lm(GOOGL ~ KO, data = bucket_data)
    cat("Bucket", i, "slope:", round(coef(bucket_model)["KO"], 3), "\n")
  }
}
cat("\nMultiple regression KO coefficient:", round(coef(model2_multiple)["KO"], 3), "\n")
