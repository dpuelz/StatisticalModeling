# Gradient Descent Example: fitting a simple linear regression
# Fit y = beta0 + beta1 * x by gradient descent instead of lm(), and watch it work.
# Simulated data, no file needed.
#
# Companion to the slides: neural-networks.pdf (gradient descent and the training loop)

library(ggplot2)

# ============================================================================
# The data
# ============================================================================
# 100 points from a line with intercept 2 and slope 3, plus noise.
# Because we made the data, we know the right answer. lm() gives the best
# least-squares fit, which is what gradient descent should find.

set.seed(123)
n <- 100
x <- rnorm(n, mean = 0, sd = 1)
y <- 2 + 3 * x + rnorm(n, mean = 0, sd = 1)

ols <- coef(lm(y ~ x))
ols

ggplot(data.frame(x, y), aes(x, y)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = ols[1], slope = ols[2], colour = "red") +
  labs(title = "The data and the least-squares line")

# ============================================================================
# 1. The loss, and its gradient
# ============================================================================
# Loss: mean squared error, MSE(b0, b1) = (1/n) * sum (y_i - b0 - b1 * x_i)^2
#
# Gradient (the direction of steepest increase):
#   dMSE/db0 = -(2/n) * sum (y_i - b0 - b1 * x_i)
#   dMSE/db1 = -(2/n) * sum (y_i - b0 - b1 * x_i) * x_i
#
# Gradient descent: take a small step DOWNHILL, opposite the gradient.
#   b <- b - learning_rate * gradient

mse <- function(b0, b1, x, y) mean((y - b0 - b1 * x)^2)

gradient <- function(b0, b1, x, y) {
  e <- y - b0 - b1 * x
  c(-2 * mean(e), -2 * mean(e * x))
}

# ============================================================================
# 2. Minibatch gradient descent
# ============================================================================
# Each epoch: shuffle the data, cut it into minibatches, and take one step per
# minibatch using only that minibatch's gradient. We record EVERY step.
#
# batch_size = n gives ordinary (full-batch) gradient descent: one exact step
# per epoch. Smaller batches give more, noisier steps.

fit_gd <- function(x, y, batch_size, learning_rate, epochs, start = c(0, 0)) {
  b <- start
  path <- data.frame(step = 0, epoch = 0, b0 = b[1], b1 = b[2], loss = mse(b[1], b[2], x, y))
  step <- 0
  for (epoch in 1:epochs) {
    order <- sample(length(y))
    for (first in seq(1, length(y), by = batch_size)) {
      idx <- order[first:min(first + batch_size - 1, length(y))]
      b <- b - learning_rate * gradient(b[1], b[2], x[idx], y[idx])
      step <- step + 1
      path <- rbind(path, data.frame(step = step, epoch = epoch, b0 = b[1], b1 = b[2],
                                     loss = mse(b[1], b[2], x, y)))
    }
  }
  path
}

set.seed(1)
mini <- fit_gd(x, y, batch_size = 10, learning_rate = 0.05, epochs = 10)
tail(mini, 50)
ols

# >> How close did we get? How many steps did it take?

# ============================================================================
# 3. Watch the line move
# ============================================================================

snapshots <- mini[mini$step %in% c(0, 2, 5, 10, 25, 100), ]
snapshots$label <- factor(paste("step", snapshots$step), levels = paste("step", snapshots$step))

ggplot(data.frame(x, y), aes(x, y)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_abline(intercept = ols[1], slope = ols[2], colour = "grey60", linetype = "dashed") +
  geom_abline(data = snapshots, aes(intercept = b0, slope = b1), colour = "red", linewidth = 1) +
  facet_wrap(~ label, nrow = 2) +
  labs(title = "Minibatch gradient descent, step by step",
       subtitle = "red: current fit;  dashed: lm()")

# ============================================================================
# 4. The path across the loss surface
# ============================================================================
# The MSE is a bowl over (b0, b1). Each contour line is a level of the loss.
# Compare full-batch gradient descent with minibatches of 10 and of 1.

set.seed(1)
paths <- rbind(
  data.frame(fit_gd(x, y, batch_size = n,  learning_rate = 0.05, epochs = 60), method = "full batch (n = 100)"),
  data.frame(fit_gd(x, y, batch_size = 10, learning_rate = 0.05, epochs = 6),  method = "minibatch of 10"),
  data.frame(fit_gd(x, y, batch_size = 1,  learning_rate = 0.05, epochs = 1),  method = "minibatch of 1 (SGD)"))
paths$method <- factor(paths$method, levels = unique(paths$method))

grid <- expand.grid(b0 = seq(-0.5, 3.5, length.out = 120), b1 = seq(-0.5, 4.5, length.out = 120))
grid$loss <- mapply(mse, grid$b0, grid$b1, MoreArgs = list(x = x, y = y))

ggplot(grid, aes(b0, b1)) +
  geom_contour_filled(aes(z = loss), breaks = c(0, 1, 1.5, 2, 3, 5, 8, 12, 18, 30), alpha = 0.8) +
  geom_path(data = paths, colour = "white", linewidth = 0.5) +
  geom_point(data = paths, colour = "white", size = 0.6) +
  geom_point(data = data.frame(b0 = ols[1], b1 = ols[2]), shape = 4, size = 3, stroke = 1.5, colour = "black") +
  facet_wrap(~ method, nrow = 1) +
  scale_fill_brewer(palette = "YlOrRd", direction = -1, name = "MSE") +
  coord_equal() +
  labs(x = expression(beta[0]), y = expression(beta[1]),
       title = "Every run starts at (0, 0) and heads for the least-squares fit (x)",
       subtitle = "Full batch: 60 epochs = 60 steps.  Minibatch of 10: 6 epochs = 60 steps.  SGD: 1 epoch = 100 steps.")

# >> Why does the full-batch path look smooth and the SGD path jittery?
# >> Which one reached the bottom in the fewest passes through the data?

# ============================================================================
# 5. Loss versus step
# ============================================================================

ggplot(paths, aes(step, loss, colour = method)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = mse(ols[1], ols[2], x, y), linetype = "dashed") +
  scale_y_log10() +
  labs(y = "MSE on all the data (log scale)", colour = NULL,
       title = "Training loss, step by step", subtitle = "dashed: the lm() minimum") +
  theme(legend.position = "top")

# ============================================================================
# 6. The learning rate
# ============================================================================
# Too small and it crawls; too big and it overshoots and blows up.
# >> Try learning_rate = 0.005 and 1.05 below, then plot the path as in Section 4.

set.seed(1)
slow <- fit_gd(x, y, batch_size = n, learning_rate = 0.005, epochs = 60)
tail(slow, 1)

# ============================================================================
# 7. (Optional) The loss surface in 3D
# ============================================================================
# An interactive surface you can spin. Needs the plotly package:
# install.packages("plotly")

if (requireNamespace("plotly", quietly = TRUE)) {
  b0s <- seq(-0.5, 3.5, length.out = 60)
  b1s <- seq(-0.5, 4.5, length.out = 60)
  surface <- outer(b0s, b1s, Vectorize(function(a, b) mse(a, b, x, y)))
  path10 <- paths[paths$method == "minibatch of 10", ]
  fig <- plotly::plot_ly()
  fig <- plotly::add_surface(fig, x = b0s, y = b1s, z = t(surface), opacity = 0.85, colorscale = "Viridis")
  fig <- plotly::add_trace(fig, x = path10$b0, y = path10$b1, z = path10$loss,
                           type = "scatter3d", mode = "lines+markers",
                           line = list(color = "red", width = 4), marker = list(size = 2, color = "red"),
                           name = "minibatch of 10")
  plotly::layout(fig, scene = list(xaxis = list(title = "b0"), yaxis = list(title = "b1"),
                                   zaxis = list(title = "MSE")))
}
