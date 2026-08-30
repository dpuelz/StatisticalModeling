# ============================================================================
# Probability You'll Need for Statistical Modeling
# STM 2102 — University of Austin
# ============================================================================
#
# This is NOT a probability-theory course. We use probability as the language
# of uncertainty around regression. Five ideas only:
#
#   1. Randomness = noise around a systematic pattern
#   2. A distribution = where outcomes tend to land (Normal = workhorse)
#   3. Mean / SD = center and spread
#   4. ~95% of Normal mass lies within ~2 SDs  →  CIs and residual checks
#   5. A p-value asks: "how surprising is this if there's really no effect?"
#
# Philosophy: simulate first, then name the idea.
# ============================================================================

set.seed(42)
par(mar = c(4.2, 4.2, 2.5, 1))


# ============================================================================
# 1. RANDOMNESS = NOISE AROUND A PATTERN
# ============================================================================
#
# In this course, "random" almost always means: there is a predictable part
# (the signal / systematic pattern) plus an unpredictable part (the noise).

# True relationship: study hours → exam score, plus noise
n <- 80
hours <- runif(n, 0, 10)
true_score <- 55 + 4 * hours          # systematic part: line
noise <- rnorm(n, mean = 0, sd = 8)    # idiosyncratic part: random noise
score <- true_score + noise

plot(hours, score, pch = 19, col = rgb(0.2, 0.4, 0.7, 0.7),
     xlab = "Study hours / week", ylab = "Exam score",
     main = "Signal + noise: the regression worldview")
abline(a = 55, b = 4, col = "firebrick", lwd = 2)
legend("topleft", bty = "n",
       legend = c("Data (signal + noise)", "True pattern: score = 55 + 4·hours"),
       col = c(rgb(0.2, 0.4, 0.7, 0.7), "firebrick"),
       pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2))

# KEY: Probability describes the noise. Regression estimates the pattern.


# ============================================================================
# 2. A DISTRIBUTION = WHERE OUTCOMES TEND TO LAND
# ============================================================================
#
# A distribution tells you which values are common and which are rare.
# Our workhorse: the Normal (Gaussian) distribution.

draws <- rnorm(5000, mean = 0, sd = 1)

hist(draws, breaks = 40, probability = TRUE, col = "steelblue", border = "white",
     main = "The Normal distribution (workhorse)",
     xlab = "Value", ylab = "Density")
curve(dnorm(x, 0, 1), add = TRUE, col = "firebrick", lwd = 2)
legend("topright", bty = "n",
       legend = c("Simulated draws", "Normal(0,1) curve"),
       fill = c("steelblue", NA), border = c("white", NA),
       lty = c(NA, 1), col = c(NA, "firebrick"), lwd = c(NA, 2))

# In regression we will assume:  errors ε ~ Normal(0, σ²)
# That one assumption unlocks CIs, tests, and prediction intervals.


# ============================================================================
# 3. MEAN AND SD = CENTER AND SPREAD
# ============================================================================

mu <- 70
sigma <- 10
exam <- rnorm(2000, mean = mu, sd = sigma)

cat("=== Center and spread ===\n")
cat("True mean (μ):", mu, "   sample mean:", round(mean(exam), 2), "\n")
cat("True SD  (σ):", sigma, "  sample SD:  ", round(sd(exam), 2), "\n\n")

# Same mean, different spread — spread is what uncertainty "feels like"
tight <- rnorm(2000, mean = 70, sd = 3)
wide  <- rnorm(2000, mean = 70, sd = 15)

par(mfrow = c(1, 2))
hist(tight, breaks = 30, probability = TRUE, col = "darkseagreen3", border = "white",
     xlim = c(20, 120), main = "Small SD (precise)", xlab = "Score")
abline(v = 70, col = "firebrick", lwd = 2)
hist(wide, breaks = 30, probability = TRUE, col = "lightcoral", border = "white",
     xlim = c(20, 120), main = "Large SD (noisy)", xlab = "Score")
abline(v = 70, col = "firebrick", lwd = 2)
par(mfrow = c(1, 1))

# KEY: Mean = where the cloud sits. SD = how fat the cloud is.
# In regression, residual SD (s_e) is the typical size of prediction errors.


# ============================================================================
# 4. ~95% OF NORMAL MASS LIES WITHIN ~2 SDs
# ============================================================================
#
# This one rule of thumb powers confidence intervals AND residual diagnostics.

within_1sd <- mean(abs(exam - mu) < 1 * sigma)
within_2sd <- mean(abs(exam - mu) < 2 * sigma)
within_3sd <- mean(abs(exam - mu) < 3 * sigma)

cat("=== Empirical coverage (should be ~68% / 95% / 99.7%) ===\n")
cat("Within 1 SD:", round(100 * within_1sd, 1), "%\n")
cat("Within 2 SD:", round(100 * within_2sd, 1), "%\n")
cat("Within 3 SD:", round(100 * within_3sd, 1), "%\n\n")

hist(exam, breaks = 40, probability = TRUE, col = "steelblue", border = "white",
     main = "Why \"±2 SD\" shows up everywhere",
     xlab = "Exam score")
curve(dnorm(x, mu, sigma), add = TRUE, col = "firebrick", lwd = 2)
abline(v = mu + c(-2, 2) * sigma, col = "darkorange", lwd = 2, lty = 2)
legend("topright", bty = "n",
       legend = c("Normal curve", "mu +/- 2 sd  (~95% of mass)"),
       col = c("firebrick", "darkorange"), lty = c(1, 2), lwd = 2)

# Connection to THIS course:
#   • A 95% CI for a slope is roughly  estimate ± 2 · SE(estimate)
#   • If residuals are roughly Normal, ~95% should fall within ± 2 · s_e
#   • If many residuals sit outside that band, something is off

# Tiny demo of the CI rule of thumb
n_ci <- 40
x_ci <- runif(n_ci, 0, 10)
y_ci <- 55 + 4 * x_ci + rnorm(n_ci, 0, 8)
fit_ci <- lm(y_ci ~ x_ci)
b1 <- coef(fit_ci)[2]
se <- summary(fit_ci)$coef[2, 2]
ci_approx <- b1 + c(-2, 2) * se
ci_exact  <- confint(fit_ci, "x_ci")

cat("=== 95% CI for the slope (rule of thumb vs R) ===\n")
cat("Estimate ± 2·SE: [", round(ci_approx[1], 3), ",", round(ci_approx[2], 3), "]\n")
cat("confint() exact: [", round(ci_exact[1], 3), ",", round(ci_exact[2], 3), "]\n\n")


# ============================================================================
# 5. p-VALUES: HOW SURPRISING UNDER "NO EFFECT"?
# ============================================================================
#
# A p-value is NOT "the probability the null is true."
# It IS: assuming the null is true, how often would chance alone produce
# a result as extreme as (or more extreme than) what we saw?

# Setup: true slope is ZERO (null is true). We still get noisy slope estimates.
true_beta1 <- 0
n_obs <- 50
n_sims <- 2000

sim_slopes <- replicate(n_sims, {
  x <- runif(n_obs, 0, 10)
  y <- 50 + true_beta1 * x + rnorm(n_obs, 0, 8)
  coef(lm(y ~ x))[2]
})

# One "observed" study from the same null world
x_obs <- runif(n_obs, 0, 10)
y_obs <- 50 + true_beta1 * x_obs + rnorm(n_obs, 0, 8)
obs_fit <- lm(y_obs ~ x_obs)
obs_slope <- coef(obs_fit)[2]
obs_p <- summary(obs_fit)$coef[2, 4]

# Simulated two-sided p-value: how often |slope| ≥ |observed| under the null?
sim_p <- mean(abs(sim_slopes) >= abs(obs_slope))

hist(sim_slopes, breaks = 40, col = "gray85", border = "white",
     main = "Sampling distribution of slope when true beta1 = 0",
     xlab = "Estimated slope")
abline(v = obs_slope, col = "firebrick", lwd = 2)
abline(v = -obs_slope, col = "firebrick", lwd = 2, lty = 2)
legend("topright", bty = "n",
       legend = c("Observed slope", "Equally extreme (other side)"),
       col = "firebrick", lty = c(1, 2), lwd = 2)

cat("=== p-value intuition ===\n")
cat("Observed slope:", round(obs_slope, 3), "\n")
cat("Simulated p (fraction of null slopes at least this extreme):",
    round(sim_p, 3), "\n")
cat("R's p-value from lm():", round(obs_p, 3), "\n")
cat("(They won't match exactly — one dataset vs many sims — but same idea.)\n\n")

# KEY: Small p → data would be unusual if there were truly no effect.
#       That is permission to doubt the null — not proof of an effect.


# ============================================================================
# BRIDGE: THE REGRESSION PROBABILITY MODEL
# ============================================================================
#
# Everything above is so we can write, and believe:
#
#   Y = β₀ + β₁ X + ε ,   ε ~ Normal(0, σ²)
#
# Equivalently:  Y | X  ~  Normal(β₀ + β₁ X, σ²)
#
# That model is why Week 2 can do confidence intervals, hypothesis tests,
# and prediction intervals for regression.

n <- 100
X <- runif(n, 0, 10)
Y <- 55 + 4 * X + rnorm(n, 0, 8)
fit <- lm(Y ~ X)

plot(X, Y, pch = 19, col = rgb(0.2, 0.4, 0.7, 0.55),
     xlab = "X", ylab = "Y",
     main = "Probability model: Normal noise around a line")
abline(fit, col = "firebrick", lwd = 2)

# Band of typical residuals: ± 2 · residual SD
se_resid <- summary(fit)$sigma
xs <- seq(min(X), max(X), length.out = 100)
ys <- coef(fit)[1] + coef(fit)[2] * xs
lines(xs, ys + 2 * se_resid, col = "darkorange", lty = 2, lwd = 2)
lines(xs, ys - 2 * se_resid, col = "darkorange", lty = 2, lwd = 2)
legend("topleft", bty = "n",
       legend = c("Fitted line", "~95% residual band (+/- 2 s_e)"),
       col = c("firebrick", "darkorange"), lty = c(1, 2), lwd = 2)

cat("=== Fitted model ===\n")
print(summary(fit)$coef)
cat("Residual SD (s_e):", round(summary(fit)$sigma, 2), "\n")
cat("R²:", round(summary(fit)$r.squared, 3), "\n\n")


# ============================================================================
# TAKEAWAYS (say these out loud on Monday)
# ============================================================================
#
# 1. Randomness in this course = noise around a pattern.
# 2. Distributions describe where outcomes land; Normal is the default for errors.
# 3. Mean = center, SD = spread (uncertainty).
# 4. ≈95% of Normal mass is within ±2 SD → CI rule of thumb and residual checks.
# 5. A p-value measures surprise under "no effect" — not P(null is true).
#
# Next up: inference for regression (Week 2) puts these to work on slopes.
# ============================================================================
