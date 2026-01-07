# ============================================================================
# LEARNING PROBABILITY THROUGH SIMULATION
# A Journey from Basic Probability to Regression
# Statistical Modeling - The University of Austin
# ============================================================================

# Philosophy: The best way to understand probability is to SIMULATE.
# Instead of memorizing formulas, we'll discover them through experimentation.

# ============================================================================
# PART I: WHAT IS PROBABILITY?
# ============================================================================

# ----------------------------------------------------------------------------
# 1.1 The Frequency Interpretation
# ----------------------------------------------------------------------------

# Probability = long-run frequency of an event
# If we repeat an experiment many times, how often does something happen?

# Let's flip a coin. What's the probability of heads?
set.seed(42)

# Flip a coin 10 times
flips_10 <- sample(c("H", "T"), size = 10, replace = TRUE)
flips_10
sum(flips_10 == "H") / 10  # Proportion of heads

# Flip 100 times
flips_100 <- sample(c("H", "T"), size = 100, replace = TRUE)
sum(flips_100 == "H") / 100

# Flip 10,000 times
flips_10000 <- sample(c("H", "T"), size = 10000, replace = TRUE)
sum(flips_10000 == "H") / 10000  # Much closer to 0.5!

# Watch probability "stabilize" as we flip more coins
n_flips <- 1:5000
cumulative_heads <- cumsum(sample(c(0, 1), 5000, replace = TRUE))
running_proportion <- cumulative_heads / n_flips

plot(n_flips, running_proportion, type = "l", col = "steelblue", lwd = 2,
     xlab = "Number of Flips", ylab = "Proportion of Heads",
     main = "Probability Stabilizes with More Data")
abline(h = 0.5, col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("Running proportion", "True probability (0.5)"),
       col = c("steelblue", "red"), lty = c(1, 2), lwd = 2, bty = "n")

# KEY INSIGHT: Probability is what the proportion CONVERGES to as n -> infinity

# ----------------------------------------------------------------------------
# 1.2 Sample Spaces and Events
# ----------------------------------------------------------------------------

# Sample space (Omega) = set of all possible outcomes
# Event = subset of the sample space

# Example: Rolling a die
sample_space_die <- 1:6
cat("Sample space for a die:", sample_space_die, "\n")

# Events are subsets
event_even <- c(2, 4, 6)         # Rolling an even number
event_greater_than_4 <- c(5, 6)  # Rolling greater than 4

# Probability = (# favorable outcomes) / (# total outcomes)  [for equally likely outcomes]
P_even <- length(event_even) / length(sample_space_die)
cat("P(even) =", P_even, "\n")

# Let's verify by simulation!
die_rolls <- sample(1:6, size = 100000, replace = TRUE)
simulated_P_even <- mean(die_rolls %% 2 == 0)
cat("Simulated P(even) =", simulated_P_even, "\n")


# ----------------------------------------------------------------------------
# 1.3 Complement, Union, and Intersection
# ----------------------------------------------------------------------------

# P(not A) = 1 - P(A)
# P(A or B) = P(A) + P(B) - P(A and B)
# P(A and B) = P(A) * P(B)  [if independent]

# Let's verify the addition rule with simulation
# Event A: Roll a 1 or 2
# Event B: Roll a 2 or 3

n_sims <- 100000
rolls <- sample(1:6, n_sims, replace = TRUE)

A <- rolls %in% c(1, 2)
B <- rolls %in% c(2, 3)

P_A <- mean(A)
P_B <- mean(B)
P_A_and_B <- mean(A & B)  # Intersection (roll a 2)
P_A_or_B <- mean(A | B)   # Union (roll 1, 2, or 3)

cat("\nAddition Rule Verification:\n")
cat("P(A) =", round(P_A, 4), "(theory: 2/6 =", round(2/6, 4), ")\n")
cat("P(B) =", round(P_B, 4), "(theory: 2/6 =", round(2/6, 4), ")\n")
cat("P(A and B) =", round(P_A_and_B, 4), "(theory: 1/6 =", round(1/6, 4), ")\n")
cat("P(A or B) =", round(P_A_or_B, 4), "(theory: 3/6 =", round(3/6, 4), ")\n")
cat("P(A) + P(B) - P(A and B) =", round(P_A + P_B - P_A_and_B, 4), "\n")


# ============================================================================
# PART II: CONDITIONAL PROBABILITY
# ============================================================================

# ----------------------------------------------------------------------------
# 2.1 What is Conditional Probability?
# ----------------------------------------------------------------------------

# P(A|B) = "Probability of A given that B occurred"
# P(A|B) = P(A and B) / P(B)

# Intuition: We RESTRICT our sample space to only outcomes where B happened,
# then ask how often A happens within that restricted space.

# Example: Two dice
n_sims <- 100000
die1 <- sample(1:6, n_sims, replace = TRUE)
die2 <- sample(1:6, n_sims, replace = TRUE)
total <- die1 + die2

# What's P(total = 8)?
P_total_8 <- mean(total == 8)
cat("\nP(total = 8) =", round(P_total_8, 4), "(theory: 5/36 =", round(5/36, 4), ")\n")

# What's P(total = 8 | die1 = 3)?
# Given that the first die is 3, we need die2 = 5 for total = 8
# So P(total=8 | die1=3) = 1/6

# Simulation: Filter to cases where die1 = 3
given_die1_is_3 <- (die1 == 3)
P_total_8_given_die1_3 <- mean(total[given_die1_is_3] == 8)
cat("P(total = 8 | die1 = 3) =", round(P_total_8_given_die1_3, 4), 
    "(theory: 1/6 =", round(1/6, 4), ")\n")

# Visualize conditional probability
par(mfrow = c(1, 2))

# Unconditional distribution of totals
hist(total, breaks = seq(1.5, 12.5, by = 1), col = "lightblue",
     main = "Distribution of Sum (Unconditional)", xlab = "Sum of Two Dice",
     probability = TRUE)

# Conditional distribution given die1 = 3
hist(total[given_die1_is_3], breaks = seq(3.5, 9.5, by = 1), col = "lightcoral",
     main = "Distribution of Sum | Die1 = 3", xlab = "Sum of Two Dice",
     probability = TRUE)

par(mfrow = c(1, 1))


# ----------------------------------------------------------------------------
# 2.2 Bayes' Theorem Through Simulation
# ----------------------------------------------------------------------------

# Bayes' Theorem: P(A|B) = P(B|A) * P(A) / P(B)
# This lets us "flip" conditional probabilities!

# Classic Example: Disease Testing
# - Disease prevalence: 1% of population has the disease
# - Test sensitivity: P(positive | disease) = 99%
# - Test specificity: P(negative | no disease) = 95%
# Question: If you test positive, what's P(disease | positive)?

# Most people guess ~95% or 99%. The truth is shocking!

n_population <- 1000000
has_disease <- sample(c(TRUE, FALSE), n_population, replace = TRUE, 
                      prob = c(0.01, 0.99))

# Generate test results
test_positive <- logical(n_population)
test_positive[has_disease] <- sample(c(TRUE, FALSE), sum(has_disease), 
                                      replace = TRUE, prob = c(0.99, 0.01))
test_positive[!has_disease] <- sample(c(TRUE, FALSE), sum(!has_disease), 
                                       replace = TRUE, prob = c(0.05, 0.95))

# What fraction of positive tests are true positives?
P_disease_given_positive <- mean(has_disease[test_positive])

cat("\n=== BAYES' THEOREM: DISEASE TESTING ===\n")
cat("P(disease) = 1%\n")
cat("P(positive | disease) = 99%\n")
cat("P(negative | no disease) = 95%\n")
cat("\nSimulated P(disease | positive) =", round(P_disease_given_positive, 4), "\n")

# Theoretical calculation using Bayes
P_disease <- 0.01
P_pos_given_disease <- 0.99
P_pos_given_no_disease <- 0.05
P_positive <- P_pos_given_disease * P_disease + P_pos_given_no_disease * (1 - P_disease)
P_disease_given_positive_theory <- (P_pos_given_disease * P_disease) / P_positive

cat("Theoretical P(disease | positive) =", round(P_disease_given_positive_theory, 4), "\n")
cat("\nSurprising! Even with a 'good' test, most positive results are FALSE POSITIVES\n")
cat("when the disease is rare. This is the base rate fallacy.\n")

# Visualize with a tree
cat("\nOut of 1,000,000 people:\n")
cat("- ", sum(has_disease), " have disease\n")
cat("- ", sum(!has_disease), " don't have disease\n")
cat("- ", sum(test_positive), " test positive\n")
cat("- ", sum(test_positive & has_disease), " true positives\n")
cat("- ", sum(test_positive & !has_disease), " FALSE positives\n")


# ----------------------------------------------------------------------------
# 2.3 Independence vs Conditional Probability
# ----------------------------------------------------------------------------

# Events A and B are INDEPENDENT if:
# P(A|B) = P(A)  -- knowing B doesn't change probability of A
# Equivalently: P(A and B) = P(A) * P(B)

# Example 1: Two coin flips (INDEPENDENT)
n_sims <- 100000
flip1 <- sample(c("H", "T"), n_sims, replace = TRUE)
flip2 <- sample(c("H", "T"), n_sims, replace = TRUE)

P_flip2_H <- mean(flip2 == "H")
P_flip2_H_given_flip1_H <- mean(flip2[flip1 == "H"] == "H")

cat("\n=== INDEPENDENCE ===\n")
cat("Coin flips are independent:\n")
cat("P(flip2 = H) =", round(P_flip2_H, 4), "\n")
cat("P(flip2 = H | flip1 = H) =", round(P_flip2_H_given_flip1_H, 4), "\n")
cat("These are equal! Knowing flip1 doesn't help predict flip2.\n")

# Example 2: Drawing cards WITHOUT replacement (DEPENDENT)
# Draw 2 cards from a deck
n_sims <- 100000
first_card_ace <- logical(n_sims)
second_card_ace <- logical(n_sims)

for (i in 1:n_sims) {
  deck <- rep(c("Ace", "Not Ace"), c(4, 48))
  draw <- sample(deck, 2, replace = FALSE)
  first_card_ace[i] <- draw[1] == "Ace"
  second_card_ace[i] <- draw[2] == "Ace"
}

P_second_ace <- mean(second_card_ace)
P_second_ace_given_first_ace <- mean(second_card_ace[first_card_ace])
P_second_ace_given_first_not_ace <- mean(second_card_ace[!first_card_ace])

cat("\nDrawing cards WITHOUT replacement is dependent:\n")
cat("P(2nd card Ace) =", round(P_second_ace, 4), "(theory: 4/52 =", round(4/52, 4), ")\n")
cat("P(2nd Ace | 1st Ace) =", round(P_second_ace_given_first_ace, 4), 
    "(theory: 3/51 =", round(3/51, 4), ")\n")
cat("P(2nd Ace | 1st not Ace) =", round(P_second_ace_given_first_not_ace, 4), 
    "(theory: 4/51 =", round(4/51, 4), ")\n")
cat("These are NOT equal! The first draw affects the second.\n")


# ============================================================================
# PART III: RANDOM VARIABLES
# ============================================================================

# ----------------------------------------------------------------------------
# 3.1 What is a Random Variable?
# ----------------------------------------------------------------------------

# A random variable X is a function that assigns a NUMBER to each outcome
# in the sample space.

# Example: Roll two dice, X = sum
# The sample space is {(1,1), (1,2), ..., (6,6)} -- 36 outcomes
# X maps each outcome to a number from 2 to 12

# We care about the DISTRIBUTION of X: what values it takes and how often

n_sims <- 100000
die1 <- sample(1:6, n_sims, replace = TRUE)
die2 <- sample(1:6, n_sims, replace = TRUE)
X <- die1 + die2  # X is a random variable: sum of two dice

# The distribution of X
table(X) / n_sims

# Visualize
barplot(table(X) / n_sims, col = "steelblue",
        main = "Distribution of X = Sum of Two Dice",
        xlab = "Sum", ylab = "Probability")

# Why is 7 most likely? Count the ways to make each sum:
# 2: (1,1) -- 1 way
# 3: (1,2), (2,1) -- 2 ways
# ...
# 7: (1,6), (2,5), (3,4), (4,3), (5,2), (6,1) -- 6 ways
# ...
# 12: (6,6) -- 1 way


# ----------------------------------------------------------------------------
# 3.2 Discrete vs Continuous Random Variables
# ----------------------------------------------------------------------------

# DISCRETE: Takes on countable values (integers, categories)
# Examples: die rolls, coin flips, count of events

# CONTINUOUS: Takes on any value in an interval
# Examples: height, weight, time, temperature

# For discrete RVs: we have a Probability Mass Function (PMF)
#   P(X = x) = probability that X equals exactly x

# For continuous RVs: we have a Probability Density Function (PDF)
#   P(a < X < b) = integral of PDF from a to b
#   Note: P(X = x) = 0 for any specific value! (infinitely many possibilities)

par(mfrow = c(1, 2))

# Discrete: Binomial (number of heads in 10 flips)
n_sims <- 100000
binomial_draws <- rbinom(n_sims, size = 10, prob = 0.5)
barplot(table(binomial_draws) / n_sims, col = "steelblue",
        main = "Discrete: Binomial(10, 0.5)", xlab = "Number of Heads",
        ylab = "Probability")

# Continuous: Normal
normal_draws <- rnorm(n_sims, mean = 0, sd = 1)
hist(normal_draws, breaks = 50, probability = TRUE, col = "lightcoral",
     main = "Continuous: Normal(0, 1)", xlab = "x")
curve(dnorm(x), add = TRUE, col = "darkred", lwd = 2)

par(mfrow = c(1, 1))


# ============================================================================
# PART IV: EXPECTED VALUE - THE CENTER OF A DISTRIBUTION
# ============================================================================

# ----------------------------------------------------------------------------
# 4.1 What is Expected Value?
# ----------------------------------------------------------------------------

# E[X] = "expected value" or "mean" of X
# = weighted average of all possible values, weighted by their probabilities
# = the "center of mass" of the distribution
# = what we'd EXPECT on average if we sampled X infinitely many times

# For discrete: E[X] = sum of (x * P(X = x)) over all x
# For continuous: E[X] = integral of (x * f(x)) dx

# Example: Expected value of a die roll
# E[X] = 1*(1/6) + 2*(1/6) + 3*(1/6) + 4*(1/6) + 5*(1/6) + 6*(1/6) = 3.5

die_rolls <- sample(1:6, 100000, replace = TRUE)
cat("\nExpected value of die roll:\n")
cat("Theoretical: 3.5\n")
cat("Simulated:", round(mean(die_rolls), 4), "\n")

# Note: The expected value doesn't have to be a possible outcome!
# You can never roll 3.5, but it's the expected value.


# ----------------------------------------------------------------------------
# 4.2 Expected Value as a Prediction
# ----------------------------------------------------------------------------

# If you had to guess a single value for X, and you're penalized by
# squared error (X - guess)^2, then the BEST guess is E[X]!

# Let's verify this by simulation
n_sims <- 10000
X_samples <- rnorm(n_sims, mean = 5, sd = 2)  # True mean is 5

guesses <- seq(2, 8, by = 0.1)
avg_squared_errors <- sapply(guesses, function(g) mean((X_samples - g)^2))

plot(guesses, avg_squared_errors, type = "l", lwd = 2, col = "steelblue",
     xlab = "Guess", ylab = "Average Squared Error",
     main = "Expected Value Minimizes Squared Error")
abline(v = mean(X_samples), col = "red", lty = 2, lwd = 2)
legend("topright", legend = paste("Best guess (mean) =", round(mean(X_samples), 2)),
       col = "red", lty = 2, lwd = 2, bty = "n")


# ----------------------------------------------------------------------------
# 4.3 Properties of Expected Value
# ----------------------------------------------------------------------------

# E[aX + b] = a*E[X] + b  (linearity)
# E[X + Y] = E[X] + E[Y]  (always, even if X and Y are dependent!)
# E[XY] = E[X]*E[Y]  (ONLY if X and Y are independent)

n_sims <- 100000
X <- rnorm(n_sims, mean = 3, sd = 1)
Y <- rnorm(n_sims, mean = 5, sd = 2)

cat("\n=== LINEARITY OF EXPECTATION ===\n")
cat("E[X] =", round(mean(X), 4), "(theory: 3)\n")
cat("E[2X + 7] =", round(mean(2*X + 7), 4), "(theory: 2*3 + 7 = 13)\n")
cat("E[X + Y] =", round(mean(X + Y), 4), "(theory: 3 + 5 = 8)\n")

# Now make Y dependent on X
Y_dependent <- 2*X + rnorm(n_sims, mean = 0, sd = 1)
cat("\nEven with dependence:\n")
cat("E[X + Y_dependent] =", round(mean(X + Y_dependent), 4), 
    "(theory: 3 + 2*3 = 9)\n")


# ----------------------------------------------------------------------------
# 4.4 Expected Value of Functions
# ----------------------------------------------------------------------------

# E[g(X)] is NOT generally equal to g(E[X])!
# This is called Jensen's inequality (for convex/concave functions)

# Example: E[X^2] vs (E[X])^2
X <- rnorm(100000, mean = 2, sd = 3)

cat("\n=== EXPECTATION OF FUNCTIONS ===\n")
cat("E[X] =", round(mean(X), 4), "(theory: 2)\n")
cat("(E[X])^2 =", round(mean(X)^2, 4), "(theory: 4)\n")
cat("E[X^2] =", round(mean(X^2), 4), "(theory: Var(X) + (E[X])^2 = 9 + 4 = 13)\n")
cat("\nE[X^2] ≠ (E[X])^2 in general!\n")


# ============================================================================
# PART V: VARIANCE - THE SPREAD OF A DISTRIBUTION
# ============================================================================

# ----------------------------------------------------------------------------
# 5.1 What is Variance?
# ----------------------------------------------------------------------------

# Var(X) = E[(X - E[X])^2] = expected squared deviation from the mean
# Also: Var(X) = E[X^2] - (E[X])^2

# Standard Deviation: SD(X) = sqrt(Var(X)) -- same units as X

# Intuition: Variance measures how "spread out" the distribution is

# Compare two distributions with same mean but different variance
par(mfrow = c(1, 2))

X_low_var <- rnorm(10000, mean = 0, sd = 1)
X_high_var <- rnorm(10000, mean = 0, sd = 3)

hist(X_low_var, breaks = 50, probability = TRUE, col = "lightblue",
     main = paste("SD = 1, Var =", round(var(X_low_var), 2)),
     xlab = "x", xlim = c(-12, 12))
hist(X_high_var, breaks = 50, probability = TRUE, col = "lightcoral",
     main = paste("SD = 3, Var =", round(var(X_high_var), 2)),
     xlab = "x", xlim = c(-12, 12))

par(mfrow = c(1, 1))


# ----------------------------------------------------------------------------
# 5.2 Computing Variance by Simulation
# ----------------------------------------------------------------------------

# Let's compute variance of sum of two dice
die1 <- sample(1:6, 100000, replace = TRUE)
die2 <- sample(1:6, 100000, replace = TRUE)
X <- die1 + die2

# Method 1: Definition
mean_X <- mean(X)
var_X_def <- mean((X - mean_X)^2)

# Method 2: Alternative formula
var_X_alt <- mean(X^2) - mean(X)^2

# Method 3: R's built-in (uses n-1 denominator)
var_X_builtin <- var(X)

cat("\n=== COMPUTING VARIANCE ===\n")
cat("Variance of sum of two dice:\n")
cat("Method 1 (definition):", round(var_X_def, 4), "\n")
cat("Method 2 (E[X^2] - E[X]^2):", round(var_X_alt, 4), "\n")
cat("Method 3 (R's var()):", round(var_X_builtin, 4), "\n")
cat("Theory: 35/6 =", round(35/6, 4), "\n")


# ----------------------------------------------------------------------------
# 5.3 Properties of Variance
# ----------------------------------------------------------------------------

# Var(aX + b) = a^2 * Var(X)  (constants shift doesn't affect spread!)
# Var(X + Y) = Var(X) + Var(Y) + 2*Cov(X,Y)
# Var(X + Y) = Var(X) + Var(Y)  (ONLY if X, Y independent)

n_sims <- 100000
X <- rnorm(n_sims, mean = 0, sd = 2)  # Var = 4
Y <- rnorm(n_sims, mean = 0, sd = 3)  # Var = 9 (independent of X)

cat("\n=== PROPERTIES OF VARIANCE ===\n")
cat("Var(X) =", round(var(X), 2), "(theory: 4)\n")
cat("Var(3X + 10) =", round(var(3*X + 10), 2), "(theory: 9*4 = 36)\n")
cat("Var(X + Y) =", round(var(X + Y), 2), "(theory: 4 + 9 = 13)\n")

# If X and Y are NOT independent:
Y_dep <- X + rnorm(n_sims, mean = 0, sd = 1)  # Y depends on X
cat("\nWith dependent Y:\n")
cat("Var(X + Y_dep) =", round(var(X + Y_dep), 2), "\n")
cat("Var(X) + Var(Y_dep) =", round(var(X) + var(Y_dep), 2), "\n")
cat("These are NOT equal when X and Y are dependent!\n")


# ============================================================================
# PART VI: FAMOUS DISTRIBUTIONS
# ============================================================================

# ----------------------------------------------------------------------------
# 6.1 Bernoulli and Binomial
# ----------------------------------------------------------------------------

# Bernoulli: Single trial with probability p of success
# X ~ Bernoulli(p): P(X=1) = p, P(X=0) = 1-p
# E[X] = p, Var(X) = p(1-p)

# Binomial: Number of successes in n independent Bernoulli trials
# X ~ Binomial(n, p)
# E[X] = np, Var(X) = np(1-p)

p <- 0.3
n <- 20

# Simulate binomial as sum of Bernoulli trials
n_sims <- 100000
binomial_sims <- replicate(n_sims, sum(rbinom(n, 1, p)))

cat("\n=== BINOMIAL DISTRIBUTION ===\n")
cat("Binomial(n=20, p=0.3):\n")
cat("E[X]: simulated =", round(mean(binomial_sims), 2), ", theory = np =", n*p, "\n")
cat("Var(X): simulated =", round(var(binomial_sims), 2), 
    ", theory = np(1-p) =", n*p*(1-p), "\n")

# Visualize
par(mfrow = c(1, 2))
hist(binomial_sims, breaks = seq(-0.5, 20.5, by = 1), probability = TRUE,
     col = "steelblue", main = "Simulated Binomial(20, 0.3)", xlab = "x")

# Compare to theoretical PMF
x_vals <- 0:20
pmf_theory <- dbinom(x_vals, n, p)
barplot(pmf_theory, names.arg = x_vals, col = "lightcoral",
        main = "Theoretical Binomial(20, 0.3)", xlab = "x", ylab = "P(X=x)")
par(mfrow = c(1, 1))


# ----------------------------------------------------------------------------
# 6.2 Poisson Distribution
# ----------------------------------------------------------------------------

# Models count of rare events in a fixed interval
# X ~ Poisson(lambda): E[X] = Var(X) = lambda

# Example: Number of emails per hour (average 5)
lambda <- 5

n_sims <- 100000
poisson_sims <- rpois(n_sims, lambda)

cat("\n=== POISSON DISTRIBUTION ===\n")
cat("Poisson(lambda=5):\n")
cat("E[X]: simulated =", round(mean(poisson_sims), 2), ", theory =", lambda, "\n")
cat("Var(X): simulated =", round(var(poisson_sims), 2), ", theory =", lambda, "\n")

# Connection to Binomial: Poisson is limit of Binomial as n->infinity, p->0, np=lambda
n_large <- 1000
p_small <- lambda / n_large
binomial_approx <- rbinom(n_sims, n_large, p_small)

par(mfrow = c(1, 2))
hist(poisson_sims, breaks = seq(-0.5, max(poisson_sims)+0.5, by = 1), 
     probability = TRUE, col = "steelblue", main = "Poisson(5)", xlab = "x")
hist(binomial_approx, breaks = seq(-0.5, max(binomial_approx)+0.5, by = 1),
     probability = TRUE, col = "lightcoral", main = "Binomial(1000, 0.005)", xlab = "x")
par(mfrow = c(1, 1))


# ----------------------------------------------------------------------------
# 6.3 Normal (Gaussian) Distribution
# ----------------------------------------------------------------------------

# The most important distribution in statistics!
# X ~ Normal(mu, sigma^2): E[X] = mu, Var(X) = sigma^2

# Why so important?
# 1. Central Limit Theorem: Sums of many RVs tend toward normal
# 2. Many natural phenomena are approximately normal
# 3. Mathematically convenient

# The 68-95-99.7 rule:
# ~68% of values within 1 SD of mean
# ~95% of values within 2 SD of mean
# ~99.7% of values within 3 SD of mean

n_sims <- 100000
normal_sims <- rnorm(n_sims, mean = 0, sd = 1)

cat("\n=== NORMAL DISTRIBUTION: 68-95-99.7 RULE ===\n")
cat("Within 1 SD:", round(mean(abs(normal_sims) < 1) * 100, 1), "% (theory: 68.3%)\n")
cat("Within 2 SD:", round(mean(abs(normal_sims) < 2) * 100, 1), "% (theory: 95.4%)\n")
cat("Within 3 SD:", round(mean(abs(normal_sims) < 3) * 100, 1), "% (theory: 99.7%)\n")

# Visualize
x <- seq(-4, 4, length.out = 1000)
y <- dnorm(x)

plot(x, y, type = "l", lwd = 2, col = "black",
     main = "Standard Normal Distribution", xlab = "x", ylab = "Density")

# Shade regions
polygon(c(x[x >= -1 & x <= 1], 1, -1), 
        c(y[x >= -1 & x <= 1], 0, 0), 
        col = rgb(0, 0, 1, 0.3), border = NA)
polygon(c(x[x >= -2 & x <= 2], 2, -2), 
        c(y[x >= -2 & x <= 2], 0, 0), 
        col = rgb(0, 0, 1, 0.2), border = NA)

legend("topright", legend = c("Within 1 SD (68%)", "Within 2 SD (95%)"),
       fill = c(rgb(0, 0, 1, 0.3), rgb(0, 0, 1, 0.2)), bty = "n")


# ----------------------------------------------------------------------------
# 6.4 Uniform Distribution
# ----------------------------------------------------------------------------

# X ~ Uniform(a, b): All values in [a,b] equally likely
# E[X] = (a+b)/2, Var(X) = (b-a)^2/12

a <- 2
b <- 8
n_sims <- 100000
uniform_sims <- runif(n_sims, a, b)

cat("\n=== UNIFORM DISTRIBUTION ===\n")
cat("Uniform(2, 8):\n")
cat("E[X]: simulated =", round(mean(uniform_sims), 2), ", theory =", (a+b)/2, "\n")
cat("Var(X): simulated =", round(var(uniform_sims), 2), 
    ", theory =", round((b-a)^2/12, 2), "\n")


# ============================================================================
# PART VII: JOINT DISTRIBUTIONS AND COVARIANCE
# ============================================================================

# ----------------------------------------------------------------------------
# 7.1 Joint Distributions
# ----------------------------------------------------------------------------

# Two random variables X and Y have a JOINT distribution
# We can ask: What's P(X=x AND Y=y)?  or P(X < a AND Y < b)?

# Example: Heights and weights of people (continuous)
n <- 1000
height <- rnorm(n, mean = 170, sd = 10)  # cm
weight <- 0.8 * height - 66 + rnorm(n, sd = 8)  # kg (correlated with height)

# Joint distribution is hard to visualize, but we can use scatterplots
plot(height, weight, pch = 16, col = rgb(0, 0, 0, 0.3),
     main = "Joint Distribution of Height and Weight",
     xlab = "Height (cm)", ylab = "Weight (kg)")


# ----------------------------------------------------------------------------
# 7.2 Marginal Distributions
# ----------------------------------------------------------------------------

# The MARGINAL distribution of X is the distribution ignoring Y
# We "marginalize out" Y by summing/integrating over all Y values

# From the joint distribution, we can get marginals:
par(mfrow = c(1, 3))

# Joint
plot(height, weight, pch = 16, col = rgb(0, 0, 0, 0.3),
     main = "Joint Distribution", xlab = "Height (cm)", ylab = "Weight (kg)")

# Marginal of Height
hist(height, breaks = 30, probability = TRUE, col = "steelblue",
     main = "Marginal of Height", xlab = "Height (cm)")

# Marginal of Weight
hist(weight, breaks = 30, probability = TRUE, col = "lightcoral",
     main = "Marginal of Weight", xlab = "Weight (kg)")

par(mfrow = c(1, 1))


# ----------------------------------------------------------------------------
# 7.3 Covariance
# ----------------------------------------------------------------------------

# Cov(X,Y) = E[(X - E[X])(Y - E[Y])] = E[XY] - E[X]E[Y]
# Measures LINEAR association between X and Y
# Cov > 0: X and Y tend to move together
# Cov < 0: X and Y tend to move opposite
# Cov = 0: No linear relationship (but could still be related nonlinearly!)

cat("\n=== COVARIANCE ===\n")
cat("Cov(height, weight) =", round(cov(height, weight), 2), "\n")
cat("This is positive: taller people tend to weigh more.\n")

# Independence implies zero covariance (but not vice versa!)
X_ind <- rnorm(10000)
Y_ind <- rnorm(10000)
cat("\nIndependent X and Y: Cov =", round(cov(X_ind, Y_ind), 4), "(approximately 0)\n")

# Zero covariance does NOT imply independence!
# Example: Y = X^2 where X ~ Normal(0,1)
X_dep <- rnorm(10000)
Y_dep <- X_dep^2

cat("\nDependent but Cov ≈ 0:\n")
cat("Y = X^2 where X ~ Normal(0,1)\n")
cat("Cov(X, X^2) =", round(cov(X_dep, Y_dep), 4), "\n")
cat("But X and Y are clearly related!\n")

par(mfrow = c(1, 2))
plot(X_ind, Y_ind, pch = ".", col = rgb(0,0,0,0.3),
     main = "Independent: Cov ≈ 0", xlab = "X", ylab = "Y")
plot(X_dep, Y_dep, pch = ".", col = rgb(0,0,0,0.3),
     main = "Dependent but Cov ≈ 0: Y = X²", xlab = "X", ylab = "Y = X²")
par(mfrow = c(1, 1))


# ----------------------------------------------------------------------------
# 7.4 Correlation
# ----------------------------------------------------------------------------

# Correlation standardizes covariance to [-1, 1]
# Cor(X,Y) = Cov(X,Y) / (SD(X) * SD(Y))

# Cor = 1: Perfect positive linear relationship
# Cor = -1: Perfect negative linear relationship
# Cor = 0: No linear relationship

cat("\n=== CORRELATION ===\n")
cat("Cor(height, weight) =", round(cor(height, weight), 3), "\n")

# Visualize different correlations
par(mfrow = c(2, 4))
correlations <- c(-1, -0.8, -0.4, 0, 0.4, 0.8, 1, 0)

for (i in 1:length(correlations)) {
  r <- correlations[i]
  n <- 300
  
  if (i < 8) {
    X <- rnorm(n)
    if (abs(r) == 1) {
      Y <- r * X
    } else {
      Y <- r * X + sqrt(1 - r^2) * rnorm(n)
    }
    plot(X, Y, pch = 16, col = rgb(0, 0, 0, 0.5),
         main = paste("r =", r), xlab = "X", ylab = "Y", asp = 1)
  } else {
    # Non-linear relationship with zero correlation
    X <- runif(n, -2, 2)
    Y <- X^2 + rnorm(n, sd = 0.3)
    plot(X, Y, pch = 16, col = rgb(0, 0, 0, 0.5),
         main = paste("r =", round(cor(X, Y), 2), "(nonlinear)"), 
         xlab = "X", ylab = "Y")
  }
}
par(mfrow = c(1, 1))


# ============================================================================
# PART VIII: CONDITIONAL EXPECTATION - THE BRIDGE TO REGRESSION
# ============================================================================

# ----------------------------------------------------------------------------
# 8.1 What is Conditional Expectation?
# ----------------------------------------------------------------------------

# E[Y|X] = Expected value of Y, given that we know X
# This is a FUNCTION of X!

# For each value of X, E[Y|X=x] tells us the average Y among all 
# observations where X equals (or is close to) x.

# Let's compute E[Y|X] empirically
n <- 5000
X <- runif(n, 0, 10)
Y <- 2 + 0.5 * X + rnorm(n, sd = 1)  # True relationship: E[Y|X] = 2 + 0.5*X

# Bin X and compute mean Y in each bin
breaks <- seq(0, 10, by = 1)
X_bin <- cut(X, breaks)
conditional_means <- tapply(Y, X_bin, mean)
bin_centers <- (breaks[-length(breaks)] + breaks[-1]) / 2

# Plot
plot(X, Y, pch = 16, col = rgb(0, 0, 0, 0.2),
     main = "Conditional Expectation E[Y|X]",
     xlab = "X", ylab = "Y")
points(bin_centers, conditional_means, pch = 19, col = "red", cex = 2)
lines(bin_centers, conditional_means, col = "red", lwd = 2)

# Add true E[Y|X]
abline(a = 2, b = 0.5, col = "blue", lwd = 2, lty = 2)
legend("topleft", 
       legend = c("Data", "Estimated E[Y|X]", "True E[Y|X] = 2 + 0.5X"),
       pch = c(16, 19, NA), lty = c(NA, 1, 2), col = c("gray", "red", "blue"),
       lwd = c(NA, 2, 2), bty = "n")


# ----------------------------------------------------------------------------
# 8.2 E[Y|X] is the Best Predictor of Y Given X
# ----------------------------------------------------------------------------

# KEY THEOREM: Among all functions g(X), E[Y|X] minimizes the mean squared error:
# E[Y|X] = argmin_g E[(Y - g(X))^2]

# This means: If you want to predict Y and you know X, your best prediction
# (in terms of minimizing squared error) is E[Y|X].

# Let's verify this by trying different prediction functions

# True relationship: Y = 2 + 0.5*X + noise
# E[Y|X] = 2 + 0.5*X (the optimal predictor)

# Compare different predictors:
predictions <- data.frame(
  True_EYX = 2 + 0.5 * X,        # Optimal
  Constant = rep(mean(Y), n),   # Just predict the mean
  Wrong_slope = 2 + 1.0 * X,     # Wrong slope
  Wrong_intercept = 0 + 0.5 * X  # Wrong intercept
)

mse <- sapply(predictions, function(pred) mean((Y - pred)^2))

cat("\n=== E[Y|X] MINIMIZES MSE ===\n")
cat("Mean Squared Error for different predictors:\n")
print(round(mse, 4))
cat("\nThe true E[Y|X] = 2 + 0.5X has the smallest MSE!\n")


# ----------------------------------------------------------------------------
# 8.3 Properties of Conditional Expectation
# ----------------------------------------------------------------------------

# 1. Law of Total Expectation: E[Y] = E[E[Y|X]]
#    The average of conditional averages equals the overall average

cat("\n=== LAW OF TOTAL EXPECTATION ===\n")
cat("E[Y] =", round(mean(Y), 4), "\n")
cat("E[E[Y|X]] =", round(mean(conditional_means), 4), "\n")
cat("These should be approximately equal!\n")

# 2. E[Y|X] "removes" the part of Y that depends on X
#    The residual Y - E[Y|X] has mean zero given X

residuals <- Y - (2 + 0.5 * X)  # Y - E[Y|X]
cat("\nMean of residuals:", round(mean(residuals), 4), "(should be ~0)\n")

# Check that residuals have mean zero in each bin
residual_means_by_bin <- tapply(residuals, X_bin, mean)
cat("Mean residual by X bin:\n")
print(round(residual_means_by_bin, 4))


# ----------------------------------------------------------------------------
# 8.4 Non-Linear Conditional Expectation
# ----------------------------------------------------------------------------

# E[Y|X] doesn't have to be linear!

# Example: Y = sin(X) + noise
n <- 1000
X <- runif(n, 0, 4*pi)
Y <- sin(X) + rnorm(n, sd = 0.3)

# Compute conditional means
breaks <- seq(0, 4*pi, length.out = 25)
X_bin <- cut(X, breaks)
conditional_means <- tapply(Y, X_bin, mean)
bin_centers <- (breaks[-length(breaks)] + breaks[-1]) / 2

plot(X, Y, pch = 16, col = rgb(0, 0, 0, 0.3),
     main = "Non-Linear Conditional Expectation",
     xlab = "X", ylab = "Y")
points(bin_centers, conditional_means, pch = 19, col = "red", cex = 1.5)
curve(sin(x), add = TRUE, col = "blue", lwd = 2)
legend("topright", 
       legend = c("Data", "Estimated E[Y|X]", "True E[Y|X] = sin(X)"),
       pch = c(16, 19, NA), col = c("gray", "red", "blue"),
       lty = c(NA, NA, 1), lwd = c(NA, NA, 2), bty = "n")


# ============================================================================
# PART IX: THE LAW OF LARGE NUMBERS AND CENTRAL LIMIT THEOREM
# ============================================================================

# ----------------------------------------------------------------------------
# 9.1 Law of Large Numbers (LLN)
# ----------------------------------------------------------------------------

# As sample size increases, the sample mean converges to the population mean
# This is why simulation works for computing probabilities!

# Demonstration: Roll a die many times
n_rolls <- 10000
die_rolls <- sample(1:6, n_rolls, replace = TRUE)
running_mean <- cumsum(die_rolls) / (1:n_rolls)

plot(1:n_rolls, running_mean, type = "l", col = "steelblue",
     xlab = "Number of Rolls", ylab = "Running Mean",
     main = "Law of Large Numbers: Die Rolls", ylim = c(2.5, 4.5))
abline(h = 3.5, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Running mean", "True mean (3.5)"),
       col = c("steelblue", "red"), lty = c(1, 2), lwd = 2, bty = "n")


# ----------------------------------------------------------------------------
# 9.2 Central Limit Theorem (CLT)
# ----------------------------------------------------------------------------

# The sum (or average) of many independent random variables is approximately
# NORMAL, regardless of the original distribution!

# This is why the normal distribution is so important.

# Let's demonstrate with dice (definitely not normal!)

# Distribution of a single die roll
single_die <- sample(1:6, 100000, replace = TRUE)

# Distribution of sum of 2 dice
sum_2_dice <- replicate(100000, sum(sample(1:6, 2, replace = TRUE)))

# Distribution of sum of 10 dice
sum_10_dice <- replicate(100000, sum(sample(1:6, 10, replace = TRUE)))

# Distribution of sum of 50 dice
sum_50_dice <- replicate(100000, sum(sample(1:6, 50, replace = TRUE)))

par(mfrow = c(2, 2))

hist(single_die, breaks = seq(0.5, 6.5, by = 1), probability = TRUE,
     col = "steelblue", main = "1 Die (Uniform)", xlab = "Value")

hist(sum_2_dice, breaks = 20, probability = TRUE,
     col = "steelblue", main = "Sum of 2 Dice (Triangular)", xlab = "Sum")

hist(sum_10_dice, breaks = 30, probability = TRUE,
     col = "steelblue", main = "Sum of 10 Dice (Getting Normal)", xlab = "Sum")
# Overlay normal
x <- seq(min(sum_10_dice), max(sum_10_dice), length.out = 100)
lines(x, dnorm(x, mean = 35, sd = sqrt(10 * 35/12)), col = "red", lwd = 2)

hist(sum_50_dice, breaks = 40, probability = TRUE,
     col = "steelblue", main = "Sum of 50 Dice (Very Normal!)", xlab = "Sum")
# Overlay normal
x <- seq(min(sum_50_dice), max(sum_50_dice), length.out = 100)
lines(x, dnorm(x, mean = 175, sd = sqrt(50 * 35/12)), col = "red", lwd = 2)

par(mfrow = c(1, 1))

cat("\n=== CENTRAL LIMIT THEOREM ===\n")
cat("Even though a single die is UNIFORM (not normal at all),\n")
cat("the sum of many dice becomes NORMAL!\n")
cat("\nThis works for ANY distribution with finite mean and variance.\n")


# ============================================================================
# PART X: FROM PROBABILITY TO REGRESSION
# ============================================================================

# ----------------------------------------------------------------------------
# 10.1 The Regression Model
# ----------------------------------------------------------------------------

# Now we connect everything to regression!

# The REGRESSION MODEL assumes:
# Y = E[Y|X] + epsilon
# where epsilon is random noise with E[epsilon|X] = 0

# If we assume E[Y|X] is LINEAR in X:
# E[Y|X] = beta_0 + beta_1 * X

# Then:
# Y = beta_0 + beta_1 * X + epsilon

# This is the simple linear regression model!

cat("\n")
cat("================================================================\n")
cat("       THE CONNECTION: PROBABILITY -> REGRESSION\n")
cat("================================================================\n")
cat("\n")
cat("Key insights:\n")
cat("1. Regression estimates E[Y|X] - the conditional expectation\n")
cat("2. E[Y|X] is the BEST predictor of Y given X (minimizes MSE)\n")
cat("3. We ASSUME E[Y|X] is linear: E[Y|X] = beta_0 + beta_1 * X\n")
cat("4. Least squares finds the beta's that minimize sum of squared errors\n")
cat("5. By CLT, our estimates are approximately normal -> inference!\n")
cat("\n")


# ----------------------------------------------------------------------------
# 10.2 Least Squares Estimates the Conditional Expectation
# ----------------------------------------------------------------------------

# Generate data
set.seed(42)
n <- 500
X <- runif(n, 0, 10)
Y <- 3 + 2 * X + rnorm(n, sd = 3)  # True model: E[Y|X] = 3 + 2X

# Fit regression
model <- lm(Y ~ X)

# Compare to conditional means
breaks <- seq(0, 10, by = 1)
X_bin <- cut(X, breaks)
conditional_means <- tapply(Y, X_bin, mean)
bin_centers <- (breaks[-length(breaks)] + breaks[-1]) / 2

# Plot
plot(X, Y, pch = 16, col = rgb(0, 0, 0, 0.3),
     main = "Regression Estimates E[Y|X]",
     xlab = "X", ylab = "Y")
points(bin_centers, conditional_means, pch = 19, col = "red", cex = 2)
abline(model, col = "blue", lwd = 3)
abline(a = 3, b = 2, col = "green", lwd = 2, lty = 2)
legend("topleft", 
       legend = c("Data", "Conditional means", "Fitted regression", 
                  "True E[Y|X] = 3 + 2X"),
       pch = c(16, 19, NA, NA), lty = c(NA, NA, 1, 2),
       col = c("gray", "red", "blue", "green"), lwd = c(NA, NA, 3, 2), bty = "n")

cat("True model: E[Y|X] = 3 + 2X\n")
cat("Fitted model: E[Y|X] =", round(coef(model)[1], 2), "+", 
    round(coef(model)[2], 2), "X\n")


# ----------------------------------------------------------------------------
# 10.3 Why Least Squares?
# ----------------------------------------------------------------------------

# Least squares minimizes sum of squared residuals:
# minimize sum((Y_i - (beta_0 + beta_1 * X_i))^2)

# This is the sample analog of minimizing E[(Y - g(X))^2]
# which is minimized by E[Y|X]!

# Visualize the loss function
beta0_grid <- seq(0, 6, length.out = 50)
beta1_grid <- seq(0, 4, length.out = 50)

loss_matrix <- matrix(NA, length(beta0_grid), length(beta1_grid))
for (i in 1:length(beta0_grid)) {
  for (j in 1:length(beta1_grid)) {
    predictions <- beta0_grid[i] + beta1_grid[j] * X
    loss_matrix[i, j] <- sum((Y - predictions)^2)
  }
}

# Contour plot
contour(beta0_grid, beta1_grid, loss_matrix, nlevels = 30,
        xlab = expression(beta[0]), ylab = expression(beta[1]),
        main = "Sum of Squared Errors (Least Squares Objective)")
points(coef(model)[1], coef(model)[2], pch = 19, col = "red", cex = 2)
points(3, 2, pch = 4, col = "green", cex = 2, lwd = 3)
legend("topright", legend = c("Least squares estimate", "True values"),
       pch = c(19, 4), col = c("red", "green"), bty = "n")


# ----------------------------------------------------------------------------
# 10.4 Inference: Using CLT for Regression
# ----------------------------------------------------------------------------

# By the CLT, our estimates beta_hat are approximately normal!
# This allows us to construct confidence intervals and do hypothesis tests.

# Let's verify by simulation: fit regression to many datasets
n_simulations <- 2000
beta1_estimates <- numeric(n_simulations)

for (i in 1:n_simulations) {
  X_sim <- runif(n, 0, 10)
  Y_sim <- 3 + 2 * X_sim + rnorm(n, sd = 3)
  model_sim <- lm(Y_sim ~ X_sim)
  beta1_estimates[i] <- coef(model_sim)[2]
}

# The sampling distribution is approximately normal!
hist(beta1_estimates, breaks = 40, probability = TRUE, col = "lightblue",
     main = "Sampling Distribution of Slope Estimate",
     xlab = expression(hat(beta)[1]))
curve(dnorm(x, mean = mean(beta1_estimates), sd = sd(beta1_estimates)),
      add = TRUE, col = "red", lwd = 2)
abline(v = 2, col = "blue", lwd = 2, lty = 2)
legend("topright", 
       legend = c("Simulated distribution", "Normal approximation", "True value"),
       fill = c("lightblue", NA, NA), lty = c(NA, 1, 2),
       col = c("lightblue", "red", "blue"), border = c("black", NA, NA), bty = "n")

cat("\n=== SAMPLING DISTRIBUTION OF REGRESSION COEFFICIENTS ===\n")
cat("True beta_1:", 2, "\n")
cat("Mean of estimates:", round(mean(beta1_estimates), 4), "\n")
cat("SD of estimates (standard error):", round(sd(beta1_estimates), 4), "\n")
cat("SE from single regression:", round(summary(model)$coef[2, 2], 4), "\n")


# ----------------------------------------------------------------------------
# 10.5 R-squared: Variance Decomposition
# ----------------------------------------------------------------------------

# Remember: Var(Y) = Var(E[Y|X]) + E[Var(Y|X)]
#           Total = Explained + Unexplained

# R-squared = Var(E[Y|X]) / Var(Y) = proportion of variance explained

Y_hat <- predict(model)
residuals <- Y - Y_hat

cat("\n=== VARIANCE DECOMPOSITION ===\n")
cat("Total variance Var(Y):", round(var(Y), 2), "\n")
cat("Explained variance Var(Y_hat):", round(var(Y_hat), 2), "\n")
cat("Unexplained variance Var(residuals):", round(var(residuals), 2), "\n")
cat("Sum:", round(var(Y_hat) + var(residuals), 2), "\n")
cat("\nR-squared:", round(var(Y_hat) / var(Y), 4), "\n")
cat("From lm():", round(summary(model)$r.squared, 4), "\n")


# ============================================================================
# SUMMARY
# ============================================================================

cat("\n")
cat("================================================================\n")
cat("                      KEY TAKEAWAYS\n")
cat("================================================================\n")
cat("\n")
cat("PROBABILITY FOUNDATIONS:\n")
cat("• Probability = long-run frequency (simulation reveals truth)\n")
cat("• Conditional probability: P(A|B) restricts to a subspace\n")
cat("• Independence: knowing one event doesn't help predict another\n")
cat("• Random variables have distributions, means, and variances\n")
cat("• Covariance/correlation measure linear association\n")
cat("\n")
cat("THE BRIDGE TO REGRESSION:\n")
cat("• E[Y|X] = conditional expectation = best predictor of Y given X\n")
cat("• E[Y|X] minimizes mean squared prediction error\n")
cat("• Linear regression assumes E[Y|X] = beta_0 + beta_1 * X\n")
cat("• Least squares estimates the conditional expectation\n")
cat("• CLT makes our estimates approximately normal -> inference!\n")
cat("• R-squared = proportion of Y's variance explained by X\n")
cat("\n")
cat("THE BIG PICTURE:\n")
cat("Regression is not just 'fitting a line' -- it's estimating\n")
cat("the conditional expectation function E[Y|X], which is the\n")
cat("optimal predictor of Y given X in the mean squared error sense.\n")
cat("\n")
