# ============================================================================
# LEARNING PROBABILITY THROUGH SIMULATION
# Statistical Modeling - The University of Austin
# ============================================================================

# Philosophy: The best way to understand probability is to SIMULATE.
# Instead of memorizing formulas, we'll discover them through experimentation.

# Color palette for visualizations
cols <- list(
  blue = "#2E86AB",
  red = "#E94F37",
  green = "#1B998B",
  orange = "#F39237",
  purple = "#7B2CBF",
  pink = "#E36588",
  gray = "#4A4A4A",
  light_blue = "#A8DADC",
  light_red = "#FFCAD4"
)

# ============================================================================
# PART I: WHAT IS PROBABILITY?
# ============================================================================

# ----------------------------------------------------------------------------
# 1.1 The Frequency Interpretation
# ----------------------------------------------------------------------------

# Probability = long-run frequency of an event
# If we repeat an experiment many times, how often does something happen?

set.seed(42)

# Let's flip a coin different numbers of times
cat("=== COIN FLIPPING EXPERIMENT ===\n\n")

flips_10 <- sample(c("H", "T"), size = 10, replace = TRUE)
flips_100 <- sample(c("H", "T"), size = 100, replace = TRUE)
flips_10000 <- sample(c("H", "T"), size = 10000, replace = TRUE)

cat("10 flips:", paste(flips_10, collapse = " "), "\n")
cat("Proportion of heads:", sum(flips_10 == "H") / 10, "\n\n")

cat("100 flips: Proportion of heads:", sum(flips_100 == "H") / 100, "\n")
cat("10,000 flips: Proportion of heads:", sum(flips_10000 == "H") / 10000, "\n\n")

cat("Notice: As we flip more coins, the proportion gets closer to 0.5\n")
cat("This is the Law of Large Numbers in action!\n\n")

# Visualize multiple paths converging
n_flips <- 2000
n_paths <- 20

plot(NULL, xlim = c(1, n_flips), ylim = c(0.2, 0.8),
     xlab = "Number of Flips", ylab = "Proportion of Heads",
     main = "20 Independent Experiments: All Converge to 0.5")

rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
     col = "#F5F5F5", border = NA)
grid(col = "white", lwd = 2)

for (i in 1:n_paths) {
  flips <- sample(c(0, 1), n_flips, replace = TRUE)
  running_prop <- cumsum(flips) / (1:n_flips)
  lines(1:n_flips, running_prop, col = rgb(0.18, 0.53, 0.67, 0.4), lwd = 1.5)
}

flips <- sample(c(0, 1), n_flips, replace = TRUE)
running_prop <- cumsum(flips) / (1:n_flips)
lines(1:n_flips, running_prop, col = cols$red, lwd = 3)

abline(h = 0.5, col = cols$orange, lwd = 3, lty = 2)
legend("topright", legend = c("Individual paths", "One highlighted path", "True P = 0.5"),
       col = c(cols$blue, cols$red, cols$orange), lty = c(1, 1, 2), lwd = c(2, 3, 3), 
       bty = "n", bg = "white")


# ----------------------------------------------------------------------------
# 1.2 Sample Spaces and Events
# ----------------------------------------------------------------------------

cat("\n=== SAMPLE SPACES AND EVENTS ===\n\n")

# Sample space for a die
sample_space_die <- 1:6
cat("Sample space for a die: {", paste(sample_space_die, collapse = ", "), "}\n\n")

# Events are subsets
event_even <- c(2, 4, 6)
event_gt_4 <- c(5, 6)

cat("Event A (even): {", paste(event_even, collapse = ", "), "}\n")
cat("Event B (greater than 4): {", paste(event_gt_4, collapse = ", "), "}\n\n")

# Theoretical probability
P_even <- length(event_even) / length(sample_space_die)
cat("P(even) = |A| / |S| =", length(event_even), "/", length(sample_space_die), "=", P_even, "\n")

# Verify by simulation
die_rolls <- sample(1:6, size = 1000000, replace = TRUE)
simulated_P_even <- mean(die_rolls %% 2 == 0)
cat("Simulated P(even) from 1,000,000 rolls:", round(simulated_P_even, 4), "\n\n")


die_rolls_1 <- sample(1:6, size = 1000000, replace = TRUE)
die_rolls_2 <- sample(1:6, size = 1000000, replace = TRUE)

sum1_2 = die_rolls_1 + die_rolls_2
hist(sum1_2)


# Visualize sample space for two dice
cat("Two dice: 36 possible outcomes\n")
cat("The grid below shows the sum for each (die1, die2) combination.\n")
cat("Sum of 7 is most likely because it can be made 6 different ways.\n\n")

par(mar = c(4, 4, 3, 1))
plot(NULL, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5), asp = 1,
     xlab = "Die 1", ylab = "Die 2", main = "Sample Space: Two Dice (36 outcomes)",
     xaxt = "n", yaxt = "n")
axis(1, at = 1:6)
axis(2, at = 1:6)

for (i in 1:6) {
  for (j in 1:6) {
    total <- i + j
    color_intensity <- (total - 2) / 10
    point_col <- rgb(0.91, 0.31, 0.22, 0.3 + 0.7 * color_intensity)
    points(i, j, pch = 19, cex = 3, col = point_col)
    text(i, j, total, cex = 0.8, col = "white", font = 2)
  }
}

# Highlight sum = 7
for (i in 1:6) {
  j <- 7 - i
  if (j >= 1 && j <= 6) {
    points(i, j, pch = 21, cex = 3.5, col = cols$green, lwd = 3)
  }
}
legend("topleft", legend = "Sum = 7 (6 ways)", pch = 21, col = cols$green, 
       pt.cex = 2, pt.lwd = 3, bty = "n")


# ----------------------------------------------------------------------------
# 1.3 Complement, Union, and Intersection
# ----------------------------------------------------------------------------

cat("\n=== PROBABILITY RULES ===\n\n")

n_sims <- 100000
rolls <- sample(1:6, n_sims, replace = TRUE)

# Event A: Roll a 1 or 2
# Event B: Roll a 2 or 3
A <- rolls %in% c(1, 2)
B <- rolls %in% c(2, 3)

cat("Event A: {1, 2}    Event B: {2, 3}\n\n")

P_A <- mean(A)
P_B <- mean(B)
P_A_and_B <- mean(A & B)
P_A_or_B <- mean(A | B)

cat("ADDITION RULE: P(A or B) = P(A) + P(B) - P(A and B)\n\n")
cat("P(A) =", round(P_A, 4), "  (theory: 2/6 =", round(2/6, 4), ")\n")
cat("P(B) =", round(P_B, 4), "  (theory: 2/6 =", round(2/6, 4), ")\n")
cat("P(A and B) =", round(P_A_and_B, 4), "  (theory: 1/6 =", round(1/6, 4), ")\n")
cat("P(A or B) =", round(P_A_or_B, 4), "  (theory: 3/6 =", round(3/6, 4), ")\n\n")
cat("Check: P(A) + P(B) - P(A and B) =", round(P_A + P_B - P_A_and_B, 4), "\n")

# Venn diagram visualization
par(mar = c(1, 1, 3, 1))
plot(NULL, xlim = c(-2, 2), ylim = c(-1.5, 1.5), asp = 1,
     xlab = "", ylab = "", main = "Events A and B on a Die Roll",
     xaxt = "n", yaxt = "n", bty = "n")

theta <- seq(0, 2*pi, length.out = 100)

x_A <- -0.5 + 0.8 * cos(theta)
y_A <- 0.8 * sin(theta)
polygon(x_A, y_A, col = rgb(0.18, 0.53, 0.67, 0.3), border = cols$blue, lwd = 3)

x_B <- 0.5 + 0.8 * cos(theta)
y_B <- 0.8 * sin(theta)
polygon(x_B, y_B, col = rgb(0.91, 0.31, 0.22, 0.3), border = cols$red, lwd = 3)

text(-1, 0, "1", cex = 1.5, font = 2)
text(0, 0, "2", cex = 1.5, font = 2, col = cols$purple)
text(1, 0, "3", cex = 1.5, font = 2)

text(-0.5, 1.2, "A = {1, 2}", col = cols$blue, font = 2, cex = 1.2)
text(0.5, 1.2, "B = {2, 3}", col = cols$red, font = 2, cex = 1.2)
text(0, -1.3, "A ∩ B = {2}", col = cols$purple, font = 2, cex = 1.1)


# ============================================================================
# PART II: CONDITIONAL PROBABILITY
# ============================================================================

# ----------------------------------------------------------------------------
# 2.1 What is Conditional Probability?
# ----------------------------------------------------------------------------

cat("\n=== CONDITIONAL PROBABILITY ===\n\n")

cat("P(A|B) = 'Probability of A given that B occurred'\n")
cat("P(A|B) = P(A and B) / P(B)\n\n")

cat("We RESTRICT our sample space to only outcomes where B happened,\n")
cat("then ask how often A happens within that restricted space.\n\n")

n_sims <- 100000
die1 <- sample(1:6, n_sims, replace = TRUE)
die2 <- sample(1:6, n_sims, replace = TRUE)
total <- die1 + die2

P_total_8 <- mean(total == 8)
cat("Example: Rolling two dice\n")
cat("P(sum = 8) =", round(P_total_8, 4), "  (theory: 5/36 =", round(5/36, 4), ")\n\n")

given_die1_is_3 <- (die1 == 3)
P_total_8_given_die1_3 <- mean(total[given_die1_is_3] == 8)

cat("Now condition on die1 = 3:\n")
cat("If die1 = 3, we need die2 = 5 for sum = 8\n")
cat("P(sum = 8 | die1 = 3) =", round(P_total_8_given_die1_3, 4), 
    "  (theory: 1/6 =", round(1/6, 4), ")\n\n")

# Visualization
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

hist(total, breaks = seq(1.5, 12.5, by = 1), col = cols$blue, border = "white",
     main = "P(Sum) - All Rolls", xlab = "Sum of Two Dice", probability = TRUE)
abline(v = 8, col = cols$red, lwd = 3, lty = 2)

hist(total[given_die1_is_3], breaks = seq(3.5, 9.5, by = 1), 
     col = cols$orange, border = "white",
     main = "P(Sum | Die1 = 3)", xlab = "Sum of Two Dice", probability = TRUE)
abline(v = 8, col = cols$red, lwd = 3, lty = 2)

par(mfrow = c(1, 1))


# ----------------------------------------------------------------------------
# 2.2 Bayes' Theorem Through Simulation
# ----------------------------------------------------------------------------

cat("\n=== BAYES' THEOREM ===\n\n")

cat("Bayes' Theorem: P(A|B) = P(B|A) * P(A) / P(B)\n")
cat("This lets us 'flip' conditional probabilities!\n\n")

cat("EXAMPLE: Medical Testing\n")
cat("- Disease prevalence: 1% of population has the disease\n")
cat("- Test sensitivity: P(positive | disease) = 99%\n")
cat("- Test specificity: P(negative | no disease) = 95%\n\n")

cat("QUESTION: If you test positive, what's P(disease | positive)?\n")
cat("Most people guess ~95% or 99%. Let's simulate to find out...\n\n")

n_population <- 1000000
has_disease <- sample(c(TRUE, FALSE), n_population, replace = TRUE, 
                      prob = c(0.01, 0.99))

test_positive <- logical(n_population)
test_positive[has_disease] <- sample(c(TRUE, FALSE), sum(has_disease), 
                                      replace = TRUE, prob = c(0.99, 0.01))
test_positive[!has_disease] <- sample(c(TRUE, FALSE), sum(!has_disease), 
                                       replace = TRUE, prob = c(0.05, 0.95))

P_disease_given_positive <- mean(has_disease[test_positive])

cat("RESULTS from 1,000,000 simulated people:\n")
cat("- People with disease:", sum(has_disease), "\n")
cat("- People without disease:", sum(!has_disease), "\n")
cat("- Total positive tests:", sum(test_positive), "\n")
cat("- True positives:", sum(test_positive & has_disease), "\n")
cat("- FALSE positives:", sum(test_positive & !has_disease), "\n\n")

cat("P(disease | positive) =", round(P_disease_given_positive, 4), "\n\n")

# Theoretical calculation
P_disease <- 0.01
P_pos_given_disease <- 0.99
P_pos_given_no_disease <- 0.05
P_positive <- P_pos_given_disease * P_disease + P_pos_given_no_disease * (1 - P_disease)
P_disease_given_positive_theory <- (P_pos_given_disease * P_disease) / P_positive

cat("Theoretical value:", round(P_disease_given_positive_theory, 4), "\n\n")
cat("SURPRISING! Even with a 'good' test, most positive results are FALSE POSITIVES\n")
cat("when the disease is rare. This is the BASE RATE FALLACY.\n\n")

# Icon array visualization
par(mar = c(2, 2, 3, 2))

set.seed(123)
n_viz <- 1000
has_disease_viz <- sample(c(TRUE, FALSE), n_viz, replace = TRUE, prob = c(0.01, 0.99))

test_positive_viz <- logical(n_viz)
test_positive_viz[has_disease_viz] <- sample(c(TRUE, FALSE), sum(has_disease_viz), 
                                              replace = TRUE, prob = c(0.99, 0.01))
test_positive_viz[!has_disease_viz] <- sample(c(TRUE, FALSE), sum(!has_disease_viz), 
                                               replace = TRUE, prob = c(0.05, 0.95))

x_pos <- rep(1:50, 20)
y_pos <- rep(1:20, each = 50)

icon_colors <- rep(cols$light_blue, n_viz)
icon_colors[test_positive_viz & !has_disease_viz] <- cols$orange
icon_colors[test_positive_viz & has_disease_viz] <- cols$red
icon_colors[!test_positive_viz & has_disease_viz] <- cols$gray

plot(x_pos, y_pos, pch = 15, cex = 0.8, col = icon_colors,
     xlab = "", ylab = "", main = "1000 People Tested (each square = 1 person)",
     xaxt = "n", yaxt = "n", bty = "n", asp = 1)

legend("bottom", horiz = TRUE, bty = "n", cex = 0.9,
       legend = c(paste0("True Pos (", sum(test_positive_viz & has_disease_viz), ")"),
                  paste0("False Pos (", sum(test_positive_viz & !has_disease_viz), ")"),
                  paste0("True Neg (", sum(!test_positive_viz & !has_disease_viz), ")"),
                  paste0("False Neg (", sum(!test_positive_viz & has_disease_viz), ")")),
       fill = c(cols$red, cols$orange, cols$light_blue, cols$gray))


# ----------------------------------------------------------------------------
# 2.3 Independence
# ----------------------------------------------------------------------------

cat("\n=== INDEPENDENCE ===\n\n")

cat("Events A and B are INDEPENDENT if:\n")
cat("P(A|B) = P(A)  -- knowing B doesn't change probability of A\n")
cat("Equivalently: P(A and B) = P(A) * P(B)\n\n")

n_sims <- 100000
flip1 <- sample(c("H", "T"), n_sims, replace = TRUE)
flip2 <- sample(c("H", "T"), n_sims, replace = TRUE)

P_flip2_H <- mean(flip2 == "H")
P_flip2_H_given_flip1_H <- mean(flip2[flip1 == "H"] == "H")

cat("EXAMPLE 1: Two coin flips (INDEPENDENT)\n")
cat("P(flip2 = H) =", round(P_flip2_H, 4), "\n")
cat("P(flip2 = H | flip1 = H) =", round(P_flip2_H_given_flip1_H, 4), "\n")
cat("These are equal! Knowing flip1 doesn't help predict flip2.\n\n")

# Drawing cards
n_sims <- 50000
first_card_ace <- logical(n_sims)
second_card_ace <- logical(n_sims)

for (i in 1:n_sims) {
  deck <- rep(c("Ace", "Not Ace"), c(4, 48))
  draw <- sample(deck, 2, replace = FALSE)
  first_card_ace[i] <- draw[1] == "Ace"
  second_card_ace[i] <- draw[2] == "Ace"
}

cat("EXAMPLE 2: Drawing cards WITHOUT replacement (DEPENDENT)\n")
cat("P(2nd card Ace) =", round(mean(second_card_ace), 4), 
    "  (theory: 4/52 =", round(4/52, 4), ")\n")
cat("P(2nd Ace | 1st Ace) =", round(mean(second_card_ace[first_card_ace]), 4), 
    "  (theory: 3/51 =", round(3/51, 4), ")\n")
cat("P(2nd Ace | 1st NOT Ace) =", round(mean(second_card_ace[!first_card_ace]), 4), 
    "  (theory: 4/51 =", round(4/51, 4), ")\n")
cat("These are NOT equal! The first draw affects the second.\n")


# ============================================================================
# PART III: RANDOM VARIABLES
# ============================================================================

# ----------------------------------------------------------------------------
# 3.1 What is a Random Variable?
# ----------------------------------------------------------------------------

cat("\n=== RANDOM VARIABLES ===\n\n")

cat("A random variable X assigns a NUMBER to each outcome in the sample space.\n\n")

cat("Example: Roll two dice, X = sum\n")
cat("X maps each of 36 outcomes to a number from 2 to 12.\n")
cat("We care about the DISTRIBUTION: what values X takes and how often.\n\n")

n_sims <- 100000
die1 <- sample(1:6, n_sims, replace = TRUE)
die2 <- sample(1:6, n_sims, replace = TRUE)
X <- die1 + die2

cat("Simulated distribution of X (sum of two dice):\n")
print(round(table(X) / n_sims, 4))

cat("\nWhy is 7 most likely?\n")
cat("2: (1,1) -- 1 way\n")
cat("3: (1,2), (2,1) -- 2 ways\n")
cat("...\n")
cat("7: (1,6), (2,5), (3,4), (4,3), (5,2), (6,1) -- 6 ways\n")
cat("...\n")
cat("12: (6,6) -- 1 way\n\n")

# Visualization
probs <- table(X) / n_sims
bar_colors <- colorRampPalette(c(cols$light_blue, cols$blue, cols$purple))(11)

par(mar = c(4, 4, 3, 1))
bp <- barplot(probs, col = bar_colors, border = "white", 
              main = "Distribution of Sum of Two Dice",
              xlab = "Sum", ylab = "Probability", ylim = c(0, 0.2))

text(bp, as.numeric(probs) + 0.01, 
     labels = paste0(round(as.numeric(probs)*100, 1), "%"), 
     cex = 0.8, col = cols$gray)

ways <- c(1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1)
text(bp, -0.015, labels = paste0("(", ways, ")"), cex = 0.7, col = cols$gray)


# ----------------------------------------------------------------------------
# 3.2 Discrete vs Continuous Random Variables
# ----------------------------------------------------------------------------

cat("\n=== DISCRETE vs CONTINUOUS ===\n\n")

cat("DISCRETE: Takes countable values (integers, categories)\n")
cat("  - Has a Probability Mass Function (PMF): P(X = x)\n")
cat("  - Examples: die rolls, coin flips, counts\n\n")

cat("CONTINUOUS: Takes any value in an interval\n")
cat("  - Has a Probability Density Function (PDF)\n")
cat("  - P(X = x) = 0 for any specific value!\n")
cat("  - P(a < X < b) = area under PDF from a to b\n")
cat("  - Examples: height, weight, time\n\n")

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Binomial
n_sims <- 100000
binom_draws <- rbinom(n_sims, size = 10, prob = 0.5)
probs <- table(factor(binom_draws, levels = 0:10)) / n_sims

plot(0:10, as.numeric(probs), type = "h", lwd = 4, col = cols$blue,
     main = "Binomial(10, 0.5) - Discrete", xlab = "Number of Heads", 
     ylab = "Probability", ylim = c(0, 0.3))
points(0:10, as.numeric(probs), pch = 19, cex = 1.5, col = cols$blue)

# Normal
x_norm <- seq(-4, 4, length.out = 200)
y_norm <- dnorm(x_norm)

plot(x_norm, y_norm, type = "n", main = "Normal(0, 1) - Continuous",
     xlab = "x", ylab = "Density")
polygon(c(x_norm, rev(x_norm)), c(y_norm, rep(0, length(y_norm))),
        col = rgb(0.91, 0.31, 0.22, 0.5), border = cols$red, lwd = 2)

# Poisson
pois_draws <- rpois(n_sims, lambda = 3)
probs_pois <- table(factor(pois_draws, levels = 0:12)) / n_sims

plot(0:12, as.numeric(probs_pois), type = "h", lwd = 4, col = cols$green,
     main = "Poisson(3) - Discrete", xlab = "Count", 
     ylab = "Probability", ylim = c(0, 0.25))
points(0:12, as.numeric(probs_pois), pch = 19, cex = 1.5, col = cols$green)

# Exponential
x_exp <- seq(0, 6, length.out = 200)
y_exp <- dexp(x_exp, rate = 1)

plot(x_exp, y_exp, type = "n", main = "Exponential(1) - Continuous",
     xlab = "x", ylab = "Density")
polygon(c(x_exp, rev(x_exp)), c(y_exp, rep(0, length(y_exp))),
        col = rgb(0.95, 0.58, 0.22, 0.5), border = cols$orange, lwd = 2)

par(mfrow = c(1, 1))


# ============================================================================
# PART IV: EXPECTED VALUE
# ============================================================================

# ----------------------------------------------------------------------------
# 4.1 What is Expected Value?
# ----------------------------------------------------------------------------

cat("\n=== EXPECTED VALUE ===\n\n")

cat("E[X] = 'expected value' or 'mean' of X\n")
cat("     = weighted average of values, weighted by probabilities\n")
cat("     = 'center of mass' of the distribution\n")
cat("     = long-run average if we sampled X infinitely many times\n\n")

cat("For a die roll:\n")
cat("E[X] = 1*(1/6) + 2*(1/6) + 3*(1/6) + 4*(1/6) + 5*(1/6) + 6*(1/6)\n")
cat("     = (1 + 2 + 3 + 4 + 5 + 6) / 6 = 21/6 = 3.5\n\n")

die_rolls <- sample(1:6, 100000, replace = TRUE)
cat("Simulated mean from 100,000 rolls:", round(mean(die_rolls), 4), "\n\n")

cat("Note: You can never roll 3.5, but it's the expected value!\n")
cat("The expected value doesn't have to be a possible outcome.\n\n")

# Balance point visualization
par(mar = c(4, 4, 3, 1))

x_vals <- 1:6
probs <- rep(1/6, 6)

plot(NULL, xlim = c(0.5, 6.5), ylim = c(0, 0.25),
     xlab = "Die Face", ylab = "Probability",
     main = "Expected Value = Balance Point")

for (i in 1:6) {
  rect(i - 0.3, 0, i + 0.3, probs[i], col = cols$blue, border = "white")
}

segments(0.5, 0, 6.5, 0, lwd = 3, col = cols$gray)
polygon(x = c(3.3, 3.5, 3.7), y = c(-0.03, 0, -0.03), 
        col = cols$red, border = cols$red)
text(3.5, -0.05, "E[X] = 3.5", col = cols$red, font = 2, cex = 1.2)


# ----------------------------------------------------------------------------
# 4.2 Expected Value Minimizes Squared Error
# ----------------------------------------------------------------------------

cat("\n=== E[X] AS THE OPTIMAL PREDICTION ===\n\n")

cat("If you had to guess a single value for X, what's the best guess?\n")
cat("It depends on how you measure 'best'!\n\n")

cat("If penalized by SQUARED ERROR: Best guess = MEAN (E[X])\n")
cat("If penalized by ABSOLUTE ERROR: Best guess = MEDIAN\n\n")

set.seed(42)
X_samples <- rnorm(1000, mean = 5, sd = 2)

cat("Sample of 1000 values from Normal(5, 4):\n")
cat("Mean:", round(mean(X_samples), 4), "\n")
cat("Median:", round(median(X_samples), 4), "\n\n")

guesses <- seq(1, 9, length.out = 200)
mse_vals <- sapply(guesses, function(g) mean((X_samples - g)^2))
mae_vals <- sapply(guesses, function(g) mean(abs(X_samples - g)))

par(mar = c(4, 4, 3, 1))
plot(guesses, mse_vals, type = "l", lwd = 3, col = cols$blue,
     xlab = "Your Guess", ylab = "Average Loss",
     main = "Mean Minimizes Squared Error", ylim = c(0, max(mse_vals)))
lines(guesses, mae_vals, lwd = 3, col = cols$orange, lty = 2)

abline(v = mean(X_samples), col = cols$blue, lwd = 2, lty = 3)
abline(v = median(X_samples), col = cols$orange, lwd = 2, lty = 3)

points(mean(X_samples), min(mse_vals), pch = 19, cex = 2, col = cols$blue)
points(median(X_samples), min(mae_vals), pch = 19, cex = 2, col = cols$orange)

legend("topright", 
       legend = c("Squared Error (minimized by mean)", 
                  "Absolute Error (minimized by median)"),
       col = c(cols$blue, cols$orange), lty = c(1, 2), lwd = 3, bty = "n")


# ----------------------------------------------------------------------------
# 4.3 Properties of Expected Value
# ----------------------------------------------------------------------------

cat("\n=== PROPERTIES OF EXPECTED VALUE ===\n\n")

n_sims <- 100000
X <- rnorm(n_sims, mean = 3, sd = 1)
Y <- rnorm(n_sims, mean = 5, sd = 2)

cat("LINEARITY: E[aX + b] = a*E[X] + b\n")
cat("E[X] =", round(mean(X), 4), "  (theory: 3)\n")
cat("E[2X + 7] =", round(mean(2*X + 7), 4), "  (theory: 2*3 + 7 = 13)\n\n")

cat("ADDITIVITY: E[X + Y] = E[X] + E[Y]  (always true!)\n")
cat("E[X + Y] =", round(mean(X + Y), 4), "  (theory: 3 + 5 = 8)\n\n")

Y_dep <- 2*X + rnorm(n_sims, mean = 0, sd = 1)
cat("Even with dependence (Y = 2X + noise):\n")
cat("E[X + Y_dep] =", round(mean(X + Y_dep), 4), "  (theory: 3 + 6 = 9)\n\n")

cat("WARNING: E[g(X)] ≠ g(E[X]) in general!\n")
X <- rnorm(100000, mean = 2, sd = 3)
cat("E[X] =", round(mean(X), 4), "\n")
cat("(E[X])² =", round(mean(X)^2, 4), "\n")
cat("E[X²] =", round(mean(X^2), 4), "  (different!)\n")


# ============================================================================
# PART V: VARIANCE
# ============================================================================

# ----------------------------------------------------------------------------
# 5.1 What is Variance?
# ----------------------------------------------------------------------------

cat("\n=== VARIANCE ===\n\n")

cat("Var(X) = E[(X - E[X])²] = expected squared deviation from the mean\n")
cat("       = E[X²] - (E[X])²\n\n")

cat("Standard Deviation: SD(X) = sqrt(Var(X))  -- same units as X\n\n")

cat("Variance measures how 'spread out' the distribution is.\n\n")

# Compare distributions
X_low <- rnorm(10000, mean = 0, sd = 1)
X_high <- rnorm(10000, mean = 0, sd = 3)

cat("Two normal distributions with mean = 0:\n")
cat("SD = 1: Var =", round(var(X_low), 2), "\n")
cat("SD = 3: Var =", round(var(X_high), 2), "\n\n")

# Visualization
x <- seq(-12, 12, length.out = 300)

par(mar = c(4, 4, 3, 1))
plot(NULL, xlim = c(-12, 12), ylim = c(0, 0.45),
     xlab = "x", ylab = "Density",
     main = "Same Mean, Different Variances")

for (sd_val in c(1, 2, 4)) {
  y <- dnorm(x, mean = 0, sd = sd_val)
  alpha <- 0.3
  col_fill <- switch(as.character(sd_val),
                     "1" = rgb(0.18, 0.53, 0.67, alpha),
                     "2" = rgb(0.91, 0.31, 0.22, alpha),
                     "4" = rgb(0.11, 0.60, 0.55, alpha))
  col_line <- switch(as.character(sd_val),
                     "1" = cols$blue,
                     "2" = cols$red,
                     "4" = cols$green)
  polygon(c(x, rev(x)), c(y, rep(0, length(y))), col = col_fill, border = NA)
  lines(x, y, col = col_line, lwd = 3)
}

abline(v = 0, col = cols$gray, lwd = 2, lty = 2)

legend("topright", 
       legend = c("SD = 1 (Var = 1)", "SD = 2 (Var = 4)", "SD = 4 (Var = 16)"),
       col = c(cols$blue, cols$red, cols$green), lwd = 3, bty = "n")


# ----------------------------------------------------------------------------
# 5.2 Properties of Variance
# ----------------------------------------------------------------------------

cat("\n=== PROPERTIES OF VARIANCE ===\n\n")

n_sims <- 100000
X <- rnorm(n_sims, mean = 0, sd = 2)  # Var = 4
Y <- rnorm(n_sims, mean = 0, sd = 3)  # Var = 9 (independent)

cat("Var(aX + b) = a² * Var(X)  (adding constant doesn't affect spread!)\n")
cat("Var(X) =", round(var(X), 2), "  (theory: 4)\n")
cat("Var(3X + 10) =", round(var(3*X + 10), 2), "  (theory: 9*4 = 36)\n\n")

cat("For INDEPENDENT X and Y: Var(X + Y) = Var(X) + Var(Y)\n")
cat("Var(X + Y) =", round(var(X + Y), 2), "  (theory: 4 + 9 = 13)\n\n")

Y_dep <- X + rnorm(n_sims, mean = 0, sd = 1)
cat("If X and Y are DEPENDENT, this formula doesn't work:\n")
cat("Var(X + Y_dep) =", round(var(X + Y_dep), 2), "\n")
cat("Var(X) + Var(Y_dep) =", round(var(X) + var(Y_dep), 2), "  (different!)\n")


# ============================================================================
# PART VI: FAMOUS DISTRIBUTIONS
# ============================================================================

cat("\n=== DISTRIBUTION GALLERY ===\n\n")

cat("Key distributions you'll encounter:\n\n")

cat("DISCRETE:\n")
cat("- Bernoulli(p): Single trial, P(X=1)=p, P(X=0)=1-p\n")
cat("- Binomial(n,p): Number of successes in n trials\n")
cat("- Poisson(λ): Count of rare events, E[X] = Var(X) = λ\n\n")

cat("CONTINUOUS:\n")
cat("- Uniform(a,b): All values in [a,b] equally likely\n")
cat("- Normal(μ,σ²): The bell curve, E[X] = μ, Var(X) = σ²\n")
cat("- Exponential(λ): Time until event, E[X] = 1/λ\n\n")

par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

# Uniform
x_unif <- seq(0, 10, length.out = 200)
plot(x_unif, dunif(x_unif, 2, 8), type = "n", main = "Uniform(2, 8)", 
     xlab = "x", ylab = "Density", ylim = c(0, 0.2))
polygon(c(2, 2, 8, 8), c(0, 1/6, 1/6, 0), col = cols$blue, border = cols$blue)
text(5, 0.1, "E[X] = 5", font = 2)

# Normal
x_norm <- seq(-4, 4, length.out = 200)
y_norm <- dnorm(x_norm)
plot(x_norm, y_norm, type = "l", lwd = 3, col = cols$red,
     main = "Normal(0, 1)", xlab = "x", ylab = "Density")
polygon(c(x_norm, rev(x_norm)), c(y_norm, rep(0, length(y_norm))),
        col = rgb(0.91, 0.31, 0.22, 0.3), border = NA)
lines(x_norm, y_norm, lwd = 3, col = cols$red)

# Exponential
x_exp <- seq(0, 5, length.out = 200)
plot(x_exp, dexp(x_exp, 1), type = "l", lwd = 3, col = cols$green,
     main = "Exponential(1)", xlab = "x", ylab = "Density")
polygon(c(x_exp, rev(x_exp)), c(dexp(x_exp, 1), rep(0, length(x_exp))),
        col = rgb(0.11, 0.60, 0.55, 0.3), border = NA)
lines(x_exp, dexp(x_exp, 1), lwd = 3, col = cols$green)

# Binomial
x_binom <- 0:20
y_binom <- dbinom(x_binom, 20, 0.3)
plot(x_binom, y_binom, type = "h", lwd = 4, col = cols$orange,
     main = "Binomial(20, 0.3)", xlab = "x", ylab = "Probability")
points(x_binom, y_binom, pch = 19, col = cols$orange)

# Poisson
x_pois <- 0:15
y_pois <- dpois(x_pois, 5)
plot(x_pois, y_pois, type = "h", lwd = 4, col = cols$purple,
     main = "Poisson(5)", xlab = "x", ylab = "Probability")
points(x_pois, y_pois, pch = 19, col = cols$purple)

# Beta
x_beta <- seq(0, 1, length.out = 200)
plot(NULL, xlim = c(0, 1), ylim = c(0, 4),
     main = "Beta Distribution", xlab = "x", ylab = "Density")
lines(x_beta, dbeta(x_beta, 2, 5), lwd = 3, col = cols$blue)
lines(x_beta, dbeta(x_beta, 5, 2), lwd = 3, col = cols$red)
lines(x_beta, dbeta(x_beta, 2, 2), lwd = 3, col = cols$green)
legend("top", legend = c("Beta(2,5)", "Beta(5,2)", "Beta(2,2)"),
       col = c(cols$blue, cols$red, cols$green), lwd = 3, bty = "n", cex = 0.8)

par(mfrow = c(1, 1))


# ----------------------------------------------------------------------------
# 6.1 The Normal Distribution: 68-95-99.7 Rule
# ----------------------------------------------------------------------------

cat("\n=== THE 68-95-99.7 RULE ===\n\n")

cat("For a Normal distribution:\n")
cat("~68% of values fall within 1 SD of the mean\n")
cat("~95% of values fall within 2 SD of the mean\n")
cat("~99.7% of values fall within 3 SD of the mean\n\n")

n_sims <- 100000
normal_sims <- rnorm(n_sims, mean = 0, sd = 1)

cat("Verification with 100,000 simulated values:\n")
cat("Within 1 SD:", round(mean(abs(normal_sims) < 1) * 100, 1), "%\n")
cat("Within 2 SD:", round(mean(abs(normal_sims) < 2) * 100, 1), "%\n")
cat("Within 3 SD:", round(mean(abs(normal_sims) < 3) * 100, 1), "%\n\n")

par(mar = c(4, 4, 3, 1))

x <- seq(-4, 4, length.out = 500)
y <- dnorm(x)

plot(x, y, type = "n", xlab = "Standard Deviations from Mean", 
     ylab = "Density", main = "The 68-95-99.7 Rule",
     xaxt = "n")
axis(1, at = -3:3, labels = c("-3σ", "-2σ", "-1σ", "μ", "+1σ", "+2σ", "+3σ"))

# Fill regions
x_3sd <- x[x >= -3 & x <= 3]
y_3sd <- dnorm(x_3sd)
polygon(c(x_3sd, rev(x_3sd)), c(y_3sd, rep(0, length(y_3sd))),
        col = rgb(0.11, 0.60, 0.55, 0.3), border = NA)

x_2sd <- x[x >= -2 & x <= 2]
y_2sd <- dnorm(x_2sd)
polygon(c(x_2sd, rev(x_2sd)), c(y_2sd, rep(0, length(y_2sd))),
        col = rgb(0.91, 0.31, 0.22, 0.3), border = NA)

x_1sd <- x[x >= -1 & x <= 1]
y_1sd <- dnorm(x_1sd)
polygon(c(x_1sd, rev(x_1sd)), c(y_1sd, rep(0, length(y_1sd))),
        col = rgb(0.18, 0.53, 0.67, 0.5), border = NA)

lines(x, y, lwd = 3, col = cols$gray)

text(0, 0.15, "68%", font = 2, cex = 1.3, col = cols$blue)
text(0, 0.06, "95%", font = 2, cex = 1.1, col = cols$red)
text(0, 0.02, "99.7%", font = 2, cex = 0.9, col = cols$green)


# ============================================================================
# PART VII: JOINT DISTRIBUTIONS AND CORRELATION
# ============================================================================

# ----------------------------------------------------------------------------
# 7.1 Joint Distributions
# ----------------------------------------------------------------------------

cat("\n=== JOINT DISTRIBUTIONS ===\n\n")

cat("Two random variables X and Y have a JOINT distribution.\n")
cat("We can ask: P(X in A AND Y in B)?\n\n")

cat("The MARGINAL distribution of X is the distribution ignoring Y.\n")
cat("We get it by summing/integrating over all Y values.\n\n")

set.seed(42)
n <- 5000
height <- rnorm(n, mean = 170, sd = 10)
weight <- 0.8 * height - 66 + rnorm(n, sd = 8)

cat("Example: Height and Weight\n")
cat("Correlation:", round(cor(height, weight), 3), "\n\n")

# Visualization with marginals
layout(matrix(c(2, 0, 1, 3), 2, 2, byrow = TRUE),
       widths = c(4, 1), heights = c(1, 4))

par(mar = c(4, 4, 0, 0))

# 2D density
library(MASS)
dens <- kde2d(height, weight, n = 50)

image(dens, col = colorRampPalette(c("white", cols$light_blue, cols$blue, cols$purple))(100),
      xlab = "Height (cm)", ylab = "Weight (kg)")
contour(dens, add = TRUE, col = cols$gray, lwd = 0.5)
points(height[1:500], weight[1:500], pch = ".", col = rgb(0, 0, 0, 0.3))

# Top marginal
par(mar = c(0, 4, 2, 0))
hist(height, breaks = 40, main = "", xaxt = "n", col = cols$blue, border = "white", ylab = "")

# Right marginal
par(mar = c(4, 0, 0, 2))
hist(weight, breaks = 40, main = "", yaxt = "n", col = cols$red, border = "white",
     xlab = "", horizontal = TRUE)

par(mfrow = c(1, 1), mar = c(4, 4, 3, 1))


# ----------------------------------------------------------------------------
# 7.2 Covariance and Correlation
# ----------------------------------------------------------------------------

cat("\n=== COVARIANCE AND CORRELATION ===\n\n")

cat("Cov(X,Y) = E[(X - E[X])(Y - E[Y])]\n")
cat("         = E[XY] - E[X]E[Y]\n\n")

cat("Cov > 0: X and Y tend to move together\n")
cat("Cov < 0: X and Y tend to move opposite\n")
cat("Cov = 0: No LINEAR relationship (could still be related nonlinearly!)\n\n")

cat("CORRELATION standardizes to [-1, 1]:\n")
cat("Cor(X,Y) = Cov(X,Y) / (SD(X) * SD(Y))\n\n")

cat("Cor = 1: Perfect positive linear relationship\n")
cat("Cor = -1: Perfect negative linear relationship\n")
cat("Cor = 0: No linear relationship\n\n")

# Independence vs zero correlation
X_ind <- rnorm(10000)
Y_ind <- rnorm(10000)
cat("Independent X and Y: Cor =", round(cor(X_ind, Y_ind), 4), "\n\n")

X_dep <- rnorm(10000)
Y_dep <- X_dep^2
cat("Y = X² (clearly dependent!):\n")
cat("Cor(X, X²) =", round(cor(X_dep, Y_dep), 4), "\n")
cat("Zero correlation does NOT mean independence!\n\n")

# Correlation gallery
par(mfrow = c(2, 4), mar = c(3, 3, 3, 1))

set.seed(42)
n <- 150

for (r in c(-0.9, -0.5, 0, 0.5, 0.9)) {
  X <- rnorm(n)
  Y <- r * X + sqrt(1 - r^2) * rnorm(n)
  
  plot(X, Y, pch = 19, col = rgb(0.18, 0.53, 0.67, 0.6),
       main = paste("r =", r), xlab = "", ylab = "", cex = 1.2)
  abline(lm(Y ~ X), col = cols$red, lwd = 2)
}

# Non-linear examples
X <- runif(n, -2, 2)
Y <- X^2 + rnorm(n, sd = 0.5)
plot(X, Y, pch = 19, col = rgb(0.91, 0.31, 0.22, 0.6),
     main = paste("r =", round(cor(X, Y), 2), "(Quadratic)"), 
     xlab = "", ylab = "", cex = 1.2)

theta <- runif(n, 0, 2*pi)
X <- cos(theta) + rnorm(n, sd = 0.1)
Y <- sin(theta) + rnorm(n, sd = 0.1)
plot(X, Y, pch = 19, col = rgb(0.11, 0.60, 0.55, 0.6),
     main = paste("r =", round(cor(X, Y), 2), "(Circle)"), 
     xlab = "", ylab = "", cex = 1.2, asp = 1)

X <- c(runif(n/2, -2, 2), runif(n/2, -2, 2))
Y <- c(X[1:(n/2)] + rnorm(n/2, sd = 0.3), -X[(n/2+1):n] + rnorm(n/2, sd = 0.3))
plot(X, Y, pch = 19, col = rgb(0.95, 0.58, 0.22, 0.6),
     main = paste("r =", round(cor(X, Y), 2), "(X Pattern)"), 
     xlab = "", ylab = "", cex = 1.2)

par(mfrow = c(1, 1))


# ============================================================================
# PART VIII: CONDITIONAL EXPECTATION
# ============================================================================

# ----------------------------------------------------------------------------
# 8.1 What is Conditional Expectation?
# ----------------------------------------------------------------------------

cat("\n=== CONDITIONAL EXPECTATION ===\n\n")

cat("E[Y|X] = Expected value of Y, given that we know X\n")
cat("This is a FUNCTION of X!\n\n")

cat("For each value of X, E[Y|X=x] tells us the average Y among all\n")
cat("observations where X equals (or is close to) x.\n\n")

cat("KEY INSIGHT: E[Y|X] is the BEST predictor of Y given X!\n")
cat("It minimizes mean squared error: E[(Y - g(X))²]\n\n")

set.seed(42)
n <- 2000
X <- runif(n, 0, 10)
Y <- 2 + 0.5 * X + rnorm(n, sd = 1)

# Compute conditional means
n_bins <- 20
breaks <- seq(0, 10, length.out = n_bins + 1)
X_bin <- cut(X, breaks, include.lowest = TRUE)
conditional_means <- tapply(Y, X_bin, mean)
conditional_sds <- tapply(Y, X_bin, sd)
bin_centers <- (breaks[-length(breaks)] + breaks[-1]) / 2

cat("True relationship: E[Y|X] = 2 + 0.5X\n\n")
cat("Estimated conditional means by X bin:\n")
print(round(data.frame(X = bin_centers, E_Y_given_X = as.numeric(conditional_means)), 2))

par(mar = c(4, 4, 3, 1))

plot(NULL, xlim = c(0, 10), ylim = c(0, 9),
     xlab = "X", ylab = "Y", main = "Conditional Expectation E[Y|X]")

points(X, Y, pch = 16, col = rgb(0.5, 0.5, 0.5, 0.2), cex = 0.8)

# Show spread at each bin
for (i in 1:length(bin_centers)) {
  if (!is.na(conditional_means[i])) {
    segments(bin_centers[i], conditional_means[i] - conditional_sds[i],
             bin_centers[i], conditional_means[i] + conditional_sds[i],
             col = cols$orange, lwd = 3)
  }
}

points(bin_centers, conditional_means, pch = 19, cex = 2, col = cols$red)
lines(bin_centers, conditional_means, col = cols$red, lwd = 2)

abline(a = 2, b = 0.5, col = cols$blue, lwd = 3, lty = 2)

model <- lm(Y ~ X)
abline(model, col = cols$green, lwd = 3)

legend("topleft", 
       legend = c("Data", "Conditional means ± SD", "True E[Y|X]", "Fitted regression"),
       pch = c(16, 19, NA, NA), lty = c(NA, NA, 2, 1),
       col = c("gray", cols$red, cols$blue, cols$green), 
       lwd = c(NA, NA, 3, 3), pt.cex = c(1, 2, NA, NA), bty = "n")


# ----------------------------------------------------------------------------
# 8.2 Non-Linear Conditional Expectation
# ----------------------------------------------------------------------------

cat("\n=== NON-LINEAR CONDITIONAL EXPECTATION ===\n\n")

cat("E[Y|X] doesn't have to be linear!\n\n")

n <- 1000
X <- runif(n, 0, 4*pi)
Y <- sin(X) + rnorm(n, sd = 0.3)

breaks <- seq(0, 4*pi, length.out = 25)
X_bin <- cut(X, breaks)
conditional_means <- tapply(Y, X_bin, mean)
bin_centers <- (breaks[-length(breaks)] + breaks[-1]) / 2

par(mar = c(4, 4, 3, 1))

plot(X, Y, pch = 16, col = rgb(0, 0, 0, 0.3),
     main = "Non-Linear Conditional Expectation: E[Y|X] = sin(X)",
     xlab = "X", ylab = "Y")
points(bin_centers, conditional_means, pch = 19, col = cols$red, cex = 1.5)
curve(sin(x), add = TRUE, col = cols$blue, lwd = 2)
legend("topright", 
       legend = c("Data", "Estimated E[Y|X]", "True E[Y|X] = sin(X)"),
       pch = c(16, 19, NA), col = c("gray", cols$red, cols$blue),
       lty = c(NA, NA, 1), lwd = c(NA, NA, 2), bty = "n")


# ============================================================================
# PART IX: CENTRAL LIMIT THEOREM
# ============================================================================

# ----------------------------------------------------------------------------
# 9.1 Law of Large Numbers
# ----------------------------------------------------------------------------

cat("\n=== LAW OF LARGE NUMBERS ===\n\n")

cat("As sample size increases, the sample mean converges to the population mean.\n")
cat("This is why simulation works for computing probabilities!\n\n")

n_rolls <- 10000
die_rolls <- sample(1:6, n_rolls, replace = TRUE)
running_mean <- cumsum(die_rolls) / (1:n_rolls)

cat("Rolling a die 10,000 times:\n")
cat("After 10 rolls, mean =", round(running_mean[10], 4), "\n")
cat("After 100 rolls, mean =", round(running_mean[100], 4), "\n")
cat("After 1000 rolls, mean =", round(running_mean[1000], 4), "\n")
cat("After 10000 rolls, mean =", round(running_mean[10000], 4), "\n")
cat("True mean = 3.5\n\n")

par(mar = c(4, 4, 3, 1))

plot(1:n_rolls, running_mean, type = "l", col = cols$blue,
     xlab = "Number of Rolls", ylab = "Running Mean",
     main = "Law of Large Numbers", ylim = c(2.5, 4.5))
abline(h = 3.5, col = cols$red, lwd = 2, lty = 2)
legend("topright", legend = c("Running mean", "True mean (3.5)"),
       col = c(cols$blue, cols$red), lty = c(1, 2), lwd = 2, bty = "n")


# ----------------------------------------------------------------------------
# 9.2 Central Limit Theorem
# ----------------------------------------------------------------------------

cat("\n=== CENTRAL LIMIT THEOREM ===\n\n")

cat("The sum (or average) of many independent random variables is approximately\n")
cat("NORMAL, regardless of the original distribution!\n\n")

cat("This is why the normal distribution is so important in statistics.\n\n")

par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

sample_sizes <- c(1, 2, 5, 10, 30, 100)
n_sims <- 50000

for (n_sum in sample_sizes) {
  sums <- replicate(n_sims, sum(runif(n_sum)))
  sums_std <- (sums - n_sum * 0.5) / sqrt(n_sum * 1/12)
  
  hist(sums_std, breaks = 50, probability = TRUE, 
       col = cols$blue, border = "white",
       main = paste("Sum of", n_sum, "Uniform RVs"),
       xlab = "Standardized Sum", xlim = c(-4, 4), ylim = c(0, 0.5))
  
  curve(dnorm(x), add = TRUE, col = cols$red, lwd = 3)
}

par(mfrow = c(1, 1))

cat("Even though a single Uniform is flat (not normal at all),\n")
cat("the sum of many Uniforms becomes Normal!\n")


# ============================================================================
# PART X: FROM PROBABILITY TO REGRESSION
# ============================================================================

# ----------------------------------------------------------------------------
# 10.1 The Regression Model
# ----------------------------------------------------------------------------

cat("\n")
cat("================================================================\n")
cat("       THE CONNECTION: PROBABILITY -> REGRESSION\n")
cat("================================================================\n\n")

cat("The regression model assumes:\n")
cat("  Y = E[Y|X] + ε\n")
cat("where ε is random noise with E[ε|X] = 0\n\n")

cat("If we assume E[Y|X] is LINEAR in X:\n")
cat("  E[Y|X] = β₀ + β₁X\n\n")

cat("Then:\n")
cat("  Y = β₀ + β₁X + ε\n\n")

cat("This is the simple linear regression model!\n\n")

cat("Key insights:\n")
cat("1. Regression estimates E[Y|X] - the conditional expectation\n")
cat("2. E[Y|X] is the BEST predictor of Y given X (minimizes MSE)\n")
cat("3. Least squares finds the β's that minimize Σ(Y - Ŷ)²\n")
cat("4. By CLT, our estimates are approximately normal → inference!\n\n")


# ----------------------------------------------------------------------------
# 10.2 Least Squares
# ----------------------------------------------------------------------------

set.seed(42)
n <- 200
X <- runif(n, 0, 10)
Y <- 3 + 2 * X + rnorm(n, sd = 3)

model <- lm(Y ~ X)

cat("=== LEAST SQUARES ESTIMATION ===\n\n")
cat("True model: Y = 3 + 2X + noise\n\n")
cat("Fitted model:\n")
print(summary(model)$coefficients)


# ----------------------------------------------------------------------------
# 10.3 Sampling Distribution
# ----------------------------------------------------------------------------

cat("\n=== SAMPLING DISTRIBUTION OF ESTIMATES ===\n\n")

cat("Our estimates β̂ are random variables - they depend on the sample.\n")
cat("By CLT, they are approximately Normal.\n\n")

n_simulations <- 1000
beta1_estimates <- numeric(n_simulations)

for (i in 1:n_simulations) {
  X_sim <- runif(n, 0, 10)
  Y_sim <- 3 + 2 * X_sim + rnorm(n, sd = 3)
  beta1_estimates[i] <- coef(lm(Y_sim ~ X_sim))[2]
}

cat("Simulating 1000 datasets and fitting regression to each:\n")
cat("True β₁:", 2, "\n")
cat("Mean of estimates:", round(mean(beta1_estimates), 4), "\n")
cat("SD of estimates:", round(sd(beta1_estimates), 4), "\n")
cat("SE from single regression:", round(summary(model)$coef[2, 2], 4), "\n\n")

par(mar = c(4, 4, 3, 1))

hist(beta1_estimates, breaks = 40, probability = TRUE, 
     col = cols$light_blue, border = "white",
     main = "Sampling Distribution of Slope Estimate",
     xlab = expression(hat(beta)[1]))

curve(dnorm(x, mean = mean(beta1_estimates), sd = sd(beta1_estimates)),
      add = TRUE, col = cols$red, lwd = 3)

abline(v = 2, col = cols$green, lwd = 3, lty = 2)
abline(v = mean(beta1_estimates), col = cols$blue, lwd = 2)

ci <- quantile(beta1_estimates, c(0.025, 0.975))
arrows(ci[1], 0.5, ci[2], 0.5, code = 3, angle = 90, length = 0.1, 
       lwd = 3, col = cols$purple)
text(mean(ci), 0.6, "95% of estimates", col = cols$purple, font = 2)

legend("topright", 
       legend = c("True slope = 2", "Mean of estimates", "Normal approximation"),
       col = c(cols$green, cols$blue, cols$red), lty = c(2, 1, 1), 
       lwd = c(3, 2, 3), bty = "n")


# ----------------------------------------------------------------------------
# 10.4 R-squared: Variance Decomposition
# ----------------------------------------------------------------------------

cat("\n=== R-SQUARED: VARIANCE DECOMPOSITION ===\n\n")

cat("Total variance = Explained variance + Unexplained variance\n")
cat("     Var(Y)    =    Var(Ŷ)        +    Var(residuals)\n\n")

cat("R² = Var(Ŷ) / Var(Y) = proportion of variance explained by X\n\n")

Y_hat <- predict(model)
residuals <- Y - Y_hat

cat("Var(Y):", round(var(Y), 2), "\n")
cat("Var(Ŷ):", round(var(Y_hat), 2), "\n")
cat("Var(residuals):", round(var(residuals), 2), "\n")
cat("Sum:", round(var(Y_hat) + var(residuals), 2), "\n\n")
cat("R²:", round(summary(model)$r.squared, 4), "\n")

par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

# Total variation
plot(X, Y, pch = 16, col = rgb(0.5, 0.5, 0.5, 0.5),
     main = "Total Variation (SST)", xlab = "X", ylab = "Y")
abline(h = mean(Y), col = cols$red, lwd = 3)
segments(X, Y, X, mean(Y), col = rgb(0.91, 0.31, 0.22, 0.3))

# Explained variation
plot(X, Y, pch = 16, col = rgb(0.5, 0.5, 0.5, 0.5),
     main = "Explained Variation (SSR)", xlab = "X", ylab = "Y")
abline(model, col = cols$blue, lwd = 3)
abline(h = mean(Y), col = cols$red, lwd = 2, lty = 2)
segments(X, Y_hat, X, mean(Y), col = rgb(0.18, 0.53, 0.67, 0.3))

# Unexplained variation
plot(X, Y, pch = 16, col = rgb(0.5, 0.5, 0.5, 0.5),
     main = "Unexplained Variation (SSE)", xlab = "X", ylab = "Y")
abline(model, col = cols$blue, lwd = 3)
segments(X, Y, X, Y_hat, col = rgb(0.11, 0.60, 0.55, 0.3))

par(mfrow = c(1, 1))


# ----------------------------------------------------------------------------
# 10.5 Final Visualization
# ----------------------------------------------------------------------------

par(mar = c(4, 4, 3, 1))

set.seed(123)
n <- 100
X <- runif(n, 0, 10)
Y <- 2 + 1.5 * X + rnorm(n, sd = 2)
model <- lm(Y ~ X)

plot(X, Y, pch = 21, bg = cols$blue, col = "white", cex = 1.5,
     xlab = "X", ylab = "Y",
     main = "Regression: Estimating E[Y|X]")

newdata <- data.frame(X = seq(0, 10, length.out = 100))
pred_int <- predict(model, newdata, interval = "prediction")
conf_int <- predict(model, newdata, interval = "confidence")

polygon(c(newdata$X, rev(newdata$X)),
        c(pred_int[, "lwr"], rev(pred_int[, "upr"])),
        col = rgb(0.11, 0.60, 0.55, 0.2), border = NA)

polygon(c(newdata$X, rev(newdata$X)),
        c(conf_int[, "lwr"], rev(conf_int[, "upr"])),
        col = rgb(0.91, 0.31, 0.22, 0.3), border = NA)

abline(model, col = cols$red, lwd = 4)

points(X, Y, pch = 21, bg = cols$blue, col = "white", cex = 1.5)

legend("topleft", 
       legend = c("Data points",
                  "Fitted line",
                  "95% CI for E[Y|X]",
                  "95% Prediction interval"),
       pch = c(21, NA, NA, NA),
       lty = c(NA, 1, NA, NA),
       fill = c(NA, NA, rgb(0.91, 0.31, 0.22, 0.3), rgb(0.11, 0.60, 0.55, 0.2)),
       border = c("white", NA, NA, NA),
       pt.bg = c(cols$blue, NA, NA, NA),
       col = c("white", cols$red, NA, NA),
       lwd = c(NA, 4, NA, NA),
       pt.cex = 1.5, bty = "n")

eq <- paste0("Ŷ = ", round(coef(model)[1], 2), " + ", round(coef(model)[2], 2), "X")
text(8, 4, eq, font = 2, cex = 1.3, col = cols$red)
text(8, 2.5, paste0("R² = ", round(summary(model)$r.squared, 3)), 
     font = 2, cex = 1.1, col = cols$gray)


# ============================================================================
# SUMMARY
# ============================================================================

cat("\n")
cat("================================================================\n")
cat("                      KEY TAKEAWAYS\n")
cat("================================================================\n\n")

cat("PROBABILITY FOUNDATIONS:\n")
cat("- Probability = long-run frequency (simulation reveals truth)\n")
cat("- Conditional probability: P(A|B) restricts to a subspace\n")
cat("- Independence: knowing one event doesn't help predict another\n")
cat("- Random variables have distributions, means, and variances\n")
cat("- Covariance/correlation measure linear association\n\n")

cat("THE BRIDGE TO REGRESSION:\n")
cat("- E[Y|X] = conditional expectation = best predictor of Y given X\n")
cat("- E[Y|X] minimizes mean squared prediction error\n")
cat("- Linear regression assumes E[Y|X] = β₀ + β₁X\n")
cat("- Least squares estimates the conditional expectation\n")
cat("- CLT makes our estimates approximately normal → inference!\n")
cat("- R² = proportion of Y's variance explained by X\n\n")

cat("THE BIG PICTURE:\n")
cat("Regression is not just 'fitting a line' -- it's estimating\n")
cat("the conditional expectation function E[Y|X], which is the\n")
cat("optimal predictor of Y given X in the mean squared error sense.\n\n")
