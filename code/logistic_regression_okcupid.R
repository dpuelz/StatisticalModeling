# Logistic Regression Example: OkCupid profiles
# Predicting the sex of a user from their height.
# Uses okcupid.csv
#
# Companion to the slides: logistic-regression.pdf

library(tidyverse)

# ============================================================================
# Data Description
# ============================================================================
# okcupid.csv: 59,946 public profiles from the OkCupid online dating service
#   in the San Francisco area, with age, height, sex, orientation, income,
#   education, body type, ethnicity, and more.
#   Source: the teaching data set released with Kim & Escobedo-Land (2015),
#   Journal of Statistics Education.
#
# Research question: can we predict whether a user is male from height alone?
# The answer is the reason this is a logistic example -- the response is
# yes/no, not a number, so a straight line is the wrong tool.

profiles <- read.csv("../data/okcupid.csv")

dim(profiles)
head(profiles[, c("age", "height", "sex", "orientation")])

# ============================================================================
# 1. Why not just run a regression?
# ============================================================================

# Code the response as a dummy: male = 1, female = 0.
profiles$male <- ifelse(profiles$sex == "m", 1, 0)

# There are impossible heights in the raw data.
ggplot(profiles, aes(x = height)) + geom_boxplot()

# Keep heights between 55 and 80 inches (4'7" and 6'8").
# That drops only 120 of 59,946 cases.
my.profiles <- profiles %>% filter(height >= 55 & height <= 80)
nrow(profiles) - nrow(my.profiles)

# Fit a line anyway, and look at what it does.
set.seed(1)
samp <- my.profiles[sample(nrow(my.profiles), 2500), ]
ggplot(samp, aes(x = height, y = male)) +
  geom_jitter(height = 0.05, width = 0.45, alpha = 0.35, size = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "height (inches)", y = "male")

# The line leaves [0, 1]. What would it mean to predict male = 1.2?

# ============================================================================
# 2. Odds, and the logistic model
# ============================================================================

# odds = p / (1 - p).  Even odds (1/1) means p = 1/2; 2/1 odds means p = 2/3.
# Logistic regression makes the LOG ODDS a linear function of X:
#
#   log( p / (1 - p) ) = beta0 + beta1 * X
#
# Solving back for p gives an S-curve that never leaves [0, 1].

model <- glm(male ~ height, data = my.profiles, family = binomial)
summary(model)

# ============================================================================
# 3. Reading the fit
# ============================================================================

# log(odds of male) = -44.45 + 0.66 * height

# A prediction on the probability scale:
predict(model, data.frame(height = 69), type = "response")
# 0.77 -- someone 5'9" has about a 77% chance of being male.

# Exponentiate to get the odds interpretation. Slopes MULTIPLY here,
# they do not add.
exp(coef(model)["height"])        # 1.94: one more inch nearly doubles the odds
exp(2 * coef(model)["height"])    # 3.76: two more inches

# Draw the fitted curve from predict() on a grid, so the picture is
# always the model we just fit.
grid <- data.frame(height = seq(55, 80, length.out = 400))
grid$p <- predict(model, newdata = grid, type = "response")

ggplot(samp, aes(x = height, y = male)) +
  geom_jitter(height = 0.05, width = 0.45, alpha = 0.35, size = 0.7) +
  geom_line(data = grid, aes(x = height, y = p), linewidth = 1.1) +
  labs(x = "height (inches)", y = "P(male)")

# ============================================================================
# 4. How good is the model?
# ============================================================================

# No R^2 here. Instead: how many cases did we get right?
# Rule: predict male if the fitted probability is at least 0.5.
predicted.sex <- ifelse(predict(model, type = "response") >= 0.5, "m", "f")
correct <- ifelse(predicted.sex == my.profiles$sex, 1, 0)
sum(correct) / nrow(my.profiles)          # 0.83

# 83% of what?  Always guessing the majority gets 60%.
xtabs(~ sex, data = my.profiles) %>% prop.table()

# The confusion matrix: rows are what we PREDICTED, columns are the TRUTH.
tab_conf <- xtabs(~ predicted.sex + my.profiles$sex)
tab_conf

tab_conf["f", "m"]    # false negatives: men we called female
tab_conf["m", "f"]    # false positives: women we called male

# Normalizing by column turns the counts into rates within each actual group.
xtabs(~ predicted.sex + my.profiles$sex) %>% prop.table(2)
# We catch 81% of the women and 85% of the men.

# Two things to remember:
#   - 0.5 is a CHOICE. Move it and you trade false positives against
#     false negatives, which rarely cost the same.
#   - All of this is in-sample, and therefore optimistic. Hold out a test
#     set for an honest number.

# ============================================================================
# 5. More than one predictor
# ============================================================================

xtabs(~ orientation, data = my.profiles)

model2 <- glm(male ~ height + orientation, data = my.profiles,
              family = binomial)
summary(model2)

# Each coefficient is an effect on the log odds, holding the others fixed.
# Exponentiate for the odds interpretation:
exp(coef(model2))
# height: 1.95 per inch
# gay vs bisexual: 8.13x the odds;  straight vs bisexual: 4.05x

# Why are those dummies so large?  Orientation carries information about
# sex all by itself:
xtabs(~ sex + orientation, data = my.profiles) %>% prop.table(2)
# bisexual profiles are 72% female; gay profiles are 72% male.

# Three curves, same slope: adding a categorical predictor SHIFTS the
# curve sideways without changing its steepness (on the log-odds scale).
grid2 <- expand.grid(height = seq(55, 80, length.out = 400),
                     orientation = c("bisexual", "gay", "straight"))
grid2$p <- predict(model2, newdata = grid2, type = "response")

ggplot(grid2, aes(x = height, y = p, colour = orientation)) +
  geom_line(linewidth = 1.1) +
  labs(x = "height (inches)", y = "predicted P(male)")

# ============================================================================
# 6. Interactions
# ============================================================================

# Let the effect of height differ by orientation.
int.model <- glm(male ~ height * orientation, data = my.profiles,
                 family = binomial)
summary(int.model)

# Now each orientation gets its own slope on the log-odds scale:
#   bisexual 0.51,  gay 0.51 + 0.12 = 0.63,  straight 0.51 + 0.17 = 0.68
grid3 <- expand.grid(height = seq(55, 80, length.out = 400),
                     orientation = c("bisexual", "gay", "straight"))
grid3$p <- predict(int.model, newdata = grid3, type = "response")

ggplot(grid3, aes(x = height, y = p, colour = orientation)) +
  geom_line(linewidth = 1.1) +
  labs(x = "height (inches)", y = "predicted P(male)")

# ============================================================================
# 7. Hypothesis tests
# ============================================================================

# Overall null (no predictive power at all): the likelihood ratio test
# compares the model against an intercept-only model.
model2$null.deviance - model2$deviance      # chi-square = 36,932
anova(model2, test = "Chisq")               # same thing, term by term

# A single coefficient: read the z value and p-value off the summary table.
summary(model2)$coefficients

# ============================================================================
# Try it yourself
# ============================================================================
# 1. Add age as a third predictor. Does height still matter as much?
# 2. Move the classification threshold from 0.5 to 0.3 and to 0.7.
#    What happens to the false positives and false negatives?
# 3. Split the data 70/30 into training and test sets, fit on the training
#    half, and compute accuracy on the test half. How much does the
#    in-sample number flatter us?
