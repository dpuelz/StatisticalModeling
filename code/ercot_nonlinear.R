# Nonlinear Regression Example: ERCOT Power Grid Load
# Predicting electricity demand in Houston from outside temperature.
# Uses load_ercot.csv and load_temperature.csv
#
# Companion to the slides: modeling-nonlinear-relationships.pdf

library(tidyverse)
library(lubridate)
library(ggfortify)

# ============================================================================
# Data Description
# ============================================================================
# load_ercot.csv: power grid load every hour for 6 1/2 years across the
#   8 ERCOT regions of Texas. Units are megawatts -- this is peak
#   instantaneous demand for power in that hour.
#   Source: scraped from the ERCOT website.
#
# load_temperature.csv: temperature in Fahrenheit at the KHOU weather
#   station (Houston Hobby Airport), on the same hourly clock.
#
# We use COAST (the Houston region) as our response and KHOU as our
# predictor, so the load and the thermometer describe the same place.
#
# Research question: how does power demand respond to temperature?
# The answer is the reason this is a nonlinear example -- Texans run
# heaters when it is cold AND air conditioners when it is hot.

# ============================================================================
# Load and merge the data
# ============================================================================

load_ercot <- read.csv("../data/load_ercot.csv")
load_temperature <- read.csv("../data/load_temperature.csv")

# Merge on the common field: Time
load_combined <- merge(load_ercot, load_temperature, by = "Time")

# Time arrives as a character string; tell R it is a timestamp
load_combined <- mutate(load_combined, Time = ymd_hms(Time))

head(load_combined[, c("Time", "COAST", "KHOU")])
nrow(load_combined)

# ============================================================================
# Look at the data first
# ============================================================================
# Always plot before you model. The shape here does the arguing for us.

ggplot(load_combined, aes(x = KHOU, y = COAST)) +
  geom_point(colour = "#4A6FA5", alpha = 0.08, size = 0.9) +
  labs(x = "Temperature at KHOU (F)",
       y = "Houston-area grid load (MW)",
       title = "Power demand is a U-shaped function of temperature") +
  theme_bw(base_size = 12)

# The demand curve is a valley: heating load on the left, cooling load on
# the right, and a comfortable minimum somewhere around 60 F. No straight
# line can describe this.

# ============================================================================
# Model 1: the straight line (and why it fails)
# ============================================================================

lm1 <- lm(COAST ~ KHOU, data = load_combined)
summary(lm1)

# The slope is positive and hugely significant, which is worth pausing on:
# the model is confidently telling us that cold weather means low demand.
# That is an artifact of forcing a line through a valley.

ggplot(load_combined, aes(x = KHOU, y = COAST)) +
  geom_point(colour = "#4A6FA5", alpha = 0.08, size = 0.9) +
  geom_smooth(method = "lm", formula = y ~ x,
              colour = "#C75B39", se = FALSE, linewidth = 1.2) +
  labs(x = "Temperature at KHOU (F)", y = "Houston-area grid load (MW)") +
  theme_bw(base_size = 12)

# The residual plot shows the curvature the line refuses to capture:
autoplot(lm1, which = 1, ncol = 1, label = FALSE) + theme_bw(base_size = 11)

# ============================================================================
# Model 2: adding a quadratic term
# ============================================================================
# A parabola is the simplest curve with one minimum, which is exactly the
# shape we saw. Note this is still a LINEAR regression -- linear in the
# coefficients, curved in temperature.

lm2 <- lm(COAST ~ KHOU + I(KHOU^2), data = load_combined)
summary(lm2)

# Predict on a grid rather than hand-typing coefficients: the curve you
# draw is then guaranteed to be the model you fit.
grid <- data.frame(KHOU = seq(min(load_combined$KHOU),
                              max(load_combined$KHOU),
                              length.out = 500))
grid$quadratic <- predict(lm2, newdata = grid)

ggplot(load_combined, aes(x = KHOU, y = COAST)) +
  geom_point(colour = "#4A6FA5", alpha = 0.08, size = 0.9) +
  geom_line(data = grid, aes(x = KHOU, y = quadratic),
            colour = "#C75B39", linewidth = 1.2) +
  labs(x = "Temperature at KHOU (F)", y = "Houston-area grid load (MW)") +
  theme_bw(base_size = 12)

autoplot(lm2, which = 1, ncol = 1, label = FALSE) + theme_bw(base_size = 11)

# ============================================================================
# Model 3: the crazy model (degree 6)
# ============================================================================
# If two terms help, why not six? Let's find out what it actually buys.

lm3 <- lm(COAST ~ poly(KHOU, 6, raw = TRUE), data = load_combined)
summary(lm3)

grid$degree6 <- predict(lm3, newdata = grid)

# Both curves on one plot -- the honest comparison
curves <- grid %>%
  pivot_longer(cols = c(quadratic, degree6),
               names_to = "Model", values_to = "Fit") %>%
  mutate(Model = factor(Model, levels = c("quadratic", "degree6"),
                        labels = c("Quadratic", "Degree 6")))

ggplot(load_combined, aes(x = KHOU, y = COAST)) +
  geom_point(colour = "#4A6FA5", alpha = 0.08, size = 0.9) +
  geom_line(data = curves, aes(x = KHOU, y = Fit,
                               colour = Model, linetype = Model),
            linewidth = 1.2) +
  scale_colour_manual(values = c(Quadratic = "#B48C28",
                                 `Degree 6` = "#C75B39")) +
  scale_linetype_manual(values = c(Quadratic = "dashed",
                                   `Degree 6` = "solid")) +
  labs(x = "Temperature at KHOU (F)", y = "Houston-area grid load (MW)",
       colour = NULL, linetype = NULL) +
  theme_bw(base_size = 12) +
  theme(legend.position = "top")

# What did four extra parameters actually buy?
r2 <- sapply(1:6, function(d)
  summary(lm(COAST ~ poly(KHOU, d, raw = TRUE),
             data = load_combined))$r.squared)
data.frame(degree = 1:6,
           r.squared = round(r2, 4),
           gain = c(NA, round(diff(r2), 4)))

# The jump from degree 1 to 2 is the whole story. Everything after it is
# rounding error -- and each extra term is another way to go wrong outside
# the range of the data.

# ============================================================================
# Extrapolation: where the crazy model gets punished
# ============================================================================
# Texas grid planning is all about the extremes. So ask what each model
# predicts at temperatures the data barely covers.

range(load_combined$KHOU)

wide <- data.frame(KHOU = seq(0, 120, length.out = 500))
wide$Quadratic <- predict(lm2, newdata = wide)
wide$`Degree 6` <- predict(lm3, newdata = wide)

wide_long <- pivot_longer(wide, cols = -KHOU,
                          names_to = "Model", values_to = "Fit")

ggplot(load_combined, aes(x = KHOU, y = COAST)) +
  geom_point(colour = "#4A6FA5", alpha = 0.06, size = 0.9) +
  geom_line(data = wide_long, aes(x = KHOU, y = Fit, colour = Model),
            linewidth = 1.2) +
  scale_colour_manual(values = c(Quadratic = "#B48C28",
                                 `Degree 6` = "#C75B39")) +
  coord_cartesian(ylim = c(0, max(load_combined$COAST) * 1.3)) +
  labs(x = "Temperature at KHOU (F)", y = "Houston-area grid load (MW)",
       colour = NULL) +
  theme_bw(base_size = 12) +
  theme(legend.position = "top")

# Predicted load at a 10 F hard freeze and a 115 F heat wave:
freeze_heat <- data.frame(KHOU = c(10, 115))
data.frame(KHOU = freeze_heat$KHOU,
           quadratic = round(predict(lm2, newdata = freeze_heat)),
           degree6 = round(predict(lm3, newdata = freeze_heat)))

# ============================================================================
# Takeaways
# ============================================================================
# 1. Plot the data first. The U-shape rules out a line before you fit one.
# 2. A quadratic term is still linear regression, and here it captures
#    essentially all of the structure.
# 3. Higher degrees buy almost no in-sample R^2 and behave terribly at the
#    edges -- which, for a power grid, is the only place that matters.
# 4. A model can be wildly significant and still be wrong about the thing
#    you care about. Model 1 has a tiny p-value and the wrong shape.
