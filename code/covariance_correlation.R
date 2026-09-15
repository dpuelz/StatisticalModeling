# Covariance and Correlation
# Statistical Modeling and Learning
#
# The idea in one line: covariance is the AVERAGE SIGNED AREA of the rectangles
# you get by drawing from the mean-point out to each data point. Correlation is
# the same thing after you delete the units.
#
# Run this top to bottom, one block at a time.

# ============================================================================
# 1. THE DATA (same numbers as the lecture slides)
# ============================================================================

set.seed(42)
n <- 200
netflix_hours <- runif(n, 0, 30)                       # hours per week
body_fat <- 15 + -0.6 * netflix_hours + rnorm(n, 0, 3)  # percent
body_fat <- pmax(5, pmin(40, body_fat))                # keep it plausible

x <- netflix_hours
y <- body_fat

plot(x, y, pch = 16, col = rgb(0, 0, 0, 0.3),
     xlab = "Hours watched on Netflix per week",
     ylab = "Body fat percentage (%)")

# ============================================================================
# 2. COVARIANCE, BUILT BY HAND
# ============================================================================

# Step 1: how far is each point from the mean, in each direction?
dx <- x - mean(x)
dy <- y - mean(y)

# Step 2: multiply the two deviations for each point.
products <- dx * dy

# Step 3: average them (dividing by n-1, same reason as the variance).
my_cov <- sum(products) / (n - 1)

my_cov
cov(x, y)        # R agrees

# Look at a few points to see what the product is doing.
round(head(data.frame(x, y, dx, dy, products)),2)

# The sign is the whole story:
#   above-average x AND above-average y  -> (+)(+) = positive
#   above-average x BUT below-average y  -> (+)(-) = negative
# Covariance just asks: which kind of point wins, on average?

# ============================================================================
# 3. THE PICTURE: EVERY POINT IS A SIGNED RECTANGLE
# ============================================================================

# dx * dy is literally the area of a rectangle with corners at the mean-point
# and the data point. Positive area = blue, negative area = red.
#
# 200 rectangles is soup, so draw a random 12 of them.

set.seed(1)
i <- sample(n, 100)

plot(dx[i], dy[i], type = "n",
     xlab = "Netflix hours - mean", ylab = "Body fat - mean",
     main = "Each point contributes a signed rectangle")

for (k in i) {
  fill <- if (dx[k] * dy[k] > 0) rgb(0, 0, 1, 0.18) else rgb(1, 0, 0, 0.18)
  rect(0, 0, dx[k], dy[k], col = fill, border = NA)
}

abline(h = 0, v = 0, lty = 2, col = "grey40")
points(dx[i], dy[i], pch = 16)
legend("topleft", c("positive", "negative"), bty = "n",
       fill = c(rgb(0, 0, 1, 0.18), rgb(1, 0, 0, 0.18)), border = NA)

# Covariance = the average of those signed areas, over all 200 points.
mean_area <- sum(dx * dy) / (n - 1)
mean_area

# ============================================================================
# 4. COUNTING THE QUADRANTS
# ============================================================================

# Same idea, no geometry: just tally which quadrant each point lands in.
quadrant <- ifelse(dx > 0 & dy > 0, "I  (+)",
            ifelse(dx < 0 & dy > 0, "II (-)",
            ifelse(dx < 0 & dy < 0, "III(+)", "IV (-)")))

table(quadrant)

# And how much total area each quadrant contributes:
tapply(dx * dy, quadrant, sum)

# I and III outweigh II and IV -> positive covariance.

# ============================================================================
# 5. WHY COVARIANCE ALONE IS USELESS: IT HAS UNITS
# ============================================================================

# Nothing about the relationship changed -- only the clock we measured it with.
x_minutes <- x * 60

cov(x, y)          # hours * percent
cov(x_minutes, y)  # minutes * percent -- 60x bigger!

cov(x_minutes, y) / cov(x, y)   # exactly 60

# So "the covariance is 20" means nothing on its own. Twenty WHAT?
# Correlation fixes this by dividing the units out.

cor(x, y)
cor(x_minutes, y)   # identical -- units are gone

# ============================================================================
# 6. CORRELATION = COVARIANCE OF Z-SCORES
# ============================================================================

# Standardize: subtract the mean, divide by the SD. Now both variables are
# unitless, measured in "SDs away from average".
zx <- (x - mean(x)) / sd(x)
zy <- (y - mean(y)) / sd(y)

sum(zx * zy) / (n - 1)   # correlation, from scratch
cov(zx, zy)              # same thing
cor(x, y)                # and R agrees

# The familiar formula is just those three lines rearranged:
cov(x, y) / (sd(x) * sd(y))

# Correlation is bounded: -1 <= r <= 1. A point can only pull r toward 1 if
# it sits the SAME number of SDs out in both directions.

# ============================================================================
# 7. THE BRIDGE TO REGRESSION
# ============================================================================

# The least-squares slope is just the correlation, put back into units:
#     b1 = r * sd(y) / sd(x)
r <- cor(x, y)
b1 <- r * sd(y) / sd(x)
b0 <- mean(y) - b1 * mean(x)

c(intercept = b0, slope = b1)
coef(lm(y ~ x))          # same numbers

# In standardized units the slope IS the correlation:
coef(lm(zy ~ zx))        # slope = r, intercept = 0

# ============================================================================
# 8. THE WARNING: r IS ONLY ONE NUMBER
# ============================================================================

# Anscombe's quartet ships with R. Four datasets, four different stories,
# and essentially the SAME correlation.

round(c(cor(anscombe$x1, anscombe$y1),
        cor(anscombe$x2, anscombe$y2),
        cor(anscombe$x3, anscombe$y3),
        cor(anscombe$x4, anscombe$y4)), 3)

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
for (j in 1:4) {
  xj <- anscombe[[paste0("x", j)]]
  yj <- anscombe[[paste0("y", j)]]
  plot(xj, yj, pch = 16, col = "firebrick",
       xlab = "x", ylab = "y",
       main = paste0("Set ", j, ":  r = ", round(cor(xj, yj), 3)))
  abline(lm(yj ~ xj), lwd = 2)
}
par(mfrow = c(1, 1))

# Set 1 is an honest linear relationship. Set 2 is a curve. Set 3 is a perfect
# line plus one outlier. Set 4 is one influential point doing all the work.
# Same r. ALWAYS PLOT THE DATA.

# ============================================================================
# 9. OPTIONAL: GUESS THE CORRELATION
# ============================================================================

# Run guess_r() in the console to test your eye. Ctrl-C / Esc to stop.

guess_r <- function(m = 100) {
  truth <- runif(1, -1, 1)
  u <- rnorm(m)
  v <- truth * u + sqrt(1 - truth^2) * rnorm(m)
  plot(u, v, pch = 16, col = rgb(0, 0, 0, 0.5),
       xlab = "x", ylab = "y", main = "What's the correlation?")
  ans <- as.numeric(readline("Your guess for r: "))
  cat("actual r =", round(cor(u, v), 2),
      " your guess =", ans,
      " off by", round(abs(cor(u, v) - ans), 2), "\n")
}
