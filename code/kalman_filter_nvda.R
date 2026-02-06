# =============================================================================
# Kalman Filter on NVIDIA (NVDA) Daily Returns
# Fetches data as far back as possible up to yesterday, then applies the
# local-level Kalman filter with different smoothing levels.
# =============================================================================

library(quantmod)   # getSymbols() for Yahoo Finance; install if needed: install.packages("quantmod")

# -----------------------------------------------------------------------------
# 1. Fetch NVIDIA daily data (as far back as possible, through yesterday)
# -----------------------------------------------------------------------------
cat("Fetching NVIDIA (NVDA) daily data...\n")
nvda <- getSymbols("NVDA", from = "1999-01-01", to = Sys.Date() - 1,
                   auto.assign = FALSE, src = "yahoo")

# Use adjusted close; drop rows with NA
price <- as.numeric(Ad(nvda))
dates <- index(nvda)
ok    <- !is.na(price)
price <- price[ok]
dates <- dates[ok]

# Daily log returns
returns <- diff(log(price))
dates   <- dates[-1]
returns <- as.numeric(returns)
returns <- returns[!is.na(returns)]
dates   <- dates[!is.na(returns)]

n <- length(returns)
cat("Using", n, "daily log returns from", as.character(dates[1]), "to", as.character(dates[n]), "\n")

# Scale returns to percentage for readability (optional; filter is scale-invariant
# in shape, but V/W need to match the scale)
Y <- returns * 100

# -----------------------------------------------------------------------------
# 2. Kalman filter (local level model)
#   Observation: Y_t = theta_t + v_t,  v_t ~ N(0, V)
#   State:       theta_t = theta_{t-1} + w_t,  w_t ~ N(0, W)
#   PREDICT: R_t = C_{t-1} + W,  a_t = m_{t-1}
#   UPDATE:  K_t = R_t/(R_t+V),  m_t = a_t + K_t*(Y_t - a_t),  C_t = (1-K_t)*R_t
# -----------------------------------------------------------------------------
kalman_filter <- function(Y, V, W, m0, C0) {
  n <- length(Y)
  m <- numeric(n)
  C <- numeric(n)
  a <- m0
  R <- C0
  for (t in 1:n) {
    R <- (if (t > 1) C[t - 1] else C0) + W
    K <- R / (R + V)
    m[t] <- a + K * (Y[t] - a)
    C[t] <- (1 - K) * R
    a <- m[t]
  }
  list(m = m, C = C)
}

# -----------------------------------------------------------------------------
# 3. Choose V and W
#    V = observation variance (we use sample variance of returns)
#    W = state variance; ratio W/V controls smoothing (small W/V = more smoothing)
# -----------------------------------------------------------------------------
V_obs <- var(Y)
# Smoothing levels (W/V): large = little smoothing, small = more smoothing
W_little  <- V_obs * 1
W_some    <- V_obs * 0.1
W_extreme <- V_obs * 0.01

# Initial prior: diffuse (first observation, large variance)
m0 <- Y[1]
C0 <- 10 * V_obs

# Run filters
kf_little  <- kalman_filter(Y, V_obs, W_little,  m0, C0)
kf_some    <- kalman_filter(Y, V_obs, W_some,    m0, C0)
kf_extreme <- kalman_filter(Y, V_obs, W_extreme, m0, C0)

# -----------------------------------------------------------------------------
# 4. Step-by-step plotting (x-axis by month; 95% intervals shown clearly)
# -----------------------------------------------------------------------------
# Use dates for x-axis so we can label by month
x_dates <- as.Date(dates)
ylab <- "Daily return"

# Monthly axis: tick every 6 months for readability
axis_dates <- seq(min(x_dates), max(x_dates), by = "6 months")

# Line (dark) and band (light) colors per smoothing level
# Little:  navy + light blue
# Some:    dark green + green
# Extreme: dark red + pink
line_little  <- "navy"
band_little  <- rgb(0.68, 0.85, 1, 0.75)      # light blue (shaded)
edge_little  <- rgb(0.68, 0.85, 1, 1)          # same color, solid for interval lines
line_some    <- "darkgreen"
band_some    <- rgb(0.5, 0.85, 0.5, 0.75)     # light green (shaded)
edge_some    <- rgb(0.5, 0.85, 0.5, 1)
line_extreme <- "darkred"
band_extreme <- rgb(1, 0.75, 0.8, 0.75)       # pink (shaded)
edge_extreme <- rgb(1, 0.75, 0.8, 1)

# ---- Plot 1: Raw data only ----
plot(x_dates, Y, type = "p", pch = 16, col = "darkgray", cex = 0.4,
     xlab = "", ylab = ylab,
     main = "NVIDIA daily log returns (raw data)", xaxt = "n")
axis.Date(1, at = axis_dates, format = "%b %Y", las = 2, cex.axis = 0.85)
grid()

# ---- Plot 2: Raw data + little smoothing + 95% band ----
u1 <- kf_little$m + 1.96 * sqrt(kf_little$C)
l1 <- kf_little$m - 1.96 * sqrt(kf_little$C)
plot(x_dates, Y, type = "p", pch = 16, col = "darkgray", cex = 0.4,
     xlab = "", ylab = ylab,
     main = "Kalman filter: little smoothing (W/V = 1)", xaxt = "n",
     ylim = range(c(l1, u1, Y), na.rm = TRUE))
polygon(c(x_dates, rev(x_dates)), c(u1, rev(l1)), col = band_little, border = NA)
lines(x_dates, u1, col = edge_little, lty = 1, lwd = 0.8)
lines(x_dates, l1, col = edge_little, lty = 1, lwd = 0.8)
lines(x_dates, kf_little$m, col = line_little, lwd = 1.2)
legend("topright", c("Observed", "Filter (W/V=1)", "95% CI"), col = c("darkgray", line_little, edge_little),
       pch = c(16, NA, 15), lty = c(NA, 1, 1), lwd = c(NA, 1.2, NA), bty = "n", cex = 0.9)
axis.Date(1, at = axis_dates, format = "%b %Y", las = 2, cex.axis = 0.85)
grid()

# ---- Plot 3: Raw data + some smoothing + 95% band ----
u2 <- kf_some$m + 1.96 * sqrt(kf_some$C)
l2 <- kf_some$m - 1.96 * sqrt(kf_some$C)
plot(x_dates, Y, type = "p", pch = 16, col = "darkgray", cex = 0.4,
     xlab = "", ylab = ylab,
     main = "Kalman filter: some smoothing (W/V = 0.1)", xaxt = "n",
     ylim = range(c(l2, u2, Y), na.rm = TRUE))
polygon(c(x_dates, rev(x_dates)), c(u2, rev(l2)), col = band_some, border = NA)
lines(x_dates, u2, col = edge_some, lty = 1, lwd = 0.8)
lines(x_dates, l2, col = edge_some, lty = 1, lwd = 0.8)
lines(x_dates, kf_some$m, col = line_some, lwd = 1.2)
legend("topright", c("Observed", "Filter (W/V=0.1)", "95% CI"), col = c("darkgray", line_some, edge_some),
       pch = c(16, NA, 15), lty = c(NA, 1, 1), lwd = c(NA, 1.2, NA), bty = "n", cex = 0.9)
axis.Date(1, at = axis_dates, format = "%b %Y", las = 2, cex.axis = 0.85)
grid()

# ---- Plot 4: Raw data + extreme smoothing + 95% band ----
u3 <- kf_extreme$m + 1.96 * sqrt(kf_extreme$C)
l3 <- kf_extreme$m - 1.96 * sqrt(kf_extreme$C)
plot(x_dates, Y, type = "p", pch = 16, col = "darkgray", cex = 0.4,
     xlab = "", ylab = ylab,
     main = "Kalman filter: extreme smoothing (W/V = 0.01)", xaxt = "n",
     ylim = range(c(l3, u3, Y), na.rm = TRUE))
polygon(c(x_dates, rev(x_dates)), c(u3, rev(l3)), col = band_extreme, border = NA)
lines(x_dates, u3, col = edge_extreme, lty = 1, lwd = 0.8)
lines(x_dates, l3, col = edge_extreme, lty = 1, lwd = 0.8)
lines(x_dates, kf_extreme$m, col = line_extreme, lwd = 1.2)
legend("topright", c("Observed", "Filter (W/V=0.01)", "95% CI"), col = c("darkgray", line_extreme, edge_extreme),
       pch = c(16, NA, 15), lty = c(NA, 1, 1), lwd = c(NA, 1.2, NA), bty = "n", cex = 0.9)
axis.Date(1, at = axis_dates, format = "%b %Y", las = 2, cex.axis = 0.85)
grid()

# ---- Plot 5: All three smoothing levels + 95% bands ----
plot(x_dates, Y, type = "p", pch = 16, col = "darkgray", cex = 0.3,
     xlab = "", ylab = ylab,
     main = "NVIDIA daily returns: three smoothing levels with 95% intervals", xaxt = "n",
     ylim = range(c(l1, u1, l2, u2, l3, u3, Y), na.rm = TRUE))
polygon(c(x_dates, rev(x_dates)), c(u1, rev(l1)), col = band_little, border = NA)
polygon(c(x_dates, rev(x_dates)), c(u2, rev(l2)), col = band_some, border = NA)
polygon(c(x_dates, rev(x_dates)), c(u3, rev(l3)), col = band_extreme, border = NA)
lines(x_dates, u1, col = edge_little, lty = 1, lwd = 0.6)
lines(x_dates, l1, col = edge_little, lty = 1, lwd = 0.6)
lines(x_dates, u2, col = edge_some, lty = 1, lwd = 0.6)
lines(x_dates, l2, col = edge_some, lty = 1, lwd = 0.6)
lines(x_dates, u3, col = edge_extreme, lty = 1, lwd = 0.6)
lines(x_dates, l3, col = edge_extreme, lty = 1, lwd = 0.6)
lines(x_dates, kf_little$m,  col = line_little,  lwd = 1)
lines(x_dates, kf_some$m,    col = line_some,    lwd = 1)
lines(x_dates, kf_extreme$m, col = line_extreme, lwd = 1)
legend("topright",
       c("Observed", "Little (W/V=1)", "Some (W/V=0.1)", "Extreme (W/V=0.01)", "95% CI"),
       col = c("darkgray", line_little, line_some, line_extreme, edge_some),
       pch = c(16, NA, NA, NA, NA), lty = c(NA, 1, 1, 1, 1), lwd = c(NA, 1, 1, 1, 0.8),
       bty = "n", cex = 0.8)
axis.Date(1, at = axis_dates, format = "%b %Y", las = 2, cex.axis = 0.85)
grid()

# ---- Plot 6: Filter with 95% uncertainty band (using "some" smoothing) ----
plot(x_dates, Y, type = "p", pch = 16, col = "darkgray", cex = 0.4,
     xlab = "", ylab = ylab,
     main = "Kalman filter (W/V=0.1) with 95% uncertainty band", xaxt = "n",
     ylim = range(c(l2, u2, Y), na.rm = TRUE))
polygon(c(x_dates, rev(x_dates)), c(u2, rev(l2)), col = band_some, border = NA)
lines(x_dates, u2, col = edge_some, lty = 1, lwd = 0.8)
lines(x_dates, l2, col = edge_some, lty = 1, lwd = 0.8)
lines(x_dates, kf_some$m, col = line_some, lwd = 1.2)
legend("topright", c("Observed", "Filter", "95% CI"),
       col = c("darkgray", line_some, edge_some),
       pch = c(16, NA, 15), lty = c(NA, 1, 1), lwd = c(NA, 1.2, NA), bty = "n", cex = 0.9)
axis.Date(1, at = axis_dates, format = "%b %Y", las = 2, cex.axis = 0.85)
grid()

cat("Done. V =", round(V_obs, 6), ", W (little/some/extreme) =",
    round(W_little, 6), "/", round(W_some, 6), "/", round(W_extreme, 6), "\n")
