# Prepare minwage.csv for HW5 (Card & Krueger 1994 DiD analysis)
# Run this script to create minwage.csv in the data folder if not present.

# Option 1: Use AER package (install with: install.packages("AER"))
if (requireNamespace("AER", quietly = TRUE)) {
  data(NJMinWage, package = "AER")
  # AER has 'njmin' in wide format - variable names may differ
  # Check: ?NJMinWage for structure
  if (exists("njmin")) {
    # Convert to our expected format if needed
    # njmin has: sheet, chain, state, emp1, emp2, ...
    # emp1 = FTE before, emp2 = FTE after
    # state: 0=PA, 1=NJ
    minwage <- data.frame(
      location = ifelse(njmin$state == 1, "NJ", "PA"),
      chain = njmin$chain,
      fullBefore = njmin$emp1,  # FTE before
      partBefore = 0,           # AER may not have part-time separately
      fullAfter = njmin$emp2,
      partAfter = 0
    )
    write.csv(minwage, file = "../data/minwage.csv", row.names = FALSE)
    message("Saved minwage.csv from AER::njmin")
  }
} else {
  message("AER package not installed. Install with: install.packages('AER')")
  message("Or download data from: https://davidcard.berkeley.edu/data_sets.html")
}
