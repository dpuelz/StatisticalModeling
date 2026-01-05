# Introduction to R
# Statistical Modeling and Learning
# Basic R concepts with interesting examples

# ============================================================================
# 1. BASIC OPERATIONS
# ============================================================================

# R as a calculator
2 + 3
10 * 5
sqrt(16)
2^3

# Variables
x <- 5
y <- 10
x + y

# ============================================================================
# 2. VECTORS (COLLECTIONS OF VALUES)
# ============================================================================

# Create vectors
ages <- c(18, 19, 20, 21, 22, 23, 24, 25)
heights <- c(65, 67, 68, 70, 72, 69, 71, 73)  # inches

# Basic operations on vectors
mean(ages)
sd(heights)
length(ages)

# Vectorized operations (R does this automatically!)
heights_cm <- heights * 2.54  # Convert to centimeters
heights_cm

# ============================================================================
# 3. DATA FRAMES (TABLES OF DATA)
# ============================================================================

# Create a data frame
students <- data.frame(
  name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
  age = c(20, 21, 19, 22, 20),
  gpa = c(3.8, 3.5, 3.9, 3.7, 3.6),
  major = c("Math", "CS", "Math", "Stats", "CS")
)

# View the data
students
head(students)
str(students)  # Structure of the data

# Access columns
students$age
students$gpa

# Basic summaries
summary(students)
mean(students$gpa)
table(students$major)

# ============================================================================
# 4. SUBSETTING DATA
# ============================================================================

# Get students with GPA > 3.7
high_gpa <- students[students$gpa > 3.7, ]
high_gpa

# Get just the names of math majors
math_majors <- students[students$major == "Math", "name"]
math_majors

# ============================================================================
# 5. BASIC PLOTTING
# ============================================================================

# Scatter plot: Age vs GPA
plot(students$age, students$gpa,
     xlab = "Age", ylab = "GPA",
     main = "Age vs GPA",
     pch = 19, col = "blue")

# Histogram of GPAs
hist(students$gpa,
     xlab = "GPA", ylab = "Frequency",
     main = "Distribution of GPAs",
     col = "lightblue")

# Bar plot of majors
barplot(table(students$major),
        xlab = "Major", ylab = "Count",
        main = "Number of Students by Major",
        col = c("red", "green", "blue"))

# ============================================================================
# 6. INTERESTING EXAMPLE: SIMULATING COIN FLIPS
# ============================================================================

# Simulate 100 coin flips
coin_flips <- sample(c("Heads", "Tails"), size = 100, replace = TRUE)
table(coin_flips)

# What proportion are heads?
mean(coin_flips == "Heads")

# Simulate 10,000 flips (law of large numbers!)
many_flips <- sample(c("Heads", "Tails"), size = 10000, replace = TRUE)
mean(many_flips == "Heads")  # Should be close to 0.5!

# ============================================================================
# 7. INTERESTING EXAMPLE: ROLLING DICE
# ============================================================================

# Roll a die 100 times
die_rolls <- sample(1:6, size = 100, replace = TRUE)
table(die_rolls)

# What's the average roll?
mean(die_rolls)  # Should be around 3.5

# Roll two dice and sum them
die1 <- sample(1:6, size = 1000, replace = TRUE)
die2 <- sample(1:6, size = 1000, replace = TRUE)
sums <- die1 + die2

# Plot the distribution
hist(sums, breaks = 2:12,
     xlab = "Sum of Two Dice",
     ylab = "Frequency",
     main = "Distribution of Sum of Two Dice",
     col = "lightgreen")

# ============================================================================
# 8. INTERESTING EXAMPLE: STUDENT PERFORMANCE
# ============================================================================

# Create data on study hours and exam scores
study_hours <- c(5, 8, 10, 12, 15, 18, 20, 22, 25, 28)
exam_scores <- c(65, 72, 78, 82, 85, 88, 90, 92, 94, 95)

# Plot the relationship
plot(study_hours, exam_scores,
     xlab = "Study Hours per Week",
     ylab = "Exam Score",
     main = "Study Hours vs Exam Score",
     pch = 19, col = "darkblue", cex = 1.2)

# Add a trend line
abline(lm(exam_scores ~ study_hours), col = "red", lwd = 2)

# Correlation
cor(study_hours, exam_scores)

# ============================================================================
# 9. INTERESTING EXAMPLE: AUSTIN RESTAURANT RATINGS
# ============================================================================

# Load the Austin restaurants data
# Note: Adjust the path based on where you run this script from
# If running from the code/ directory, use: "../data/austin_restaurants.csv"
# If running from the StatisticalModeling/ directory, use: "data/austin_restaurants.csv"
restaurants <- read.csv("../data/austin_restaurants.csv")

# Explore the data
head(restaurants)
str(restaurants)
summary(restaurants)

# What's the average rating?
mean(restaurants$rating)

# Which cuisine type has the highest average rating?
aggregate(rating ~ cuisine, data = restaurants, FUN = mean)

# How many restaurants in each neighborhood?
table(restaurants$neighborhood)

# Plot: Rating distribution
hist(restaurants$rating,
     xlab = "Rating", ylab = "Number of Restaurants",
     main = "Distribution of Restaurant Ratings in Austin",
     col = "lightcoral",
     breaks = seq(3.5, 5.0, by = 0.1))

# Plot: Ratings by cuisine type
boxplot(rating ~ cuisine, data = restaurants,
        xlab = "Cuisine Type", ylab = "Rating",
        main = "Restaurant Ratings by Cuisine Type",
        las = 2,  # Rotate x-axis labels
        col = "lightblue")

# Find restaurants with rating >= 4.5
top_restaurants <- restaurants[restaurants$rating >= 4.5, ]
top_restaurants[, c("name", "cuisine", "rating", "price_range")]

# Relationship between number of reviews and rating?
plot(restaurants$reviews, restaurants$rating,
     xlab = "Number of Reviews", ylab = "Rating",
     main = "Reviews vs Rating",
     pch = 19, col = "darkgreen")
cor(restaurants$reviews, restaurants$rating)

# ============================================================================
# 10. WORKING WITH REAL DATA: LOADING CSV FILES
# ============================================================================

# Example: Load a CSV file
# data <- read.csv("path/to/your/file.csv")
# head(data)

# ============================================================================
# 11. USEFUL FUNCTIONS TO KNOW
# ============================================================================

# Sequences
1:10
seq(0, 1, by = 0.1)

# Repeating values
rep("Hello", 5)
rep(c("A", "B"), each = 3)

# Random numbers
rnorm(10)  # 10 random numbers from normal distribution
runif(10)  # 10 random numbers from uniform distribution

# Rounding
round(3.14159, 2)
ceiling(3.2)
floor(3.8)

# ============================================================================
# 12. CONDITIONAL LOGIC
# ============================================================================

# If-else statements
score <- 85
if (score >= 90) {
  grade <- "A"
} else if (score >= 80) {
  grade <- "B"
} else {
  grade <- "C or below"
}
grade

# ============================================================================
# 13. LOOPS (BUT VECTORIZED OPERATIONS ARE USUALLY BETTER!)
# ============================================================================

# For loop example
squares <- numeric(10)
for (i in 1:10) {
  squares[i] <- i^2
}
squares

# But this is better (vectorized):
(1:10)^2

# ============================================================================
# 14. FUNCTIONS
# ============================================================================

# Create your own function
convert_f_to_c <- function(fahrenheit) {
  celsius <- (fahrenheit - 32) * 5/9
  return(celsius)
}

convert_f_to_c(32)   # Freezing point
convert_f_to_c(212)  # Boiling point

# ============================================================================
# 15. PACKAGES (EXTENDING R'S CAPABILITIES)
# ============================================================================

# Install a package (only need to do once)
# install.packages("ggplot2")

# Load a package (need to do each session)
# library(ggplot2)

# ============================================================================
# TIPS FOR BEGINNERS
# ============================================================================

# 1. Use <- for assignment (not =)
# 2. R is case-sensitive: Age != age
# 3. Use # for comments
# 4. Get help: ?function_name or help(function_name)
# 5. RStudio is your friend - use it!
# 6. Google is your friend - R has great online resources
# 7. Practice with real data - it's more fun!

# Try these:
# ?mean
# ?plot
# ?data.frame

