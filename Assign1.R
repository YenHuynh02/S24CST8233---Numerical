library(dplyr)
library(polynom)

setwd("C:\\Users\\yenhu\\OneDrive\\Algonquin College\\Spring 2024\\CST8233 - Numerical Computing\\Assignment 1")

# Task 1
# Part 1
CerealsDF <- read.csv("assignment1.csv", header = TRUE, sep = ";")

str(CerealsDF)

# Display the first 10 rows
head(CerealsDF, 10)

# Part 2
# Delete the second row of data frame
CerealsDFNew <- CerealsDF[-1, ]

# Print the number of row and column
print(dim(CerealsDFNew))

# Convert the carbo and sugars columns to numeric, handling non-numeric values by coercing them to NA
CerealsDFNew$carbo <- as.numeric(CerealsDFNew$carbo)
CerealsDFNew$sugars <- as.numeric(CerealsDFNew$sugars)

# Add a new column named “totalcarbo” that shows the total of both carbo and sugars columns and remove N/A value
CerealsDFNew$totalcarbo <- rowSums(CerealsDFNew[, c("carbo", "sugars")], na.rm = TRUE)

# Part 3
# Find how many cereals are hot
hotC <- subset(CerealsDFNew, type == "H")
hotC

# Find how many unique manufacturers are mentioned in the data frame
uniqueM <- unique(CerealsDFNew$mfr)
uniqueM

# Extract all cereals that are manufactured by Kellogg’s (“K”)
cereals_K <- subset(CerealsDFNew, mfr == "K")
cereals_K

# Part 4
# Extract all cereals that have less than or equal 90 calories AND have more than 2 units of fat
subsetFC <- subset(CerealsDFNew, calories <= 90)
subsetFC <- subset(CerealsDFNew, fat > 2)
subsetFC

# Save this subset as a CSV file on your desk. Use write.csv() function
write.csv(subsetFC, "subsetFC.csv", row.names = FALSE)

# Task 2
# Assign data
x <- c(pi, 6.678, 3*pi, 12.961, 5*pi, 19.244, 7*pi)
y <- c(0, -1.921, 0, 3.584, 0, -6.718, 0)

# P(x)=∑ f(xi)*Li(x)
MyIntCal <- function(x, y, x_given) {
  n <- length(x)
  L <- rep(1, n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        L[i] <- L[i] * (x_given - x[j]) / (x[i] - x[j])
      }
    }
  }
  interPolation <- sum(y * L)
  return(interPolation)
}

# What is degree of the interpolating function?
degree <- length(x) - 1

# Plot each Lagrange polynomial and the final interpolating function
pdf("MyIntFig.pdf", width = 8, height = 8)
par(mfrow = c(4, 2))  # 4x2 layout

for (k in 1:length(x)) {
  Lk <- function(x_given) {
    L <- 1
    for (j in 1:length(x)) {
      if (j != k) {
        L <- L * (x_given - x[j]) / (x[k] - x[j])
      }
    }
    return(L)
  }
  
  x_seq <- seq(min(x), max(x), length.out = 100)
  Lk_vals <- sapply(x_seq, Lk)
  
  # "main" use for display the number of table like L 1 (x), etc.
  # "type" for specify the shape of line
  plot(x_seq, Lk_vals, type = "l", main = paste("L", k, "(x)"), xlab = "x", ylab = "L(x)")
}

# Close pdf file
dev.off()

# Use poly.calc to find the interpolating function
pf_x <- poly.calc(x, y)
pf_x

# Find f(15) and f(24)
f_15_MyIntCal <- MyIntCal(x, y, 15)
f_24_MyIntCal <- MyIntCal(x, y, 24)
f_15_MyIntCal
f_24_MyIntCal