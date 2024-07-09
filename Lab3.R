#Part 1
require('PolynomF')

#p <- x^3 - 3 * x^2 - 2 * x + 7
p <- polynom(c(7, -2, -3, 1))

#What is the class of the variable p? "Polynom"
class(p)

#Find these coefficients
c <- coef(p)
c

# Define independent y, q and its class
q <- polynom(c(0, 2, 1)) #q <- y^2 + 2 * y

class(q)
class(y)

# Sum of addition, subtraction, and multiplication
addition <- p + q
substraction <- p - q
multiplication <- p * q
addition
substraction
multiplication

# Find the derivatives of p and q
dP <- deriv(p)
dQ <- deriv(q)
dP
dQ

# Plot p and dpdx
curve(p(x), from = -2, to = 3, col = "blue", ylab = "p(x), dpdx")
curve(dP(x), from = -2, to = 3, col = "red", add = TRUE)

# Add a horizontal line, slope = 0
abline(h = 0, col = "orange")

# Part 2
require('dplyr')
require(datasets)

my_df <- datasets::airquality

# Display the structure of my_df, print the first 6 lines, and name of columns
str(my_df)
head(my_df)
names(my_df)

# Display temperature column
my_df_temp <- select(my_df, "Temp")
my_df_temp

# Mean, median, standard deviation
months <- c(6, 7, 8)

mean <- numeric(length(months))
median <- numeric(length(months))
stdDev <- numeric(length(months))

for (i in sequence(months)) {
  temp <- filter(my_df, Month == months[i])$Temp
  
  # Remove N/A value
  mean[i] <- mean(temp, na.rm = TRUE)
  median[i] <- median(temp, na.rm = TRUE)
  stdDev[i] <- sd(temp, na.rm = TRUE)
}

# Print the results by using seq_along (display the selected month only)
for (i in seq_along(months)) {
  cat("Month:", months[i], "\n")
  cat("Mean Temperature:", mean[i], "\n")
  cat("Median Temperature:", median[i], "\n")
  cat("Standard Deviation:", stdDev[i], "\n\n")
}

# Find the probability of temperature
all_temp <- my_df$Temp
mean_all <- mean(all_temp, na.rm = TRUE)
sd_all <- sd(all_temp, na.rm = TRUE)

# Probability that temperature is less than 70
p_less_70 <- pnorm(70, mean = mean_all, sd = sd_all)

# Probability that temperature is greater than 85
p_greater_85 <- 1 - pnorm(85, mean = mean_all, sd = sd_all)

# Probability that temperature is between 75 and 90
p_between_75_90 <- pnorm(90, mean = mean_all, sd = sd_all) - pnorm(75, mean = mean_all, sd = sd_all)

p_less_70
p_greater_85
p_between_75_90