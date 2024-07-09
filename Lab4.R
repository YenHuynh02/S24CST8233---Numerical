# Function of Maclaurin series
maclaurin_ln <- function(x, n_terms) {
  sum <- 0
  for (n in 1:n_terms) {
    term <- ((-1)^(n + 1) * x^n) / n
    sum <- sum + term
  }
  return(sum)
}

# Function for absolute and relative errors
compute_errors <- function(x, n_terms) {
  #log() is a natural logarithm(ln), log10() is a base 10 logarithm
  true_value <- log(1 + x)
  absolute <- numeric(n_terms)
  relative <- numeric(n_terms)
  
  for (n in 1:n_terms) {
    approx_value <- maclaurin_ln(x, n)
    # ABS = |TrueValue - EstimatedValue|
    absolute[n] <- abs(true_value - approx_value)
    
    # REL = ABS / TrueValue * 100
    relative[n] <- abs(true_value - approx_value) / abs(true_value) * 100
  }
  
  return(data.frame(n = 1:n_terms, Absolute_Error = absolute, Relative_Error = relative))
}

# Plotting the original function and the approximation
plot_function <- function() {
  x_vals <- seq(-0.5, 0.5, by = 0.01)
  y_true <- log(1 + x_vals)
  
  plot(x_vals, y_true, type = "l", ylab = "f(x)", xlab = "x", main = "f(6) for Maclaurin Series Approximation")
}

# Main script
main <- function() {
  x <- as.numeric(readline(prompt = "Enter the value of x: "))
  n_terms <- 10
  
  # Compute errors
  errors <- compute_errors(x, n_terms)
  print(errors)
  
  # Plot the functions
  plot_function()
}

# Run the main script
main()
