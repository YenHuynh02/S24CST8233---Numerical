#1 
#Declare function
func1 <- function(x) {
 return(0.1 * exp(x) * cos(x) + 2 * log(abs(x))); 
}

#Vector that store value of a function and range from 3, 3.1, ..., 6
cVec <- func1(c(seq(3, 6, by = 0.1)))
sum <- sum(cVec)
cat("The sum of this vector is:", sum)

#Plot the vector
plot(cVec, main = "My First Plot")

#2
i <- 1:25
sum2 <- sum(((2^i) / i) + ((3^i) / i^2))
cat("The sum of this summation is:", sum2)

#3
set.seed(75)
Vec1 <- sample(0:999, 100, replace = TRUE)
Vec2 <- sample(0:999, 100, replace = TRUE)

#a Extract the values in Vec2 which are greater than 600.
Vec2a <- Vec2[Vec2 > 600]
Vec2a

#b Index positions in Vec2 of these values
Vec2b <- which(Vec2 > 600)
Vec2b

#c Vec1 which correspond to the values in Vec2 which are greater than 600
Vec1c <- Vec1[Vec2b]
Vec1c

#d Numbers in Vec1 are divisible by 2. (None)
DivisibleBy2 <- (Vec1 %% 2) == 0
sum3 <- sum(DivisibleBy2)
sum3

#4
func2 <- function(x) {
  if (x < 0) {
    return(x^2 + 2 * x +3)
  }
  
  else if (x >= 0 && x < 2) {
    return(x + 3)
  }
  
  else {
    return(x^2 + 4 * x - 7)
  }
}

myFun <- function(Vec1) {
  #Apply a function over a Vector by using sapply()
  sapply(Vec1, func2)
}

plot(myFun(seq(-4, 3, by = 1)))