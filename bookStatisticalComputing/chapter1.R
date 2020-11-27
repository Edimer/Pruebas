# In this script i go to write the examples from book:
# Statistical Computing with R (second edition)
# Chapter 1

# Formula
formula0 <- lm(rock$peri ~ 0) # null model
formula1 <- lm(rock$peri ~ 1) # only intercept
formula2 <- lm(rock$peri ~ rock$area) # both parameters
formula3 <- lm(rock$peri ~ 0 + rock$area) # without intercept
formula4 <- lm(rock$peri ~ 1 + rock$area) # both parameters
summary(formula0)
summary(formula1)
summary(formula2)
summary(formula3)
summary(formula4)

# Plot empirical CDF (cumulative distribution function)
x11(); plot.ecdf(iris$Sepal.Length)
x11(); plot.ecdf(iris$Petal.Length)
stem(iris$Sepal.Length)

# Exercises

## 1.1
set.seed(123)
exerc1.1 <- rt(n = 100, df = 4)

## 1.2
MASS::truehist(exerc1.1, col = "gray")
curve(dt(x, df = 4), add = TRUE, col = "red")

## 1.3
MASS::truehist(exerc1.1, col = "gray")
lines(density(exerc1.1), col = "blue")

## 1.4 (a)
f <- function(x, a, b){
  fx = (x - a) / b
  return(fx)
}

f(x = 10, a = 5, b = 2)

## 1.4 (b)
f2 <- function(x){
  fx = (x - min(x)) / (max(x) - min(x))
  return(fx)
}

random_x <- rnorm(n = 10, mean = 2, sd = 2)
f2(x = random_x)

## 1.5
f3 <- function(x){
  fx = (x - mean(x)) / (sd(x))
  return(fx)
}

sample_studentize <- f3(x = random_x)
mean(sample_studentize)
sd(sample_studentize)

## 1.6
median_x <- median(sample_studentize)
iqr_x <- IQR(sample_studentize)
median_x / iqr_x
