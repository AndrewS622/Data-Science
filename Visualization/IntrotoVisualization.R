## Data types
# variable names, types, and basic properties
library(dslabs)
data(heights)
names(heights)

class(heights$sex)

x <- heights$height
length(unique(x))

tab <- table(x)

sum(tab == 1)

## Vectors, quantiles, boxplots, and z-scores
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
length(male)
length(female)

male_percentiles <- quantile(male, seq(0.10,0.90,0.20))
female_percentiles <- quantile(female, seq(0.10,0.90,0.20))
df <- data.frame("female" = female_percentiles, "male"= male_percentiles)

z <- scale(x)

mean(abs(z) < 2)

p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height, p)

percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]

theoretical_quantiles <- qnorm(p, 69, 3)

## The normal distribution
x <- heights$height[heights$sex == "Male"]
mean(x > 69 & x <= 72)

avg <- mean(x)
stdev <- sd(x)
pnorm((72-avg)/stdev)-pnorm((69-avg)/stdev)

exact <- mean(x > 79 & x <= 81)
approx <- pnorm((81-mean(x))/sd(x)) - pnorm((79-mean(x))/sd(x))
exact/approx

# use pnorm to calculate the proportion over 7 feet (7*12 inches)
1 - pnorm((7*12-69)/3)

p <- 1 - pnorm((7*12 - 69)/3)
round(10^9*p)

p <- 1 - pnorm((7*12 - 69)/3)
N <- round(p*10^9)
10/N

# Change the solution to previous answer
p <- 1 - pnorm(6*12+8, 69, 3)
N <- round(p * 10^9)
150/N

## Robust summaries: analyzing height
library(HistData)
data(Galton)
x <- Galton$child
mean(x)
median(x)

sd(x)
mad(x)

x_with_error <- x
x_with_error[1] <- x_with_error[1]*10
mean(x_with_error) - mean(x)

sd(x_with_error) - sd(x)

median(x_with_error) - median(x)

mad(x_with_error) - mad(x)

x <- Galton$child
error_avg <- function(k){
  x[1] <- k
  mean(x)
}
error_avg(10000)
error_avg(-10000)
