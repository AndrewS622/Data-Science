## Analyzing SAT Data
# points for correct or incorrect answer
# probability of guessing correctly out of 5 choices
points <- c(1,-0.25)
probs <- c(0.2, 0.8)
n <- 44

# expected values
mu1 <- sum(points*probs)
mu_all <- n*mu1

# standard error of sum
se_all <- abs(diff(points))*sqrt(prod(probs))*sqrt(n)

# probability of getting at least 8 points
1 - pnorm(8, mu_all, se_all)
          
# MC simulation
set.seed(21, sample.kind = "Rounding")
B <- 10000

res <- replicate(B, {
  Xs <- sample(points, n, replace = TRUE, prob = probs)
  sum(Xs)
})
mean(res>=8)

# no penalty for wrong answers, only 4 choices
points <- c(1,0)
probs <- c(0.25, 0.75)

mu_all <- sum(points*probs)*n

# what is the lowest p to get at least 80%
p <- seq(0.25, 0.95, 0.05)
grades <- sapply(p, function(i) {
  mui <- n*i
  sei <- sqrt(i*(1-i)*n)
  c(i, mui, sei, 1 - pnorm(35, mui, sei))
})
grades
grades[1,which(grades[4,] >= 0.80)[1]]

## continued in Roulette.R