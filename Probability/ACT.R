# Analyzing ACT scores
set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000, 20.9, 5.7)

# parameters of the distribution
mean(act_scores)
sd(act_scores)
sum(act_scores >= 36)
mean(act_scores > 30)
mean(act_scores <= 10)

# plotting the distribution
x <- 1:36
f_x <- sapply(x, dnorm, mean = 20.9, sd = 5.7)
plot(x,f_x)

# using z-scored data
Zact_scores <- scale(act_scores)
mean(Zact_scores > 2)
2*sd(act_scores) + mean(act_scores)
qnorm(0.975, mean(act_scores), sd(act_scores))

# finding empirical quantiles
quantiles <- function (q) {
  mean(act_scores <= q)
}

qs <- 1:36
quantVals <- sapply(qs, quantiles)
quantVals

# expected 95th percentile
qnorm(0.95, 20.9, 5.7)

# finding percentiles with the quantile function
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)

# which percentile does 26 fall in
sample_quantiles[which(sample_quantiles > 26)[1] - 1]
sample_quantiles["82%"]

# theoretical quantiles form normal distribution
theoretical_quantiles <- sapply(p, qnorm, mean = 20.9, sd = 5.7)
qqplot(theoretical_quantiles, sample_quantiles)
