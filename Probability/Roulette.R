## Roulette
# bet on five pockets out of 38 pays 6:1
money <- c(6, -1)
probs <- c(5/38, 33/38)
n <- 500

# expected payout and error for one bet
mu1 <- sum(probs*money)
se1 <- abs(diff(money))*sqrt(prod(probs))

# expected payout for sum of 500 bets, error of sum and average
mu_all <- n*mu1
se_avg <- abs(diff(money))*sqrt(prod(probs))/sqrt(n)
se_all <- abs(diff(money))*sqrt(prod(probs))*sqrt(n)

# probability of losing money
pnorm(0, mu_all, se_all)
