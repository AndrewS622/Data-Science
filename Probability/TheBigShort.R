## Simulating Bank Earnings
# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a vector called `defaults` that contains the default outcomes of `n` loans
defaults <- sample(c(0, 1), n, replace = TRUE, prob = c(1-p_default, p_default))

# Generate `S`, the total amount of money lost across all foreclosures. Print the value to the console.
S <- loss_per_foreclosure*sum(defaults)
S

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# The variable `B` specifies the number of times we want the simulation to run
B <- 10000

# Generate a list of summed losses 'S'. Replicate the code from the previous exercise over 'B' iterations to generate a list of summed losses for 'n' loans.  Ignore any warnings for now.
S <- replicate(B, {
  loss_per_foreclosure*sum(sample(c(0,1), n, replace = TRUE, prob = c(1-p_default, p_default)))
})

# Plot a histogram of 'S'.  Ignore any warnings for now.
hist(S)

# Calculate the expected loss due to default out of 10,000 loans
loss_per_foreclosure*p_default*n

# Compute the standard error of the sum of 10,000 loans
abs(0-loss_per_foreclosure)*sqrt(n*p_default*(1-p_default))

# Assign a variable `x` as the total amount necessary to have an expected outcome of $0
x <- (-loss_per_foreclosure*p_default)/(1-p_default)

# Convert `x` to a rate, given that the loan amount is $180,000. Print this value to the console.
x/180000

# Generate a variable `z` using the `qnorm` function
z <- qnorm(0.05)

# Generate a variable `x` using `z`, `p_default`, `loss_per_foreclosure`, and `n`
x <- -loss_per_foreclosure*(n*p_default-z*sqrt(n*p_default*(1-p_default)))/(n*(1-p_default) + z*sqrt(n*p_default*(1-p_default)))

# Convert `x` to an interest rate, given that the loan amount is $180,000. Print this value to the console.
x/180000
