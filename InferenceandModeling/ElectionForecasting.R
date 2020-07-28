# Load the libraries and data
library(dplyr)
library(dslabs)
library(ggplot2)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Create an object called `cis` that has the columns indicated in the instructions
cis <- polls %>% 
  mutate(X_hat = (spread+1)/2, se = 2*sqrt(X_hat*(1-X_hat)/samplesize), lower = spread - se*qnorm(1-0.05/2), upper = spread + se*qnorm(1-0.05/2)) %>% 
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of confidence intervals that contain the actual value. Print this object to the console.
p_hits <- ci_data %>% 
  mutate(hit = (actual_spread >= lower & actual_spread <= upper)) %>% 
  summarize(mean(hit))

# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has at least 5 polls.
p_hits <- ci_data %>% 
  group_by(pollster) %>% 
  filter(n() >= 5) %>% mutate(hit = (actual_spread >= lower & actual_spread <= upper)) %>% 
  summarize(proportion_hits = mean(hit), n = n(), grade = grade[1]) %>% 
  arrange(-proportion_hits)

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls.
p_hits <- ci_data %>% 
  mutate(hit = (actual_spread >= lower & actual_spread <= upper)) %>%
  group_by(state) %>% 
  filter(n() > 5) %>% 
  summarize(proportion_hits = mean(hit), n = n())

# Make a barplot of the proportion of hits for each state
p_hits %>% ggplot(aes(state, proportion_hits)) + 
  geom_bar(stat = "identity") + 
  coord_flip()

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- ci_data %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))

# Examine the last 6 rows of `errors`
tail(errors)

# Make a barplot of the proportion of hits for each state
p_hits %>%
  arrange(desc(proportion_hits)) %>% 
  ggplot(aes(x = reorder(state,proportion_hits), y = proportion_hits)) + 
  geom_bar(stat = "identity") + 
  coord_flip()

# Generate a histogram of the error
hist(errors$error)

# Calculate the median of the errors. Print this value to the console.
median(errors$error)

# Create a boxplot showing the errors by state for polls with grades B+ or higher
errors %>% filter(grade %in% c("A+", "A", "A-", "B+")) %>% 
  ungroup() %>% 
  arrange(error) %>% 
  ggplot(aes(reorder(state, error), error)) + 
  geom_boxplot() + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Create a boxplot showing the errors by state for states with at least 5 polls with grades B+ or higher
e <- errors %>% ungroup() %>% filter(grade %in% c("A+", "A", "A-", "B+")) %>% group_by(state) %>% filter(n() >= 5) %>% arrange(error)

e%>% ggplot(aes(reorder(state, error),error)) + 
  geom_boxplot() + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## The t Distribution
# Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when 'df = 3'.
2*(1 - pt(2, 3))

# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df <- 3:50

# Make a function called 'pt_func' that calculates the probability that a value is more than |2| for any degrees of freedom 
pt_func <- function(d) {
  2*(1-pt(2, d))
}

# Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities
probs <- sapply(df, pt_func)

# Plot 'df' on the x-axis and 'probs' on the y-axis
plot(df, probs)

# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations
res <- replicate(B, {
  X <- sample(x, N, replace = TRUE)
  interval <- c(-1,1)*sd(X)*qnorm(1-0.05/2)/sqrt(N) + mean(X)
  between(mu, interval[1], interval[2])
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)

# The vector of filtered heights 'x' has already been loaded for you. Calculate the mean.
mu <- mean(x)

# Use the same sampling parameters as in the previous exercise.
set.seed(1)
N <- 15
B <- 10000

# Generate a logical vector 'res' that contains the results of the simulations using the t-distribution
res <- replicate(B, {
  X <- sample(x, N, replace = TRUE)
  interval <- c(-1,1)*sd(X)*qt(1-0.05/2, N-1)/sqrt(N) + mean(X)
  between(mu, interval[1], interval[2])
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)