library(tidyverse)
# The 'errors' data have already been loaded. Examine them using the `head` function.
head(errors)

# Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-
totals <- errors %>% filter(grade %in% c("A-", "C-")) %>% group_by(grade, hit) %>% summarize(n = n()) %>% spread(grade, n)

# Print the proportion of hits for grade A- polls to the console
totals[["A-"]][2]/sum(totals[["A-"]])

# Print the proportion of hits for grade C- polls to the console
totals[["C-"]][2]/sum(totals[["C-"]])

# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.
chisq_test <- chisq.test(totals[,2:3])

# Print the p-value of the chi-squared test to the console
chisq_test$p.value

# Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls
odds_C <- totals[["C-"]][2]/totals[["C-"]][1]

# Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls
odds_A <- totals[["A-"]][2]/totals[["A-"]][1]

# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
odds_A/odds_C