## Basics of Evaluating ML Algorithms
library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

# filter and add type column to dataset
dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

# extract sex and class type
y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# percentages of females in each type
sum(x == "inclass" & y == "Female")/sum(x == "inclass")
sum(x == "online" & y == "Female")/sum(x == "online")

# use type as predictor for sex assuming all of one class is one sex
library(caret)
y_hat <- ifelse(x == "inclass", "Female", "Male") %>%
  factor(levels = levels(y))

# checking results
table(y_hat, y)
sensitivity(y_hat, y)
specificity(y_hat, y)

# confusion matrix
confusionMatrix(data = y_hat, reference = y)

## Iris data set, removing setosa species
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# split data into train and test sets
set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) 
test <- iris[test_index,]
train <- iris[-test_index,]

# train ML algorithms based on each available feature
# Sepal.Length
x <- train$Sepal.Length
r <- range(x)
cutoff <- seq(r[1], r[2], 0.1)
accuracy <- sapply(cutoff, function(c) {
  y_hat <- ifelse(x > c, "virginica", "versicolor")
  mean(train$Species == y_hat)
})
max(accuracy)

# Sepal.Width
x <- train$Sepal.Width
r <- range(x)
cutoff <- seq(r[1], r[2], 0.1)
accuracy <- sapply(cutoff, function(c) {
  y_hat <- ifelse(x > c, "virginica", "versicolor")
  mean(train$Species == y_hat)
})
max(accuracy)

# Petal.Length
x <- train$Petal.Length
r <- range(x)
cutoff <- seq(r[1], r[2], 0.1)
accuracy <- sapply(cutoff, function(c) {
  y_hat <- ifelse(x > c, "virginica", "versicolor")
  mean(train$Species == y_hat)
})
max(accuracy)

# Petal.Width
x <- train$Petal.Width
r <- range(x)
cutoff <- seq(r[1], r[2], 0.1)
accuracy <- sapply(cutoff, function(c) {
  y_hat <- ifelse(x > c, "virginica", "versicolor")
  mean(train$Species == y_hat)
})
max(accuracy)

# using best algorithm (Petal.Length), evaluate test set
x_best <- train$Petal.Length
r <- range(x_best)
cutoff <- seq(r[1], r[2], 0.1)
accuracy <- sapply(cutoff, function(c) {
  y_hat <- ifelse(x_best > c, "virginica", "versicolor")
  mean(train == y_hat)
})
cutoff <- cutoff[which.max(accuracy)]
predictions <- ifelse(test$Petal.Length > cutoff, "virginica", "versicolor")
mean(test$Species == predictions)

# what happens if we train on the test set instead?
# Sepal.Length
x <- test$Sepal.Length
r <- range(x)
cutoff <- seq(r[1], r[2], 0.1)
accuracy <- sapply(cutoff, function(c) {
  y_hat <- ifelse(x > c, "virginica", "versicolor")
  mean(train$Species == y_hat)
})
max(accuracy)

# Sepal.Width
x <- test$Sepal.Width
r <- range(x)
cutoff <- seq(r[1], r[2], 0.1)
accuracy <- sapply(cutoff, function(c) {
  y_hat <- ifelse(x > c, "virginica", "versicolor")
  mean(train$Species == y_hat)
})
max(accuracy)

# Petal.Length
x <- test$Petal.Length
r <- range(x)
cutoff <- seq(r[1], r[2], 0.1)
accuracy <- sapply(cutoff, function(c) {
  y_hat <- ifelse(x > c, "virginica", "versicolor")
  mean(train$Species == y_hat)
})
max(accuracy)

# Petal.Width
x <- test$Petal.Width
r <- range(x)
cutoff <- seq(r[1], r[2], 0.1)
accuracy <- sapply(cutoff, function(c) {
  y_hat <- ifelse(x > c, "virginica", "versicolor")
  mean(train$Species == y_hat)
})
max(accuracy)
# now Petal.Width shows best feature

# some exploratory data analysis
plot(iris,pch=21,bg=iris$Species)
# combination of petal length and width seems to be more applicable

# using cutoffs for petal length and width, use combination cutoff
# Petal.Length
x <- train$Petal.Length
r <- range(x)
cutoff <- seq(r[1], r[2], 0.1)
accuracy <- sapply(cutoff, function(c) {
  y_hat <- ifelse(x > c, "virginica", "versicolor")
  mean(train$Species == y_hat)
})
c1 <- cutoff[which.max(accuracy)]

# Petal.Width
x <- train$Petal.Width
r <- range(x)
cutoff <- seq(r[1], r[2], 0.1)
accuracy <- sapply(cutoff, function(c) {
  y_hat <- ifelse(x > c, "virginica", "versicolor")
  mean(train$Species == y_hat)
})
c2 <- cutoff[which.max(accuracy)]

# now test
x <- cbind(test$Petal.Length, test$Petal.Width)
y_hat <- ifelse(x[,1] > c1 | x[,2] > c2, "virginica", "versicolor")
mean(y_hat == test$Species)

## Conditional Probabilities
# Bayes Theorem practice
# probability of having a disease given a positive test result
pposdis <- 0.85
pnegnodis <- 0.9
pposnodis <- 1 - pnegnodis
pdis <- 0.02
ppos <- pposdis*pdis + pposnodis*(1-pdis)
pdispos <- pposdis*pdis/ppos
pdispos

# confirm via simulation 
N <- 1000000
set.seed(1)
disease <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-pdis, pdis))
test <- rep(NA, N)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(pnegnodis, 1-pnegnodis))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(1-pposdis, pposdis))

# probability of a positive test
mean(test==1)

# probability of having disease given negative test
sum(disease == 1 & test == 0)/sum(test == 0)

# probability of having disease if test is positive
sum(disease == 1 & test == 1)/sum(test == 1)

# prevalence of disease in people who test positive to overall
pdisgivenpositive <- sum(disease == 1 & test == 1)/sum(test == 1)
ppos <- mean(disease == 1)
pdisgivenpositive/ppos

# heights dataset
library(dslabs)
data("heights")

# conditional probabilities for sex given height
heights %>%
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male"))

# use quantiles and cut to prevent high variability when low numbers of samples present
ps <- seq(0, 1, 0.1)
heights %>%
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data=.)

# generate data from bivariate normal distribution
# Sigma is covariance matrix, c(69,69) gives means
library(MASS)
Sigma <- 9*matrix(c(1,0.5,0.5,1),2,2)
dat <- MASS::mvrnorm(n = 10000, c(69,69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

# estimate conditional probabilities and make plot
ps <- seq(0,1,0.1)
dat %>%
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data = .)
