## Linear Regression

# generate bivariate normal data
library(MASS)
set.seed(1, sample.kind="Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

# use average of 100 combined models
set.seed(1, sample.kind="Rounding")
models <- replicate(n, {
  idx <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  test <- dat[idx,]
  train <- dat[-idx,]
  fit <- lm(y ~ x, data = train)
  y_hat <- predict(fit, test)
  sqrt(mean((y_hat-dat$y[idx])^2))
})
mean(models)
sd(models)

# using larger data set
B <- 100
modelFit <- function(n) {
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  models <- replicate(B, {
    idx <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    test <- dat[idx,]
    train <- dat[-idx,]
    fit <- lm(y ~ x, data = train)
    y_hat <- predict(fit, test)
    sqrt(mean((y_hat-dat$y[idx])^2))
  })
  c(mean(models), sd(models))
}

set.seed(1, sample.kind = "Rounding")
fits <- sapply(c(100, 500, 1000, 5000, 10000), modelFit)
fits

# what if the covariance is larger
set.seed(1, sample.kind = "Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind = "Rounding")
models <- replicate(n, {
  idx <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  test <- dat[idx,]
  train <- dat[-idx,]
  fit <- lm(y ~ x, data = train)
  y_hat <- predict(fit, test)
  sqrt(mean((y_hat-dat$y[idx])^2))
})
mean(models)
sd(models)

# using multiple predictors
set.seed(1, sample.kind = "Rounding")
n <- 100
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)

# training on different predictors
set.seed(1, sample.kind = "Rounding")
library(caret)
idx <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
test <- dat[idx,]
train <- dat[-idx,]

fit1 <- lm(y ~ x_1, data = train)
fit2 <- lm(y ~ x_2, data = train)
fit12 <- lm(y ~ ., data = train)

y_hat1 <- predict(fit1, test)
y_hat2 <- predict(fit2, test)
y_hat12 <- predict(fit12, test)

sqrt(mean((y_hat1 - test$y)^2))
sqrt(mean((y_hat2 - test$y)^2))
sqrt(mean((y_hat12 - test$y)^2))

# what if the predictors are highly correlated
set.seed(1, sample.kind = "Rounding")
n <- 100
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)

set.seed(1, sample.kind = "Rounding")
library(caret)
idx <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
test <- dat[idx,]
train <- dat[-idx,]

fit1 <- lm(y ~ x_1, data = train)
fit2 <- lm(y ~ x_2, data = train)
fit12 <- lm(y ~ ., data = train)

y_hat1 <- predict(fit1, test)
y_hat2 <- predict(fit2, test)
y_hat12 <- predict(fit12, test)

sqrt(mean((y_hat1 - test$y)^2))
sqrt(mean((y_hat2 - test$y)^2))
sqrt(mean((y_hat12 - test$y)^2))
# correlation of predictors decreases influence of having multiple predictors

# Logistic regression
# make data set with binomial outcomes
set.seed(2, sample.kind="Rounding")
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

# continuous x predictive of binary y
dat$train %>% ggplot(aes(x, color = y)) + geom_density()

# generate datasets with different average of y = 1 predictors
set.seed(1, sample.kind="Rounding")
mu_1s <- seq(0, 3, len=25)
res <- sapply(mu_1s, function(mu_1) {
  dat <- make_data(mu_1 = mu_1)
  glm_fit <- glm(y ~ x, data = dat$train, family = "binomial")
  p_hat <- predict(glm_fit, newdata = dat$test, type = "response")
  y_hat <- as.factor(ifelse(p_hat > 0.5, 1, 0))
  confusionMatrix(y_hat, as.factor(dat$test$y))$overall[["Accuracy"]]
})

df <- data.frame(mu_1s, res)
ggplot(df, aes(mu_1s, res)) + geom_point()

## Smoothing
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

# extract data from Puerto Rico 2015-2018
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

# fit local weighted regression prediction of deaths based on date
r <- diff(range(as.numeric(dat$date)))
span <- 60/r
est <- loess(deaths ~ as.numeric(date), degree = 1, span = span, data = dat)
dat %>%
  ggplot(aes(date, deaths)) + 
  geom_point(size = 3) + 
  geom_smooth(size = 2,color='red', span = span, method = "loess", method.args=list(degree=1))

# plot estimate as a function of day of the year
dat %>%
  mutate(smooth = predict(est, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd=2)

# MNIST data set
library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>%
  tidy()
# x_2 coefficient is not significant
qplot(x_2, y, data = mnist_27$train)
# binary plot does not reveal much

est <- loess(as.numeric(y) ~ x_2, degree = 1, data = mnist_27$train)
predictions <- predict(est, mnist_27$test)
ggplot(data = mnist_27$test, aes(x_2, predictions, col = y)) +
  geom_point(size = 3)

# lower prediction values are more likely to be 2

## Working with Matrices
library(dslabs)
mnist <- read_mnist()
m <- mnist$train$images
grey <- mean(m < 205 & m > 50)