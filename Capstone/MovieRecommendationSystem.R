##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

options(tidyverse.quiet = TRUE)
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)
library(recosystem)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# which genres are unique?
genre <- edx$genres %>%
  unique() %>%
  str_split("\\|") %>%
  unlist() %>%
  unique() %>% 
  sort()
genre_validation <- validation$genres %>%
  unique() %>%
  str_split("\\|") %>%
  unlist() %>%
  unique() %>%
  sort()

# confirm same sets of genres present in training and validation sets
identical(genre, genre_validation)

# make new columns for all genres
for (i in 1:length(genre)) {
  edxmov <- edx$movieId %in% edx$movieId[grep(genre[i], edx$genres)]
  valmov <- validation$movieId %in% validation$movieId[grep(genre[i], validation$genres)]
  edx <- edx %>%
    mutate(!!genre[i] := edxmov)
  validation <- validation %>%
    mutate(!!genre[i] := valmov) 
  print(i)
}

head(edx$genres[edx$Thriller])
head(edx$genres[edx$Comedy])
head(edx$genres[edx$Romance])

# extract release year by parsing title string
edx <- edx %>% 
  mutate(year = str_replace_all(str_extract(title, "\\(\\d{4}\\)$"), "\\(|\\)", "")) %>%
  mutate(year = as.numeric(year))

validation <- validation %>% 
  mutate(year = str_replace_all(str_extract(title, "\\(\\d{4}\\)$"), "\\(|\\)", "")) %>%
  mutate(year = as.numeric(year))

## Exploratory Data Analysis

library(tidyverse)

# dimensions
paste("Rows:", nrow(edx),", Columns:", ncol(edx))

# ratings
paste("Number of zeros:", sum(edx$rating == 0.0), ", Number of threes:", sum(edx$rating == 3.0))
hist(edx$rating)

# numbers of distinct features
paste("Number of unique movies:", length(unique(edx$movieId)), ", Number of unique users:", length(unique(edx$userId)))

# ratings per genre
paste("Drama:", length(grep("Drama", edx$genres)), "Comedy:", length(grep("Comedy", edx$genres)),
      "Thriller:", length(grep("Thriller", edx$genres)), "Romance:", length(grep("Romance", edx$genres)))

# ratings per movie
edx %>%
  group_by(movieId) %>%
  summarize(title = title, n = n()) %>%
  distinct() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head()

# most common ratings
edx %>%
  group_by(rating) %>%
  summarize(n = n()) %>%
  distinct() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head()

# half-star vs. whole-star ratings
edx %>%
  mutate(star = rating %% 1) %>% 
  group_by(star) %>%
  summarize(n = n())

## Machine Learning Models

# correlations and plot for date effect
cor(edx$timestamp, edx$rating)
edx %>% mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
	group_by(date) %>%
	summarize(rating = mean(rating)) %>%
	ggplot(aes(date, rating)) +
	geom_point() +
	geom_smooth()

# correlations and plot for release year effect
idx <- which(!is.na(edx$year))
cor(edx$year[idx], edx$rating[idx])
edx %>%
	group_by(year) %>%
	summarize(rating = mean(rating)) %>%
	ggplot(aes(year, rating)) +
	geom_point() +
	geom_smooth()

## Linear Regression

# estimate movie and user parameters
mu <- mean(edx$rating)
movie_avg <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
user_avg <- edx %>%
  left_join(movie_avg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# calculate RMSE for training set
predictions <- edx %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
# predictions <- round(predictions * 2)/2
sqrt(mean((predictions-edx$rating)^2))

# add in genre effects
b_g = numeric(length(genre))
for (i in 1:length(genre)) {
  idx <- which(edx[[genre[i]]])
  movs <- edx[idx,]
  genre_effect <- movs %>%
    select(Action:Western)
  genre_effect <- as.matrix(genre_effect) %*% b_g
  resid <- movs %>%
    left_join(movie_avg, by='movieId') %>%
    left_join(user_avg, by='userId') %>%
    mutate(resid = rating - mu - b_i - b_u) %>%
    pull(resid)
  resid <- resid - genre_effect
  b_g[i] = mean(resid)
}

# compute RMSE
predictions <- edx %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
genre_effect <- edx %>%
  select(Action:Western)
genre_effect <- as.matrix(genre_effect) %*% b_g
# predictions <- round(2*(predictions + genre_effect))/2
predictions <- predictions + genre_effect
sqrt(mean((predictions-edx$rating)^2))

# compute date effect
genre_effect <- edx %>%
  select(Action:Western)
genre_effect <- as.matrix(genre_effect) %*% b_g

date_effect <- edx %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  mutate(date = round_date(as_datetime(timestamp), "week"), genre_effect = genre_effect, resid = rating - genre_effect - mu - b_i - b_u) %>%
  group_by(date) %>%
  summarize(d = mean(resid))

# compute RMSE
predictions <- edx %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  mutate(date = round_date(as_datetime(timestamp), "week")) %>%
  left_join(date_effect, by = "date") %>%
  mutate(pred = mu + b_i + b_u + d) %>%
  pull(pred)
genre_effect <- edx %>%
  select(Action:Western)
genre_effect <- as.matrix(genre_effect) %*% b_g
# predictions <- round(2 * (predictions + genre_effect))/2
predictions <- predictions + genre_effect
sqrt(mean((predictions-edx$rating)^2))

# compute year effect
year_effect <- edx %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  mutate(date = round_date(as_datetime(timestamp), "week")) %>%
  left_join(date_effect, by = "date") %>%
  mutate(genre_effect = genre_effect, resid = rating - genre_effect - mu - b_i - b_u - d) %>%
  group_by(year) %>%
  summarize(y = mean(resid))

# compute RMSE
predictions <- edx %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  mutate(date = round_date(as_datetime(timestamp), "week")) %>%
  left_join(date_effect, by = "date") %>%
  left_join(year_effect, by = "year") %>%
  mutate(pred = mu + b_i + b_u + d + y) %>%
  pull(pred)

# predictions <- round(2 * (predictions + genre_effect))/2
predictions <- predictions + genre_effect
sqrt(mean((predictions - edx$rating)^2))

### Regularization

# function that re-estimates all parameters for a given lambda
# lambda is added to the length of the vector in the denominator during estimation

est_RMSE <- function(lambda) {
  paste("Evaluating lambda =", lambda)
  movie_avg_r <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + lambda))
  user_avg_r <- edx %>%
    left_join(movie_avg_r, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n() + lambda))
  b_g_r = numeric(length(genre))
  for (i in 1:length(genre)) {
    idx <- which(edx[[genre[i]]])
    movs <- edx[idx,]
    genre_effect <- movs %>%
      select(Action:Western)
    genre_effect <- as.matrix(genre_effect) %*% b_g_r
    resid <- movs %>%
      left_join(movie_avg_r, by='movieId') %>%
      left_join(user_avg_r, by='userId') %>%
      mutate(resid = rating - mu - b_i - b_u) %>%
      pull(resid)
    resid <- resid - genre_effect
    b_g_r[i] = sum(resid)/(length(resid) + lambda)
  }
  genre_effect <- edx %>%
    select(Action:Western)
  genre_effect <- as.matrix(genre_effect) %*% b_g_r
  date_effect_r <- edx %>%
    left_join(movie_avg_r, by='movieId') %>%
    left_join(user_avg_r, by='userId') %>%
    mutate(date = round_date(as_datetime(timestamp), "week"), genre_effect = genre_effect, resid = rating - genre_effect - mu - b_i - b_u) %>%
    group_by(date) %>%
    summarize(d = sum(resid)/(n() + lambda))
  year_effect_r <- edx %>%
    left_join(movie_avg_r, by='movieId') %>%
    left_join(user_avg_r, by='userId') %>%
    mutate(date = round_date(as_datetime(timestamp), "week")) %>%
    left_join(date_effect_r, by = "date") %>%
    mutate(genre_effect = genre_effect, resid = rating - genre_effect - mu - b_i - b_u - d) %>%
    group_by(year) %>%
    summarize(y = sum(resid)/(n() + lambda))
  
  predictions <- edx %>%
    left_join(movie_avg_r, by='movieId') %>%
    left_join(user_avg_r, by='userId') %>%
    mutate(date = round_date(as_datetime(timestamp), "week")) %>%
    left_join(date_effect_r, by = "date") %>%
    left_join(year_effect_r, by = "year") %>%
    mutate(pred = mu + b_i + b_u + d + y) %>%
    pull(pred)
  
  # predictions <- round(2 * (predictions + genre_effect))/2
  predictions <- predictions + genre_effect
  sqrt(mean((predictions - edx$rating)^2))
}

# plot errors vs. lambdas and extract best estimate
lambdas <- seq(0, 5, 0.25)
RMSEs <- sapply(lambdas, est_RMSE)
df <- data.frame(lambda = lambdas, RMSE = RMSEs)
ggplot(data = df, aes(lambda, RMSE)) + geom_point()

lambda_best <- lambdas[which.min(RMSEs)]

### Other Models

# set control parameters for cross-validation
# Control <- trainControl(method = "cv", number = 5, p = .9, returnData = FALSE, trim = TRUE)
# use range of complexity parameters (cp)
# cp <- seq(0, 0.05, 0.005)
# fit_rpart <- train(rating ~ movieId + userId, data = edx, method = "rpart", tuneGrid = data.frame(cp = cp), trControl = Control)
# 
# plot results, extract best cp, view confusion matrix, plot final results
# plot(fit_rpart)
# cp[which.max(fit_rpart$results$Accuracy)]
# predictions_rpart <- predict(fit_rpart, validation)
# confusionMatrix(predictions_rpart, validation$rating)$overall[["Accuracy"]]
# plot(fit_rpart$finalModel, margin = 0.1)
# text(fit_rpart$finalModel, cex = 0.75)

# scan over a range of number of variables available to split at each node
# mtry <- seq(1, 7, 2)
# 
# fit model with 50 trees per forest
# fit_rf <- train(rating ~ movieId + userId, data = edx, method = "rf", tuneGrid = data.frame(mtry = mtry), ntree = 50, trControl = Control)
# 
# plot results, extract best parameters, and examine confusion matrix and variable importance
# plot(fit_rf)
# mtry[which.max(fit_rf$results$Accuracy)]
# predictions_rf <- predict(fit_rf, validation)
# confusionMatrix(predictions_rf, validation$rating)$overall[["Accuracy"]]
# varImp(fit_rf)

# fit_knn <- train(rating ~ movieId + userId, data = edx, method = "knn", trControl = Control, tuneGrid = data.frame(k = seq(3, 11, 2)))

# edx_wide <- edx %>%
#   select(userId, movieId, rating) %>%
#   spread(movieId, rating)
# 
# r = Reco()
# 
# opts = r$tune(edx_wide, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2), costp_l1 = 0, costq_l1 = 0, nthread = 1, niter = 10))
# 
# r$train(edx_wide, opts = c(opts$min, nthread = 1, niter = 20))
# 
# predictions <- r$predict()

### Final Validation

# re-estimate coefficients using detected best lambda
lambda = lambda_best
movie_avg_r <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + lambda))
user_avg_r <- edx %>%
  left_join(movie_avg_r, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n() + lambda))
b_g_r = numeric(length(genre))
for (i in 1:length(genre)) {
  idx <- which(edx[[genre[i]]])
  movs <- edx[idx,]
  genre_effect <- movs %>%
    select(Action:Western)
  genre_effect <- as.matrix(genre_effect) %*% b_g_r
  resid <- movs %>%
    left_join(movie_avg_r, by='movieId') %>%
    left_join(user_avg_r, by='userId') %>%
    mutate(resid = rating - mu - b_i - b_u) %>%
    pull(resid)
  resid <- resid - genre_effect
  b_g_r[i] = sum(resid)/(length(resid) + lambda)
}
genre_effect <- edx %>%
  select(Action:Western)
genre_effect <- as.matrix(genre_effect) %*% b_g_r
date_effect_r <- edx %>%
  left_join(movie_avg_r, by='movieId') %>%
  left_join(user_avg_r, by='userId') %>%
  mutate(date = round_date(as_datetime(timestamp), "week"), genre_effect = genre_effect, resid = rating - genre_effect - mu - b_i - b_u) %>%
  group_by(date) %>%
  summarize(d = sum(resid)/(n() + lambda))
year_effect_r <- edx %>%
  left_join(movie_avg_r, by='movieId') %>%
  left_join(user_avg_r, by='userId') %>%
  mutate(date = round_date(as_datetime(timestamp), "week")) %>%
  left_join(date_effect_r, by = "date") %>%
  mutate(genre_effect = genre_effect, resid = rating - genre_effect - mu - b_i - b_u - d) %>%
  group_by(year) %>%
  summarize(y = sum(resid)/(n() + lambda))

# calculate predictions and RMSE on final validation set
genre_effect_v <- validation %>%
    select(Action:Western)
  genre_effect_v <- as.matrix(genre_effect_v) %*% b_g_r
predictions <- validation %>%
    left_join(movie_avg_r, by='movieId') %>%
    left_join(user_avg_r, by='userId') %>%
    mutate(date = round_date(as_datetime(timestamp), "week")) %>%
    left_join(date_effect_r, by = "date") %>%
    left_join(year_effect_r, by = "year") %>%
    mutate(pred = mu + b_i + b_u + d + y) %>%
    pull(pred)
  
# predictions <- round(2 * (predictions + genre_effect))/2
predictions <- predictions + genre_effect_v
sqrt(mean((predictions - validation$rating)^2))
