## Least Squares Estimates
library(HistData)
data("GaltonFamilies")
library(Lahman)
library(tidyverse)
library(ggplot2)

# extract father and first-born son heights
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# function to calculate residual sum of squares for a given regression equation
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

# results iterates over beta1 for a given value of beta0
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)
LSE <- results[which.min(results$rss),]

# Lahman library
Teams6101 <- Teams %>%
  filter(yearID >= 1961 & yearID <= 2001) %>%
  mutate(AB = AB/G, R = R/G, E = E/G, W = W/G, X3B = X3B/G, X2B = X2B/G, BB = BB/G, HR = HR/G)

# linear model to predict runs based on walks and home runs
fit <- lm(R ~ BB + HR, data = Teams6101)
summary(fit)

# plotting linear models with confidence intervals
galton_heights %>% ggplot(aes(father, son)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# alternative method
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) + 
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.2) +
  geom_point(data = galton_heights, aes(x = father, y = son))

# back to females from Galton data
set.seed(1989, sample.kind = "Rounding")
female_heights <- GaltonFamilies %>%
  filter(gender == "female") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(mother, childHeight) %>%
  rename(daughter = childHeight)

# fit linear model
model <- lm(mother ~ daughter, data = female_heights)
summary(model)

# predict heights based on model
predictions <- predict(model)
predictions[1]
female_heights$mother[1]

# assessing stability across years
library(Lahman)

# compare average from 1999-2001 vs. 2002
# use only players that have >100 plate appearances
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat9901 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb)) %>%
  ungroup()

# check for number of players above threshold of 0.2/PA
sum(bat9901$mean_singles > 0.2)
sum(bat9901$mean_bb > 0.2)

# join tables together for players in both tables
bat <- inner_join(bat_02, bat9901)

# compute correlations
cor(bat$mean_singles, bat$singles)
cor(bat$mean_bb, bat$bb)

# make scatterplots to inspect for bivariate normality
bat %>%
  ggplot(aes(mean_singles, singles)) +
  geom_point()

bat %>%
  ggplot(aes(mean_bb, bb)) +
  geom_point()

# fit linear models
model_singles <- lm(singles ~ mean_singles, data = bat)
model_bb <- lm(bb ~ mean_bb, data = bat)
summary(model_singles)
summary(model_bb)

## Tibbles, do, and broom
# filter to examine by league ID
library(broom)
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 

# utilize the tidy function to construct models for each group
dat %>%
  group_by(lgID) %>%
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>%
  filter(term == "HR")

library(HistData)
data("GaltonFamilies")
set.seed(1, sample.kind = "Rounding")

# include parents and children of both sexes
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

# summarize and compute height correlations
galton %>%
  group_by(pair) %>%
  summarize(n = n(), r = cor(childHeight, parentHeight))

library(broom)
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = T)) %>%
  filter(term == "parentHeight") %>% 
  mutate(spread = conf.high - conf.low) %>%
  select(estimate, conf.low, conf.high, spread)

## Regression and Baseball
# estimated parameter from code in videos
est <- function(bb, s, d, t, hr) {
  -2.769 + 0.371*bb + 0.519*s + 0.771*d + 1.24*t + 1.443*hr
}
est(2, 4, 1, 0, 1)
est(1, 6, 2, 1, 0)

# effects of walks and home runs on runs
library(Lahman)
Teams %>% 
  filter(yearID == 1971) %>%
  do(tidy(lm(R ~ BB + HR, data = .)))

# repeat for every year from 1961 to 2018, plot BB coefficient vs. time
Teams %>% 
  filter(yearID >= 1961 & yearID <= 2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() + 
  geom_smooth(method = "lm")

# fit linear model to time vs. BB effect
Teams %>% 
  filter(yearID >= 1961 & yearID <= 2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  filter(term == "BB") %>%
  ungroup() %>%
  do(tidy(lm(estimate ~ yearID, data = .)))
