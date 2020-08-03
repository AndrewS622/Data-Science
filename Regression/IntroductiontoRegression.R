## Baseball as a Motivating Example
library(Lahman)
head(Teams)
library(dplyr)
library(ggplot2)

# filter to between 1961 and 2001, normalize to per-game
Teams6101 <- Teams %>%
  filter(yearID >= 1961 & yearID <= 2001) %>%
  mutate(AB = AB/G, R = R/G, E = E/G, W = W/G, X3B = X3B/G, X2B = X2B/G)

# relationship between at-bats and runs
Teams6101 %>%
  ggplot(aes(AB, R)) +
  geom_point()

# win rate versus fielding errors
Teams6101 %>%
  ggplot(aes(E, W)) + 
  geom_point()

# triples versus doubles
Teams6101 %>%
  ggplot(aes(X2B, X3B)) + 
  geom_point()

## Correlation
# between runs and at-bats
cor(Teams6101$R, Teams6101$AB)

# between win rate and errors
cor(Teams6101$W, Teams6101$E)

# between doubles and triples
cor(Teams6101$X2B, Teams6101$X3B)

## Stratification and Variance Explained
library(HistData)
data("GaltonFamilies")
set.seed(1989, sample.kind = "Rounding")

# female heights takes mother and first-born daughter's heights
female_heights <- GaltonFamilies %>%
  filter(gender == "female") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(mother, childHeight) %>%
  rename(daughter = childHeight)

# calculate parameters
muM <- mean(female_heights$mother)
sdM <- sd(female_heights$mother)
muD <- mean(female_heights$daughter)
sdD <- sd(female_heights$daughter)
corr <- cor(female_heights$mother, female_heights$daughter)

# calculate regression parameters
m <- corr * sdD/sdM
b <- muD - m*muM

# variance explained
100 * corr^2

# prediction
mom <- 60
daughter <- mom*m + b
