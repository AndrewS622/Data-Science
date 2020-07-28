options(digits = 3)
library(titanic)
library(tidyverse)
library(ggplot2)

## Exploring the Titanic data set
titanic <- titanic_train %>% 
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass), 
         Sex = factor(Sex)) %>% filter(!is.na(Age))

# age distributions grouped by sex
titanic %>% ggplot(aes(Age, fill = Sex)) + 
  geom_density() + 
  facet_grid(Sex~.)

# overlaid age distributions
titanic %>% ggplot(aes(Age, fill = Sex)) + 
  geom_density(alpha = 0.3)
sum(titanic$Sex == "male" & titanic$Age <= 35 & titanic$Age >= 18)
sum(titanic$Sex == "female" & titanic$Age <= 35 & titanic$Age >= 18)

# normality of age distribution
params <- titanic %>% summarize(mean = mean(Age), sd = sd(Age))
titanic %>% ggplot(aes(sample = Age)) + geom_qq(dparams = params) + geom_abline()

# survival by sex
titanic %>% ggplot(aes(Survived, fill = Sex)) + geom_bar(position =  position_dodge())

# survival by sex and age
titanic %>% ggplot(aes(Age, fill = Survived)) + 
  geom_density() + 
  facet_grid(Survived~.)

# overlaid as above
titanic %>% ggplot(aes(Age, fill = Survived)) + 
  geom_density(alpha = 0.3)

# survival by fare paid
titanic %>% filter(Fare != 0) %>% 
  ggplot(aes(x= Survived, y = Fare, group = Survived)) +
  geom_boxplot() + 
  scale_y_continuous(trans = "log2") + 
  geom_jitter()

# survival by passenger class
titanic %>% filter(!is.na(Pclass)) %>%
  ggplot(aes(Pclass, fill = Survived)) + 
  geom_bar()

# scaled version of above showing proportion
titanic %>% filter(!is.na(Pclass)) %>%
  ggplot(aes(Pclass, fill = Survived)) + 
  geom_bar(position = position_fill())

# passenger class by survival
titanic %>% filter(!is.na(Pclass)) %>%
  ggplot(aes(Survived, fill = Pclass)) + 
  geom_bar(position = position_fill())

# survival by passenger class and sex
titanic %>% ggplot(aes(Age, fill = Survived)) + 
  geom_density(alpha = 0.4) +
  facet_grid(Sex~Pclass)