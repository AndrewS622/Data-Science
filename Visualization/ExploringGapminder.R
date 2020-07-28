## Exploring the Gapminder data set
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)

# life expectancy vs. fertility in Africa
gapminder %>% filter(year == 2012 & continent == "Africa") %>%
  ggplot(aes(fertility , life_expectancy)) +
  geom_point()

# color by region
gapminder %>% filter(year == 2012 & continent == "Africa") %>%
  ggplot(aes(fertility, life_expectancy, color = region)) +
  geom_point()

# high life expectancy and low fertility
df <- gapminder %>% 
  filter(year == 2012 & continent == "Africa" & fertility <= 3 & life_expectancy >= 70) %>% 
  select(country, region)
df <- data.frame(df)

# influence of the Vietnam war on life expectancy
tab <- gapminder %>% 
  filter(year >= 1960 & year <= 2010 & (country == "Vietnam" | country == "United States"))

p <- tab %>% 
  ggplot(aes(year, life_expectancy, color = country)) + 
  geom_line()
p 

# Cambodia in the Vietnam war and the reign of Pol Pot
gapminder %>% 
  filter(year >= 1960 & year <= 2010 & country == "Cambodia") %>% ggplot(aes(year, life_expectancy)) + 
  geom_line()

# use of dollars/day as an indication of wealth
daydollars <- gapminder %>% 
  filter(continent == "Africa" & year == 2010) %>% mutate(dollars_per_day = gdp/population/365) %>% 
  filter(!is.na(dollars_per_day))

daydollars %>% 
  ggplot(aes(dollars_per_day)) + 
  geom_density() + 
  scale_x_continuous(trans = "log2")

# comparing across time
gapminder %>% 
  filter((year == 1970 | year == 2010 ) & continent == "Africa") %>% 
  mutate(dollars_per_day = gdp/population/365) %>% 
  filter(!is.na(dollars_per_day)) %>% 
  ggplot(aes(dollars_per_day)) + 
  geom_density() + 
  scale_x_continuous(trans = "log2") + 
  facet_grid(~year)

# stacking regions
gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>% 
  filter((year == 1970 | year == 2010) & continent == "Africa" & !is.na(dollars_per_day)) %>% 
  ggplot(aes(dollars_per_day, fill = region)) + 
  geom_density(alpha = 0.2, bw = 0.5, position = "stack") + scale_x_continuous(trans = "log2") + 
  facet_grid(~year)

# infant mortality vs. dollars/day
gapminder_Africa_2010 <- gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>% 
  filter(year == 2010 & continent == "Africa" & !is.na(dollars_per_day))

gapminder_Africa_2010 %>% 
  ggplot(aes(dollars_per_day, infant_mortality, color = region)) + 
  geom_point()

gapminder_Africa_2010 %>% 
  ggplot(aes(dollars_per_day, infant_mortality, color = region)) + 
  geom_point() + 
  scale_x_continuous(trans = "log2")

gapminder_Africa_2010 %>% 
  ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country)) + 
  geom_point() + 
  scale_x_continuous(trans = "log2") + 
  geom_text()

# comparing over time
gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>% 
  filter((year == 1970 | year == 2010) & continent == "Africa" & !is.na(dollars_per_day) & !is.na(infant_mortality)) %>% 
  ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country)) + 
  geom_point() + 
  scale_x_continuous(trans = "log2") +  
  geom_text() + 
  facet_grid(year~.)
