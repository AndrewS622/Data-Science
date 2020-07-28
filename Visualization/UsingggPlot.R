## ggplot2 basics
library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
data(murders)
p <- ggplot(murders)
class(p)

# scatterplot
murders %>% ggplot(aes(x = population, y = total)) +
  geom_point()

# text labels
murders %>% ggplot(aes(population, total, label = abb)) + 
  geom_label()

# colors
murders %>% ggplot(aes(population, total,label= abb)) +
  geom_label(color = "blue")

# filtering colors by another property
murders %>% ggplot(aes(population, total, label = abb, color = region)) +
  geom_label()

p <- murders %>% ggplot(aes(population, total, label = abb, color = region)) + 
  geom_label()

# logarithmic scaling
p + scale_x_log10() + scale_y_log10() +
  ggtitle("Gun murder data")

p <- heights %>% 
  ggplot(aes(height))

# histograms
p + geom_histogram()

p + geom_histogram(binwidth = 1)

# density plots
heights %>% 
  ggplot(aes(height)) + geom_density()

heights %>% 
  ggplot(aes(height, group = sex)) + geom_density()

# separating and coloring/filling by group
heights %>% 
  ggplot(aes(height, group = sex, color = sex)) + geom_density()

heights %>% ggplot(aes(height, fill=sex)) + geom_density(alpha = 0.2)
