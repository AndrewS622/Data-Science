## Import
library(tidyverse)
url <- "http://nrvis.com/data/mldata/breast-cancer-wisconsin_wdbc.csv"
tab <- read_csv(url, col_names = TRUE)
nrow(tab)
ncol(tab)

## Reshaping
library(dslabs)
length(co2)
class(co2)
co2
# not tidy: need year, month, value columns

# change months to digits and add year column
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>%
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

# gather so each column has a year, month, and value
co2_tidy <- co2_wide %>% gather("month", "co2", -year)
head(co2_tidy)

# plot values with year as color
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) +
  geom_line()

# examine admissions statistics, removing applicants percentage column
data(admissions)
dat <- admissions %>% select(-applicants)
head(dat)

# tidy by separating admitted men and women into their own columns
dat_tidy <- dat %>% spread(gender, admitted)

# gather to create new keys admitted and applicants and their values
tmp <- gather(admissions, key, value, admitted:applicants)
tmp

# unite key and gender and spread into applicants and acceptances by gender for each major
tmp2 <- tmp %>% unite(column_name, c(key, gender))
tmp2

final <- tmp2 %>% spread(column_name, value)
final

## Combining Tables
# offensive statistics for baseball players in Batting
library(Lahman)

# filter to top 10 home run hitters in 2016
top <- Batting %>%
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%
  slice(1:10)

top %>% as_tibble()
Master %>% as_tibble()

# names and stats of top 10 by joining with Master
top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)

# add salary column to top_names
top_salary <- Salaries %>% 
  filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, playerID, HR, salary)

# filter awards to only 2016, only players in top 10 hitters
top_awards <- AwardsPlayers %>%
  filter(yearID == 2016) %>%
  semi_join(x=top_salary)
nrow(top_awards)

# find number of unique players who won awards but were not in the top 10 hitters
top_awards_not_hitters <- AwardsPlayers %>%
  filter(yearID == 2016) %>%
  anti_join(top_salary)

length(unique(top_awards_not_hitters$playerID))

## Web Scraping
library(rvest)
url <-"https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"

# read in web page containing MLB payrolls
h <- read_html(url)

# extract table nodes
nodes <- html_nodes(h, "table")
html_text(nodes[[8]])
html_table(nodes[[8]])

# how many of first four nodes are tables of payroll
data.frame(html_table(nodes[[1]]))
data.frame(html_table(nodes[[2]]))
data.frame(html_table(nodes[[3]]))
data.frame(html_table(nodes[[4]]))

# last table shows averages across all teams over time
l <- length(nodes)
data.frame(html_table(nodes[[l-2]]))
data.frame(html_table(nodes[[l-1]]))
data.frame(html_table(nodes[[l]]))

# extract table entries 10 and 19
tab_1 <- html_table(nodes[[10]])
tab_2 <- html_table(nodes[[19]])
head(tab_1)
head(tab_2)

# remove first column of tab_1 and first row of both (headers)
tab_1 <- tab_1[-1,-1]
tab_2 <- tab_2[-1,]

# specify column names and re-specify row numbers
colnames(tab_1) = c("Team", "Payroll", "Average")
rownames(tab_1) <- seq(length=nrow(tab_1))
colnames(tab_2) = c("Team", "Payroll", "Average")
rownames(tab_2) <- seq(length=nrow(tab_2))
head(tab_1)
head(tab_2)

# join tables
full_join(tab_1, tab_2, by="Team")
nrow(full_join(tab_1, tab_2, by="Team"))

# explore Brexit data
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)
tab <- html_nodes(h, "table")
length(tab)

# find first table with 9 columns and first column of "Date(s) conducted"
i <- 0
detect <- FALSE
while (!detect) {
  i <- i + 1
  t <- suppressWarnings(html_table(tab[[i]], fill = TRUE))
  detect <- ncol(t) == 9 & colnames(t)[1] == "Date(s) conducted"
}
