## Dates and Times
library(dslabs)
library(dplyr)
library(lubridate)
options(digits = 3) 

# ambiguity of dates
dates <- c("09-01-02", "01-12-07", "02-03-04")
ymd(dates)
mdy(dates)
mdy(dates)

# Brexit polls ending in April
data(brexit_polls)
start <- brexit_polls$startdate
month <- month(start)
sum(month == 4)

# rounding to week
end <- round_date(brexit_polls$enddate, unit = "week")
sum(end == "2016-06-12")

# which day did most polls end
table(weekdays(brexit_polls$enddate))

data(movielens)

# convert timestamp to dates (uses epoch time normally)
movielens <- movielens %>% mutate(timestamp = as_datetime(timestamp))

# what year and hour were most common
years <- year(movielens$timestamp)
hours <- hour(movielens$timestamp)
which.max(table(years))
which.max(table(hours))

## Text
library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)

gutenberg_metadata

# which ID is Pride and Prejudice
idx <- which(str_detect(gutenberg_metadata$title, "Pride and Prejudice"))
ids <- gutenberg_metadata$gutenberg_id[idx]
length(ids)

# get rid of duplicates using gutenberg_works()
idx <- which(str_detect(gutenberg_works()$title, "Pride and Prejudice$"))
id <- gutenberg_works()$gutenberg_id[idx]

# extract words using unnest_tokens()
txt <- gutenberg_download(id)
words <- unnest_tokens(txt, word, text)
nrow(words)

# filter out stop_words and digits
words_filt <- words %>% filter(!(word %in% stop_words$word) & 
                                 !str_detect(word, "[_\\d+]"))
nrow(words_filt)

# count number of times each word appears and sort by most common
tab <- table(words_filt$word)
tab_sort <- abs(sort(desc(tab)))
common <- which(tab_sort > 100)
length(tab_sort[common])
tab_sort[common[1]]

# use sentiment analysis with afinn lexicon
afinn <- get_sentiments("afinn")
afinn_sentiments <- inner_join(words_filt, afinn, by = "word")
nrow(afinn_sentiments)
mean(afinn_sentiments$value > 0)
sum(afinn_sentiments$value == 4)
