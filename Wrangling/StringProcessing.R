## String Processing Exercises
library(rvest)
library(tidyverse)
library(stringr)

# import Brexit data
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)

# change column names
col_names <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
colnames(polls) <- col_names

# filter for only rows containing a %
polls <- polls %>% filter(str_detect(remain, "%"))
nrow(polls)

# convert remain and leave to proportions
polls$remain <- parse_number(polls$remain)/100
polls$leave <- parse_number(polls$leave)/100

# convert NAs to 0 and convert undecided to proportion
polls$undecided <- str_replace(polls$undecided, "N/A", "0")
polls$undecided <- parse_number(polls$undecided)/100

polls$dates

# extract end day and month
temp <- str_extract_all(polls$dates, "\\d{1,2}\\s[a-zA-Z]{3,5}")
end_date <- sapply(temp, function(x) x[length(x)])
