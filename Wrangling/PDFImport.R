## Extracting a table from a PDF
# From course material

# download PDF
library("pdftools")
temp_file <- tempfile()
url <- "https://www.pnas.org/content/pnas/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file, mode="wb")
txt <- pdf_text(temp_file)
file.remove(temp_file)

# keep page containing table of interest
raw_data_research_funding_rates <- txt[2]
raw_data_research_funding_rates %>% head

# split by newline characters
tab <- str_split(raw_data_research_funding_rates, "\n")
tab <- tab[[1]]

# extract names of columns
the_names_1 <- tab[3]
the_names_2 <- tab[4]

# remove leading spaces, characters after comma
# split at locations where at least two spaces are present
the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1

# trim leading space and split as above
the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2

# join together
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names

# extract data from rows 6:14, trim, split, and set names
new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()
