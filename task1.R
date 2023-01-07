## Task 1 - Getting and cleaning the data

# Libraries
library(tibble)
library(dplyr)
library(stringr)
library(readr)

# Set randomiser seed for reproducibility
set.seed(1969)

# Sample size for whole corpus
sample_size <- 0.05

# Data constants
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
filename <- "~/Downloads/Coursera-SwiftKey.zip"
data_folder <- "raw_data"

# Obtain and extract raw data when required
if (!file.exists(filename)) {
  download.file(url, destfile = filename)
  unzip(filename, files = source_files)
}

if (!file.exists(data_folder)) {
  source_files_index <- grep("en_US", unzip(filename, list = TRUE)$Name)
  source_files <- unzip(filename, list = TRUE)[source_files_index, "Name"]
  unzip(filename, files = source_files, exdir = data_folder)
}

# Read all raw data files and sample each 
all_text <- list()
files <- list.files(data_folder, full.names = TRUE, recursive = TRUE)

for (f in files) {
  text <- readLines(f, skipNul = TRUE)
  n <- rbinom(n = length(text), size = 1, prob = sample_size)
  all_text[[f]] <- text[as.logical(n)]
}

names(all_text) <- str_extract(files, "(?<=US\\.)(.*)(?=.txt)")

write_rds(all_text, "all_text.rds")

# Create tibble with all text entries and clean the data
(all_text_df <- tibble(source = rep(names(all_text),
                                      lapply(all_text, length)),
                         text = unlist(all_text)) %>%
  mutate(text = tolower(text),
         text = str_remove_all(text, "[0-9]|[[:punct:]]"),
         text = iconv(text, "utf-8", "ascii", sub = "")) %>%
  group_by(source) %>%
  mutate(line = 1:n()) %>%
  ungroup())

# Sample the data (same fraction for each source)
all_text_df %>% 
  group_by(source) %>% 
  sample_frac(sample_size) %>% 
  ungroup() %>% 
  count(source) %>% 
  write_csv("sample_text_df.csv")
