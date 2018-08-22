library(tidyverse)
library(tidytext)
library(purrr)
library(lubridate)
library(stringr)
library(here)
library(wordcloud2)

file_path <- paste(here::here(), "/data/li_douban_book.csv", sep="") 
data <- read_csv(file_path)

# filter out all chinese books (at least summary contains chinese)
data$chinese_ind <- 0
data[grep("[\u4e00-\u9fa5]", data$summary),]$chinese_ind <- 1

# clean up some common writer names
data %>% 
  mutate(author = if_else(grepl("村上春树"
                                , author)
                          , "Haruki Murakami"
                          , author)) %>%
  mutate(author = if_else(grepl("凡尔纳"
                                , author)
                          , "Jules Verne"
                          , author)) %>%
  mutate(author = if_else(grepl("屠格涅夫"
                                , author)
                          , "屠格涅夫"
                          , author)) %>%
  mutate(author = if_else(grepl("Tolkien"
                                , author)
                          , "J. R. R. Tolkien"
                          , author)) %>%
  mutate(author = if_else(grepl("George", author) & grepl("Martin", author)
                          , "George R. R. Martin"
                          , author)) %>%
  mutate(author = if_else(grepl("Rowling"
                                , author)
                          , "J. K. Rowling"
                          , author)) %>%
  mutate(author = if_else(grepl("安妮·普鲁克斯"
                                , author)
                          , "Annie Proulx"
                          , author)) ->
  data

# count the number of characters in each summary
data %>% mutate(summary_length = nchar(summary)) -> data

# first limited to summary length > 50 and exclude chinese title

data %>% filter(summary_length >=50 
                & chinese_ind == 0) -> book_summary

custom_stop <- tibble(word = c("book", "books"))

# create word cloud

book_summary %>% 
  select(title, summary, author) %>%
  unnest_tokens(word, summary) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop) %>%
  count(word, sort = TRUE) %>%
  top_n(200) %>%
  rename(freq = n) %>%
  wordcloud2()

# word frequency by author
book_summary %>% 
  select(title, summary, author) %>%
  unnest_tokens(word, summary) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop) %>%
  group_by(author) %>%
  count(word) -> word_freq_by_author


  
