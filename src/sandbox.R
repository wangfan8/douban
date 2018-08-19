library(tidyverse)
library(tidytext)
library(purrr)
library(lubridate)
library(stringr)

data <- read_csv("~/Dropbox/douban/data/li_douban_book.csv")

# clean up some common writer names
data %>% 
  mutate(author = if_else(grepl("村上春树"
                                , author)
                          , "Haruki Murakami"
                          , author)) %>%
  mutate(author = if_else(grepl("凡尔纳"
                                , author)
                          , "Jules Verne"
                          , author)) ->
  data

# filter out all chinese books (at least title contains chinese)
data$chinese_ind <- 0
data[grep("[\u4e00-\u9fa5]", data$title),]$chinese_ind <- 1

# book read in each year by language 
# filter out 2008 due to retroactive marking
data %>% 
  filter(year(updated) != 2008 & status == 'read') %>%
  ggplot(aes(factor(year(updated)), fill = factor(chinese_ind))) +
  geom_bar()

# pages of book read
data %>% 
  filter(year(updated) != 2008 & status == 'read' & !is.na(pages) & pages > 0) %>%
  mutate(read_year = year(updated)) %>%
  group_by(read_year)%>%
  summarise(total_page = sum(pages, na.rm = TRUE)) %>%
  ggplot(aes(x = read_year, y = total_page)) +
  geom_line()

# average number of pages per book
data %>% 
  filter(year(updated) != 2008 & status == 'read' & !is.na(pages) & pages > 0) %>%
  mutate(read_year = year(updated)) %>%
  group_by(read_year)%>%
  summarise(average_page = sum(pages, na.rm = TRUE)/n()) %>%
  ggplot(aes(x = read_year, y = average_page)) +
  geom_line()

# page distribution
data %>% 
  filter(year(updated) != 2008 & status == 'read' & !is.na(pages) & pages > 0) %>%
  mutate(read_year = year(updated)) %>%
  group_by(read_year)%>%
  ggplot(aes(x = read_year, y = pages, group = read_year)) +
  geom_boxplot()

# page distribution by language
data %>% 
  filter(year(updated) != 2008 & status == 'read' & !is.na(pages) & pages > 0) %>%
  mutate(read_year = year(updated)) %>%
  group_by(read_year, chinese_ind)%>%
  ggplot(aes(x = read_year
             , y = pages
             , group = read_year)) +
  geom_boxplot() +
  facet_grid(col = vars(chinese_ind))

# top 20 authors
data %>% 
  filter(status == 'read' & !is.na(author)) %>%
  group_by(author) %>%
  tally() %>%
  top_n(20) %>%
  mutate(author = reorder(author, n)) %>%
  ggplot(aes(x = author, y = n)) +
  geom_col() +
  coord_flip() +
  theme_grey(base_family = "STKaiti")

# top 20 authors page distribution
data %>% 
  filter(status == 'read' & !is.na(author)) %>%
  group_by(author) %>%
  tally() %>%
  top_n(20) %>%
  mutate(author = reorder(author, n)) %>%
  mutate(author = as.character(author)) %>%
  left_join(data, by = "author") %>%
  filter(!is.na(pages) & pages > 0) -> data_fav_author

data_fav_author %>%
  group_by(author)%>%
  ggplot(aes(x = author, y = pages, group = author)) +
  geom_boxplot() +
  coord_flip() +
  theme_grey(base_family = "STKaiti")









