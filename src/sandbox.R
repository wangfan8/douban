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
                          , author))->
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

# Is there a difference between chinese book number of pages and others(mainly english)?
data %>% 
  filter(status == 'read' & !is.na(pages) & pages > 0) %>%
  mutate(chinese_ind_cat = if_else(chinese_ind == 1
                                   , "Chinese"
                                   , "Others")) %>%
  group_by(chinese_ind_cat) %>%
  ggplot(aes(x = chinese_ind_cat, group = chinese_ind_cat, y = pages)) +
  geom_violin()

# Is there a difference between chinese book number of reviews and others(mainly english)?
data %>% 
  filter(status == 'read' & !is.na(numRaters) & numRaters > 0) %>%
  mutate(chinese_ind_cat = if_else(chinese_ind == 1
                                   , "Chinese"
                                   , "Others")) %>%
  group_by(chinese_ind_cat) %>%
  ggplot(aes(x = chinese_ind_cat, group = chinese_ind_cat, y = numRaters)) +
  geom_boxplot()

data %>% 
  filter(status == 'read' & !is.na(numRaters) & numRaters > 0) %>%
  mutate(chinese_ind_cat = if_else(chinese_ind == 1
                                   , "Chinese"
                                   , "Others")) %>%
  group_by(chinese_ind_cat) %>%
  summarise(trimmedMean = mean(numRaters, trim = 0.05),
            quantile25 = quantile(numRaters, probs = 0.25),
            quantile50 = quantile(numRaters, probs = 0.50),
            quantile75 = quantile(numRaters, probs = 0.75))

# Is there a difference between chinese book average reviews and others(mainly english)?
data %>% 
  filter(status == 'read' & !is.na(averageRate) & numRaters > 10) %>%
  mutate(chinese_ind_cat = if_else(chinese_ind == 1
                                   , "Chinese"
                                   , "Others")) %>%
  group_by(chinese_ind_cat) %>%
  ggplot(aes(x = chinese_ind_cat, group = chinese_ind_cat, y = averageRate)) +
  geom_boxplot()

data %>% 
  filter(status == 'read' & !is.na(averageRate) & numRaters > 10) %>%
  mutate(chinese_ind_cat = if_else(chinese_ind == 1
                                   , "Chinese"
                                   , "Others")) %>%
  group_by(chinese_ind_cat) %>%
  summarise(mean = mean(averageRate),
            quantile25 = quantile(averageRate, probs = 0.25),
            quantile50 = quantile(averageRate, probs = 0.50),
            quantile75 = quantile(averageRate, probs = 0.75))

# ralationship between number of pages and average rate

data %>% 
  filter(status == 'read' 
         & !is.na(averageRate) 
         & numRaters > 10 
         & !is.na(pages)
         & pages > 0) %>%
  mutate(chinese_ind_cat = if_else(chinese_ind == 1
                                   , "Chinese"
                                   , "Others")) %>%
  group_by(chinese_ind_cat) %>%
  ggplot(aes(x = pages, y = averageRate, weight = sqrt(numRaters))) +
  geom_point(aes(size = sqrt(numRaters)
                 , color = chinese_ind_cat)) +
  geom_smooth(span = 0.4)
  






