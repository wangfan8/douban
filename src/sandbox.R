library(tidyverse)
library(tidytext)
library(purrr)
library(lubridate)
library(stringr)

data <- read_csv("~/Dropbox/douban/data/li_douban_book.csv")

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









