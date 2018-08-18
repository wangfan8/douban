library(tidyverse)
library(tidytext)
library(purrr)
library(lubridate)

load(file = "li_douban_book_extract")

# flatten the hierachical lists once
book_contents <- flatten(book_contents)

# we will need every 4th element as the book info
book <- book_contents[seq(from = 4,
                           to   = length(book_contents),
                           by   = 4)]

book <- flatten(book)

number_books <- length(book)

df_book <- data.frame(status      = character(length         = number_books),
                      updated     = as.Date(rep("2017-01-01", number_books)),
                      numRaters   = numeric(  length         = number_books),
                      averageRate = numeric(  length         = number_books),
                      author      = list(     length         = number_books),
                      binding     = character(length         = number_books),
                      pages       = numeric(  length         = number_books),
                      publisher   = character(length         = number_books),
                      title       = character(length         = number_books),
                      url         = character(length         = number_books),
                      author_info = character(length         = number_books),
                      summary     = character(length         = number_books),
                      price       = character(length         = number_books),
                      stringsAsFactors = FALSE)

for (i in 1:length(book)){
  df_book[i, "status"] = book[[i]]$status
  
}






