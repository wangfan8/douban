library(tidyverse)
library(tidytext)
library(purrr)
library(lubridate)
library(here)

raw_file_path <- paste(here::here(), "/data/li_douban_book_extract", sep="") 
load(file = raw_file_path)

# flatten the hierachical lists once
book_contents <- flatten(book_contents)

# we will need every 4th element as the book info
book <- book_contents[seq(from = 4,
                           to   = length(book_contents),
                           by   = 4)]

book <- flatten(book)

number_books <- length(book)

df_book <- data_frame(status      = character(length         = number_books),
                      updated     = as.POSIXct(rep("2017-01-01", number_books)),
                      numRaters   = numeric(  length         = number_books),
                      averageRate = numeric(  length         = number_books),
                      author      = character(length         = number_books),
                      binding     = character(length         = number_books),
                      pages       = numeric(  length         = number_books),
                      publisher   = character(length         = number_books),
                      title       = character(length         = number_books),
                      url         = character(length         = number_books),
                      author_info = character(length         = number_books),
                      summary     = character(length         = number_books),
                      price       = character(length         = number_books),
                      isbn13      = character(length         = number_books),
                      isbn10      = character(length         = number_books))

for (i in 1:number_books){
  df_book[i, "status"]      = book[[i]]$status
  df_book[i, "updated"]     = ymd_hms(book[[i]]$updated)
  df_book[i, "numRaters"]   = book[[i]]$book$rating$numRaters
  df_book[i, "averageRate"] = book[[i]]$book$rating$average
  df_book[i, "author"]      = paste(unlist(book[[i]]$book$author)
                                    , collapse = "; ")
  df_book[i, "binding"]     = book[[i]]$book$binding
  df_book[i, "pages"]       = as.numeric(book[[i]]$book$pages)
  df_book[i, "publisher"]   = book[[i]]$book$publisher
  df_book[i, "title"]       = book[[i]]$book$title
  df_book[i, "url"]         = book[[i]]$book$alt
  df_book[i, "author_info"] = book[[i]]$book$author_intro
  df_book[i, "summary"]     = book[[i]]$book$summary
  df_book[i, "price"]       = book[[i]]$book$price
  df_book[i, "isbn13"]      = paste(c(book[[i]]$book$isbn13, 
                                      ""), 
                                      collapse = "")
  df_book[i, "isbn10"]      = book[[i]]$book$isbn10
}

out_file_path <- paste(here::here(), "/data/li_douban_book.csv", sep="")
write_csv(df_book, path = file.path(out_file_path))




