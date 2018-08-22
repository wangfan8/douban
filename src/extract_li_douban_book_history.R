library(httr)
library(here)

li_book <- list()

start <- seq(from = 0, to = 600, by = 20)

common_string <- "https://api.douban.com/v2/book/user/schweik/collections?start="
urls <- unlist(lapply(common_string, paste, start, sep=""))

book_contents <- lapply(lapply(urls, GET), content)

file_path <- paste(here::here(), "/data/li_douban_book_extract", sep="") 
save(book_contents, file = file_path)




