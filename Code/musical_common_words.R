library(tidyverse)
input.dir <- "Short_ID_Sample Corpus_txt"
files.v <- dir(path=input.dir, pattern=".*txt")
title <- gsub("*.txt", "",files.v)
authors <-gsub("^([^-]*-[^-]*)-.*$", "\\1", title)
authors1 <- gsub("^([^_]*_[^_]*)_.*$", "\\1", authors)
#load text file from corpus
tbl <- list.files(pattern = "*.txt") %>%
  map_chr(~ read_file(.)) %>%
  data_frame(text = .)
tbl[3,1]
tbl$author <- authors1
tbl$title <- title
tbl



# In order to turn it into a tidy text dataset
#we first need to put it into a data frame
library(dplyr)
tbl_df <- data_frame(line = 1:165, text = tbl$text)
tbl_df$title <- title
tbl_df$author <- authors1
tbl_df


tbl_df$text <- tbl_df$text %>%
  iconv(., "ASCII", "UTF-8", sub="") %>%
  #enc2utf8(tbl_df$text) %>%
  #{paste(., collapse = " ")} %>%
  tolower(.) %>%
  gsub("[^[:alnum:][:space:]']", " ", .)
# strip(.,char.keep=c("/","'"),
#        digit.remove = F,
#        apostrophe.remove=F,lower.case=T)
library(tidytext)
tidy_corpus <- tbl_df %>%
  unnest_tokens(word, text, 
                token = stringr::str_split, pattern = "\\s+") %>%
  filter(!str_detect(word, "\\d"))


#remove stop words
data(stop_words)
tidy_corpus <- tidy_corpus %>%
  anti_join(stop_words)


#Most frequent 25 words excluding numbers 
plots <-
  tidy_corpus %>%
  group_by(title) %>%
  count(word, sort = TRUE) %>%
  top_n(25) %>%
  arrange(title) %>%
  ungroup() %>%
  mutate(title = title,
         text_order = nrow(.):1) 

newplots <- aggregate(n~word,data=plots,FUN=sum)



write.csv(newplots,"freq-merge-duplicate.csv")
