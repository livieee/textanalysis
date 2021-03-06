#chapter 2 sentiment analysis
library(tidytext)
sentiments
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

#take the text of the book in a tidy format(tokenized)
#keep track of which line/text of the book each word comes from
library(dplyr)
library(stringr)

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

#Make a tidy form of corpus
library(tidytext)
tidy_corpus <- tbl_df %>%
  unnest_tokens(word, text) 
tidy_corpus

#Split dataframe randomly into 11 groups based on text
random_tbl_df <- tbl_df[sample(1:nrow(tbl_df)),]
v <- 1:11
group_name <- rep(v, each = 15)
random_tbl_df$group <- group_name
tidy_random_corpus <- random_tbl_df %>%
  unnest_tokens(word, text) %>%
  
  
  library(dplyr)
##Find how much each word contribute to each sentiment
bing_word_counts <- tidy_random_corpus %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(group) %>%
  count(word, sentiment, sort = TRUE) 

bing_word_counts_sorted <- bing_word_counts[
  with(bing_word_counts, order(group)),
  ]
group_sentiment_words <- 
  bing_word_counts %>%
  split(.,.$group)

#Generate a list of most 15 common positive and negative words
sentiment_word_list <- bing_word_counts %>%
  # filter(group == 1) %>%
  group_by(group,sentiment) %>%
  top_n(15) %>%
  arrange(group) %>%
  # ungroup()) %>%
  mutate(i = row_number()) %>%
  spread(sentiment, word) %>%
  rename(frequency = n, index = i)

write.csv(sentiment_word_list,"15-common-pos-neg-words.csv")
 
positive <- sentiment_word_list[,c(1,2,3,5)]
positive_merge_duplicate <- aggregate(frequency~positive,data=positive,FUN=sum)

negative <- sentiment_word_list[,1:4]
negative_merge_duplicate <- aggregate(frequency~negative,data=negative,FUN=sum)

write.csv(positive_merge_duplicate, "positive_merge_duplicate.csv")
write.csv(negative_merge_duplicate, "negative_merge_duplicate.csv")
