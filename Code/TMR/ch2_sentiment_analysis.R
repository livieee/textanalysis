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

#load text file from corpus
setwd("~/Documents/Sample Corpus/Short_ID_Sample Corpus_txt")
tbl <- list.files(pattern = "*.txt") %>%
  map_chr(~ read_file(.)) %>%
  data_frame(text = .)

files.v <- list.files(pattern=".*txt")
title <- gsub("*.txt", "",files.v)
authors <-gsub("^([^-]*-[^-]*)-.*$", "\\1", title)
authors1 <- gsub("^([^_]*_[^_]*)_.*$", "\\1", authors)


# In order to turn it into a tidy text dataset
#we first need to put it into a data frame
library(dplyr)
tbl_df <- data_frame(line = 1:165, text = tbl$text)
tbl_df$title <- title
tbl_df$author <- authors1
tbl_df

library(tidytext)
tidy_corpus <- tbl_df %>%
  unnest_tokens(word, text) 
tidy_corpus


#To see most common joy words in a book
#filter the joy words from NRC lexicon
#get the joy words in the novel by inner-joy with joy-words
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_corpus %>%
  filter(title == "Evans_2012_The_African_origins") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

#Examine how sentiment changes throught each novel
library(tidyr)

#Find a sentiment score for each word using the Bing
#Inner join them with words of novel
#count up number of positive and negative wors in defined sections of each book
#defined section: each with 80 lines
evans_2012_text <- tidy_corpus %>%
  filter(title == "Evans_2012_The_African_origins")
evans_2012_text$wordnumber <- seq_along(evans_2012_text$word)
evans_2012_the_african <-evans_2012_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(title, index = wordnumber%/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


#plot against the index
library(ggplot2)

ggplot(evans_2012_the_african, aes(index, sentiment, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x")



#Examine three lexicons use "Hester_1997_The_melodic_and"
hester_1997_text <- tidy_corpus %>% 
  filter(title == "Hester_1997_The_melodic_and")

hester_1997_text
hester_1997_text$wordnumber <- seq_along(hester_1997_text$word)
#Inner-join afinn words and book words
afinn <- hester_1997_text %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = wordnumber %/% 500) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(hester_1997_text %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          hester_1997_text %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = wordnumber %/% 500, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#We have estimate of the net sentiment in each chunk of the text
#For three lexicon
#Visualzie them
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")


#Examine three lexicons use "Keller_2003_Mapping_the_soundscape"
keller_2003_text <- tidy_corpus %>% 
  filter(title == "Keller_2003_Mapping_the_soundscape")

keller_2003_text
keller_2003_text$wordnumber <- seq_along(keller_2003_text$word)
#Inner-join afinn words and book words
afinn <- keller_2003_text %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = wordnumber %/% 200) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(keller_2003_text %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          keller_2003_text %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = wordnumber %/% 200, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#We have estimate of the net sentiment in each chunk of the text
#For three lexicon
#Visualzie them
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")


##Find how much each word contribute to each sentiment
bing_word_counts <- tidy_corpus %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts
#Visualize it 
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


#Add anoamly into custom stop-words
custom_stop_words <- bind_rows(data_frame(word = c("music"), 
                                          lexicon = c("custom")), 
                               stop_words)

custom_stop_words


#2.5 Wordclouds
library(wordcloud)
png(paste("~/Documents/Sample Corpus/most-100-common-words-in-corpus.png"), 
    width=12,height=8, units='in', res=300)
tidy_corpus %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
dev.off()


#sentimental anlaysis to tag positive and negative words
#find the most common postivei and negative words
library(reshape2)
png(paste("~/Documents/Sample Corpus/most-common-positive-and-negative-words-in-corpus.png"), 
    width=12,height=8, units='in', res=300)
tidy_corpus %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
dev.off()


#2.6 Look at units beyond just words

#split a novel by chapter
austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex", 
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

#summarize each chapter number of each book for an author
austen_chapters %>% 
  group_by(book) %>% 
  summarise(chapters = n())

#get negative emotion words from bing
bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- tidy_corpus %>%
  group_by(title) %>%
  summarize(words = n())

#find the most negative chapters in each book of an author
#
tidy_corpus %>%
  semi_join(bingnegative) %>%
  group_by(title) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("title")) %>%
  mutate(ratio = negativewords/words) %>%
  top_n(3) %>%
  ungroup()
  