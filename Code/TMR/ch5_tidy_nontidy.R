#Chapter 5
#load corpus into tm DTM format
library(tm)
input.dir <- ("~/Documents/Sample Corpus/Short_ID_Sample Corpus_txt")
a  <-Corpus(DirSource(input.dir), readerControl = list(language="english")) #specifies the exact folder where my text file(s) is for analysis with tm.
summary(a)  #check what went in
a <- tm_map(a, removeNumbers)
a <- tm_map(a, removePunctuation)
a <- tm_map(a , stripWhitespace)
a <- tm_map(a, tolower)
a <- tm_map(a, removeWords, stopwords("english")) # this stopword file is at C:\Users\[username]\Documents\R\win-library\2.13\tm\stopwords 
a <- tm_map(a, stemDocument, language = "english")
    adtm <-DocumentTermMatrix(a) 
#adtm <- removeSparseTerms(adtm, 0.75)


adtm

#access the terms in the document
terms <- Terms(adtm)
head(terms)

#turn DTM into tidy text format
library(dplyr)
library(tidytext)

a_td <- tidy(adtm)
a_td

#perform sentiment analysis
a_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

a_sentiments


#visualize which words from this article most often
#contributed to positive or negative sentiment
library(ggplot2)

a_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()


#extract the year from each document's name
#compute the toal number of words within each year
library(tidyr)
library(tidytext)

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

library(tidytext)
tidy_corpus <- tbl_df %>%
  unnest_tokens(word, text)
  

data(stop_words)
tidy_corpus <- tidy_corpus %>%
  anti_join(stop_words)

tidy_corpus <- tidy_corpus %>%
  #group_by(author) %>%
  count(title, word, sort = TRUE) %>%
  ungroup()


year_term_counts <- tidy_corpus %>%
  filter (grepl("Magadini", author)) %>%
  extract(author, "year", "(\\d+)", convert = TRUE) %>%
  complete(year, word, fill = list(n = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(n))

#pick up several words and visualize how they changed in 
#frequency over time
library(ggplot2)
year_term_counts %>%
  filter(word %in% c("music", "rhythm", "note", "triplets", "polyrhythm")) %>%
  ggplot(aes(year, n / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ word, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("% frequency of word in inaugural address")


#5.2
#turn tidy into dtm
tidy_corpus %>%
  cast_dtm(title, word, n)

#turn tidy into dfm
tidy_corpus %>%
  cast_dfm(title, word, n)

library(Matrix)

# cast into a Matrix object
m <- tidy_corpus %>%
  cast_sparse(title, word, n)

class(m)
dim(m)

#create a DTM of corpus
corpus_dtm <- tbl_df %>%
  unnest_tokens(word, text) %>%
  count(title, word) %>%
  cast_dtm(title, word, n)

corpus_dtm

