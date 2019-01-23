#Explore Song title extraction
#Generate n-grams of cap words, set n = 3
#Find highest trigrams cap words across corpus
library(dplyr)
library(tidytext)


library(tidyverse)
#Set the path to the directory of your documents before you run tbl
setwd("~/Documents/Sample Corpus/Short_ID_Sample Corpus_txt")
tbl <- list.files(pattern = "*") %>%
  map_chr(~ read_file(.)) %>%
  data_frame(text = .)

#Read each txt file 
files.v <- list.files(pattern=".*txt")

#Extract title name from the file
title <- gsub("*.txt", "",files.v)

#Extract author and years, authors_year is used to differentiate each text
authors <-gsub("^([^-]*-[^-]*)-.*$", "\\1", title)
authors_year <- gsub("^([^_]*_[^_]*)_.*$", "\\1", authors)


#In order to turn it into a tidy text dataset
#we first need to put it into a data frame
library(dplyr)
tbl_df <- data_frame(line = 1:165, text = tbl$text)


#Store metadata into tbl_df
tbl_df$title <- title
tbl_df$author <- authors_year
tbl_df


#tokenize texts into consecutive sequence of words
#n=2, examine pairs of two consective words
library(tidytext)
corpus_bigrams <- tbl_df %>%
  unnest_tokens(bigram, text,
                token = "ngrams", n = 2, to_lower = F) 


corpus_bigrams

#examine the  most common bigrams
corpus_bigrams %>%
  group_by(title) %>%
  count(bigram, sort = TRUE)


library(tidyr)
#separate tetragrams into four columns
bigrams_separated <- corpus_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#keep bigrams that four words are not stopwords
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) 

bigrams_filtered2 <- bigrams_filtered %>%
  filter(str_detect(word1, "^[A-Z]")) %>%
  filter(str_detect(word2, "^[A-Z]")) %>%
  filter(!str_detect(word1, "\\d")) %>%
  filter(!str_detect(word2, "\\d"))
           


# new bigram counts:
bigrams_counts <- bigrams_filtered2 %>% 
  group_by(title)%>%
  count(word1, word2, sort = TRUE) %>%
  arrange(title) %>%
  top_n(15) %>%
  filter(n >1)

bigrams_counts


#recombine the four words into one bigram
bigrams_united <- bigrams_counts %>%
  unite(bigram, word1, word2,sep = " ")

bigrams_united
write.csv(bigrams_united, "top-15-cap-bigrams.csv")



#Find most frequent trigrams
corpus_trigrams <- tbl_df %>%
  unnest_tokens(trigram, text,
                token = "ngrams", n = 3, to_lower = F) 

corpus_trigrams

#examine the  most common trigrams
corpus_trigrams %>%
  #group_by(title) %>%
  count(trigram, sort = TRUE)


library(tidyr)
#separate tetragrams into four columns
trigrams_separated <- corpus_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

#keep bigrams that four words are not stopwords
trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) 

trigrams_filtered2 <- trigrams_filtered %>%
  filter(str_detect(word1, "^[A-Z]")) %>%
  filter(!str_detect(word1, "\\d")) %>%
  filter(!str_detect(word2, "\\d")) %>%
  filter(!str_detect(word3, "\\d")) 



# new bigram counts:
trigrams_counts <- trigrams_filtered2 %>% 
  group_by(title)%>%
  count(word1, word2,word3, sort = TRUE) %>%
  arrange(title) %>%
  top_n(10) %>%
  filter(n >1)

trigrams_counts


#recombine the four words into one bigram
trigrams_united <- trigrams_counts %>%
  unite(bigram, word1, word2,word3,sep = " ")

trigrams_united
write.csv(trigrams_united, "top-10-trigrams.csv")



#====Find most frequent tetrgrams=====
corpus_tetrgrams <- tbl_df %>%
  unnest_tokens(tetrgram, text,
                token = "ngrams", n = 4, to_lower = F) 

corpus_tetrgrams

#examine the  most common trigrams
corpus_tetrgrams %>%
  #group_by(title) %>%
  count(tetrgram, sort = TRUE)


library(tidyr)
#separate tetragrams into four columns
tetrgrams_separated <- corpus_tetrgrams %>%
  separate(tetrgram, c("word1", "word2", "word3","word4"), sep = " ")

#keep bigrams that four words are not stopwords
tetrgrams_filtered <- tetrgrams_separated %>%
  filter(!word1 %in% stop_words$word) 

tetrgrams_filtered2 <- tetrgrams_filtered %>%
  filter(str_detect(word1, "^[A-Z]|[0-9]"))




# new bigram counts:
tetrgrams_counts <- tetrgrams_filtered2 %>% 
  group_by(title)%>%
  count(word1, word2,word3,word4, sort = TRUE) %>%
  arrange(title) %>%
  top_n(10) %>%
  filter(n >1)

tetrgrams_counts


#recombine the four words into one bigram
tetrgrams_united <- tetrgrams_counts %>%
  unite(bigram, word1, word2,word3,word4,sep = " ")


write.csv(tetrgrams_united, "top-10-tetrgrams.csv")


#kwic of keyword "album"
library(quanteda)
album_kwic <-
  kwic(tbl_df$text, pattern = "album",window = 8, valuetype =  "fixed",
     case_insensitive = TRUE)

newname <- data_frame(gsub('text','',album_kwic$docname))
names(newname) <- "line"
newname <- left_join(newname,new_tbl_df, by = "line")
new_tbl_df <- tbl_df[,c("line","title")]

library(stringr)
album_kwic$docname<-
  as.integer(gsub('text','',album_kwic$docname))
album_kwic_result <-
  album_kwic %>% 
  left_join(new_tbl_df, 
            by =c("docname" = "line"), match_fun = str_detect) 
head(album_kwic_result)

album_kwic_result <- album_kwic_result[,c("title","from",
                                          "to","pre",
                                          "keyword","post")]

write.csv(album_kwic_result, "Album_8_context.csv")




