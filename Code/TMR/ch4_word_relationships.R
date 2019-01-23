#Replicate Chapter 4 of Text Mining With R
#n-grams and correlations
library(dplyr)
library(tidytext)


library(tidyverse)
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
                token = "ngrams", n = 2) 


corpus_bigrams 

#examine the  most common bigrams
corpus_bigrams %>%
  count(bigram, sort = TRUE)


library(tidyr)
#separate bigram into two columns: word1, word2
bigrams_separated <- corpus_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#keep bigrams that two words are not stopwords
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

#recombine the two words into one bigram
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united


#find most common trigrams
tbl_df %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)


#Find the most common "polymeter" in each text
bigrams_filtered %>%
  filter(word2 == "polymeter") %>%
  count(title, word1, sort = TRUE)

#Find the most common "rhythm" in each text
bigrams_filtered %>%
  filter(word2 == "rhythm") %>%
  count(title, word1, sort = TRUE)


#Find tf-idf values of bigram in each text
bigram_tf_idf <- bigrams_united %>%
  count(title, bigram) %>%
  bind_tf_idf(bigram, title, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

#plot them
# The 12 bigrams with the highest tf-idf 
#from each text of corpus
library(ggplot2)

 plots <-
  bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  group_by(title) %>%
  top_n(12, tf_idf) %>%
  #ungroup() %>%
  mutate(bigram = factor(reorder(bigram, tf_idf))) %>%
  do(plots = 
       ggplot(data=.) +
     (aes(bigram, tf_idf, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ title, ncol = 2, scales = "free") +
  coord_flip() +
  labs(y = "tf-idf of bigram to text",
       x = ""))
 pdf("text-specific-high-tf-idf-bigrams.pdf")
 plots$plots
 dev.off()
 



#4.1.3 
# check how often words are preceded by a word like "not"
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

#use the AFINN lexcion for sentiment analysis
#receive a numeric sentiment score for each word 
AFINN <- get_sentiments("afinn")

AFINN

#examine the most frequent words that were 
#preceded by "not" and were associated with a sentiment
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

not_words


#see which word contribute the most in the "wrong" direction
not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

#examine sentiment words preceeded by four common negated words
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()

#also visualize the most contribution direction words
negated_words %>%
  mutate(contribution = n * score,
         word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
  group_by(word1) %>%
  top_n(12, abs(contribution)) %>%
  ggplot(aes(word2, contribution, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free") +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Words preceded by negation term") +
  ylab("Sentiment score * # of occurrences") +
  coord_flip()


#4.1.4 connection graph
library(igraph)

# original counts
bigram_counts

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 100) %>%
  graph_from_data_frame()

bigram_graph


#Visualize, add three layers: nodes, edges, and text
#Common bigrams in Pride and Prejudic
#occurred more than 20 times and where neither word was a stop-word
library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#generate a better graph
set.seed(2016)

#add directionality with an arrow
png(paste("network-graph-common-bigrams-in-corpus.png"), 
    width=12,height=8, units='in', res=300)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
dev.off()


#make bigram count and visualization a function
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

#now we could use this function in other text dataset
library(stringr)

kjv_bigrams <- kjv %>%
  count_bigrams()

# filter out rare combinations, as well as digits
bigram_counts %>%
  filter(n > 100,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()


#4.2
#Divide a text into 10-line sections
#Find what words tend to appear witin the same section
corpus_section_words <- tbl_df %>%
  filter(title == "Sargeant_1975_Jazz_hot_and") %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  mutate(section = row_number() %/% 150) %>%
  filter(section > 0) 

corpus_section_words

library(widyr)
# count words co-occuring within same section
word_pairs <- corpus_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs

#find the words that most often occur with Darcy
word_pairs %>%
  filter(item1 == "music")


#find phi cofficient between words based on how often
#they appear in the same section

# we need to filter for at least relatively common words first
word_cors <- corpus_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors


#find the words most correlated with a word like "pounds"
word_cors %>%
  filter(item1 == "jazz")

#find the other words most associated with interesting words
word_cors %>%
  filter(item1 %in% c("jazz", "music", "rhythm")) %>%
  group_by(item1) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


#visualize the correlations and clusters of words
# Pairs of words in Pride and Prejudice 
#that show at least a .4 correlation of appearing 
#within the same 10-line section
set.seed(2016)
png(paste("ch4-Sargeant_1975-correlation-network-wordpairs.png"), 
    width=12,height=8, units='in', res=300)
word_cors %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
dev.off()


#Repeat 4.2 for whole corpus
#Divide a text into several sections, each has 150 tokens
#Find what words tend to appear witin the same section
corpus_section_words <- tbl_df %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  mutate(section = row_number() %/% 150) %>%
  filter(section > 0) 

corpus_section_words

library(widyr)
# count words co-occuring within same section
word_pairs <- corpus_section_words %>%
  filter(!str_detect(word, "\\d")) %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs

#find the words that most often occur with rhythm
word_pairs %>%
  filter(item1 == "rhythm")


#find phi cofficient between words based on how often
#they appear in the same section

# we need to filter for at least relatively common words first
word_cors <- corpus_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors


#find the words most correlated with a word like "pounds"
word_cors %>%
  filter(item1 == "music")

#find the other words most associated with interesting words
word_cors %>%
  filter(item1 %in% c("multiplicity", "music", "rhythm")) %>%
  group_by(item1) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


#visualize the correlations and clusters of words
# Pairs of words in Pride and Prejudice 
#that show at least a .4 correlation of appearing 
#within the same 10-line section
set.seed(2016)
png(paste("ch4-corpus-correlation-network-wordpairs.png"), 
    width=12,height=8, units='in', res=300)
word_cors %>%
  filter(correlation > .7, 
         !str_detect(item1, "\\d"),
         !str_detect(item2, "\\d")) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
dev.off()

