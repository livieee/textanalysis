#Replicate Chapter 6 Topic modeling
library(topicmodels)

#load dataset
#load corpus into tm DTM format
library(tm)
input.dir <- ("~/Documents/Sample Corpus/Short_ID_Sample Corpus_txt")
corpus  <-Corpus(DirSource(input.dir), readerControl = list(language="english")) #specifies the exact folder where my text file(s) is for analysis with tm.
summary(corpus)  #check what went in
#corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus , stripWhitespace)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("english")) # this stopword file is at C:\Users\[username]\Documents\R\win-library\2.13\tm\stopwords 
#corpus <- tm_map(corpus, stemDocument, language = "english")
corpus_dtm <-DocumentTermMatrix(corpus) 

# set a seed so that the output of the model is predictable
#two-topic model
corpus_lda <- LDA(corpus_dtm, k = 5, control = list(seed = 1234))
corpus_lda

#extract the per-topic-per-word probabilities, called("beta")
library(tidytext)

corpus_topics <- tidy(corpus_lda, matrix = "beta")
corpus_topics


#find the 10 terms that are most common within each topic
library(ggplot2)
library(dplyr)

corpus_top_terms <- corpus_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

corpus_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#consider common terms that have the greatest difference
#in beta between two topics
library(tidyr)

beta_spread <- corpus_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

#words with greatest difference between tow topics
beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()

#Examine the per-document-per-topic probabilities
corpus_documents <- tidy(corpus_lda, matrix = "gamma")
result <- corpus_documents %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  arrange(document)
write.csv(result, "corpus-5-topics-per-document-per-topic-prob.csv")




#6.2 re-cluster disorganized random_corpus into 4 groups
#load corpus
library(dplyr)
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
random_tbl <- sample_n(tbl, 4)


# divide into documents, each representing one section
by_section <- random_tbl %>%
  unnest_tokens(word, text) %>%
  mutate(section = row_number() %/% 150) %>%
  filter(section > 0) %>%
  unite(document, title, section, sep = "#")

# find document-word counts
word_counts <- by_section %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts

#6.2.1 LDA on random_corpus
#convert word_counts tidy form into dtm
random_corpus_dtm <- word_counts %>%
  cast_dtm(document, word, n)

random_corpus_dtm

#create a four-topic model
random_corpus_lda <- LDA(random_corpus_dtm, k = 4, control = list(seed = 1234))
random_corpus_lda

#examine per-topic-per-word probabilities
random_corpus_topics <- tidy(random_corpus_lda, matrix = "beta")
random_corpus_topics

#find the top 10 terms within each topic.
top_terms <- random_corpus_topics %>%
  group_by(topic) %>%
  filter (!str_detect(term, "\\d")) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

#visualize top_terms in each topic
library(ggplot2)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


#6.2.2 Per-document classification
#examining the per-document-per-topic probabilities
random_corpus_gamma <- tidy(random_corpus_lda, matrix = "gamma")
random_corpus_gamma

#re-separate the document name into chapter and title
#visualize the per-document-per-topic probability for each
random_corpus_gamma <- random_corpus_gamma %>%
  separate(document, c("title", "section"), sep = "#", convert = TRUE)

random_corpus_gamma

# reorder titles in order of topic 1, topic 2, etc before plotting
random_corpus_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)


#find the topic that was most associated with each section
section_classifications <- random_corpus_gamma %>%
  group_by(title, section) %>%
  top_n(1, gamma) %>%
  ungroup()

section_classifications

# compare each to the most common topic among its random_corpus
#for each book
text_topics <- section_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

#find a chapter topic that is not the "consensus"
# topic in its book
section_classifications %>%
  inner_join(text_topics, by = "topic") %>%
  filter(title != consensus)

#6.2.3 assign each word in each document to a topic
assignments <- augment(random_corpus_lda, data = random_corpus_dtm)
assignments

#find incorrectly classified words 
assignments <- assignments %>%
  separate(document, c("title", "section"), sep = "#", convert = TRUE) %>%
  inner_join(text_topics, by = c(".topic" = "topic"))

assignments

#visualize it
library(scales)
library(ggplot2)
assignments %>%
  count(title, consensus, wt = count) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "text words were assigned to",
       y = "text words came from",
       fill = "% of assignments")


#find the most commonly mistaken words
wrong_words <- assignments %>%
  filter(title != consensus)
wrong_words %>%
  group_by(title) %>%
  count(term, sort = T)

wrong_words
write.csv(wrong_words, "missclassified_words.csv")

wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))

#Explore misclassified words distribution in each book
word_counts %>%
  filter(word == "metric")


#6.3 mallet implementation 
library(dplyr)
library(tidytext)
library(stringr)

library(ggplot2)
theme_set(theme_light())
library(mallet)

# create a vector with one string per chapter
collapsed <- by_section %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_replace(word, "'", "")) %>%
  group_by(document) %>%
  summarize(text = paste(word, collapse = " "))

# create an empty file of "stopwords"
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$document, collapsed$text, empty_file)

mallet_model <- MalletLDA(num.topics = 4)
mallet_model$loadDocuments(docs)
mallet_model$train(100)

# word-topic pairs
tidy(mallet_model)

# document-topic pairs
tidy(mallet_model, matrix = "gamma")

# column needs to be named "term" for "augment"
term_counts <- rename(word_counts, term = word)
augment(mallet_model, term_counts)
