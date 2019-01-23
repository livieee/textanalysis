#Replicate chapter 1 for Text Mining With R
#The tidy text format
library(tidyverse)


#Load text file from Sample Corpus
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


#tokenize the text 
#you can customize the tokenization function: unnest_tokens
library(tidytext)
tidy_corpus <- tbl_df %>%
  unnest_tokens(word, text) 
tidy_corpus

#remove stop words
#note: here you can import any custom stop_words
data(stop_words)
tidy_corpus <- tidy_corpus %>%
  anti_join(stop_words)

#find the most common words across the corpus
tidy_corpus %>%
  count(word, sort = TRUE) %>%
  top_n(25)


#plot most common words across the corpus
library(ggplot2)
tidy_corpus %>%
  count(word, sort = TRUE) %>%
  top_n(25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


#top 25 most common words in each text
tidy_corpus %>%
  group_by(title) %>%
  count(word, sort = TRUE) %>%
  top_n(25)


#Visualize 
set.seed(123)
tidy_corpus %>%
  group_by(title) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(title = title,
         text_order = nrow(.):1) %>%
  ggplot(aes(reorder(word, text_order), n, fill = title)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ title, scales = "free_y") +
  labs(x = "NULL", y = "Frequency") +
  coord_flip() +
  theme(legend.position="none")
  


# calculate percent of word use across corpus
corpus_pct <- tidy_corpus %>%
  count(word) %>%
  transmute(word, all_words = n / sum(n))

# calculate percent of word use within each text
frequency <- tidy_corpus %>%
  count(title, word) %>%
  mutate(text_words = n / sum(n)) %>%
  left_join(corpus_pct) %>%
  arrange(desc(text_words)) %>%
  ungroup()

frequency

ggplot(frequency, aes(x = text_words, y = all_words, color = abs(all_words - text_words))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~ title, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Sample Corpus", x = NULL)



#quantify how similar and different these sets of word freqs
#are using a correlation test
#How correlated are the word frequencies between the corpus and 
#each book
word.freqs.p<-
  frequency %>%
  group_by(title) %>%
  summarize(correlation = cor(text_words, all_words),
            p_value = cor.test(text_words, all_words)$p.value)
View(word.freqs.p)
write.csv(word.freqs.p, "correlation-word-freqs-between-corpus-and-each-text.csv")


#compare small set of texts from different authors
#Browns
browns <- tbl_df[c(12,13),]

tidy_browns <- browns %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_browns %>%
  count(word, sort = TRUE)

#Butlers
butler <- tbl_df[c(15:18),]
tidy_butler <- butler %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_butler %>%
  count(word, sort = TRUE)


#Magadini
magadini <- tbl_df[c(83:85),]
tidy_magadini <- magadini %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_magadini %>%
  count(word, sort = TRUE)


library(tidyr)

frequency <- bind_rows(mutate(tidy_browns, author = "Brown"),
                       mutate(tidy_butler, author = "Butler"), 
                       mutate(tidy_magadini, author = "Magadini")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Brown`:`Butler`)


library(scales)

# expect a warning about rows with missing values being removed
png(paste("~/Documents/Sample Corpus/Comparing.png"), 
    width=12,height=8, units='in', res=300)
ggplot(frequency, aes(x = proportion, y = `Magadini`, color = abs(`Magadini` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Magadini", x = NULL)
dev.off()

#see how similar and different these sets of word frequencies are
#using a correlation test
cor.test(data = frequency[frequency$author == "Brown",],
         ~ proportion + `Magadini`)


cor.test(data = frequency[frequency$author == "Butler",], 
         ~ proportion + `Magadini`)

