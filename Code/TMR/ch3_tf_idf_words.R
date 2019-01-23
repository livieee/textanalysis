#Chapter3
library(dplyr)
library(tidytext)

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


#Work our corpus as a tidy dataset
#restructure it in the one-token-per-row format
tbl_df$text <- tbl_df$text %>%
  iconv(., "ASCII", "UTF-8", sub="") %>%
  tolower(.) %>%
  gsub("[^[:alnum:][:space:]']", " ", .)
# strip(.,char.keep=c("/","'"),
#        digit.remove = F,
#        apostrophe.remove=F,lower.case=T)


library(tidytext)
tidy_corpus <- tbl_df %>%
  unnest_tokens(word, text, 
                token = stringr::str_split, pattern = "\\s+") 

data(stop_words)
tidy_corpus <- tidy_corpus %>%
  anti_join(stop_words)


corpus_words <- tidy_corpus %>%
  count(title, word, sort = TRUE) %>%
  ungroup()

total_text_words <- corpus_words %>% 
  group_by(title) %>% 
  summarize(total = sum(n))

corpus_words <- left_join(corpus_words, total_text_words)
corpus_words
arranged_corpus_words <- corpus_words %>%
  arrange(title)
#randomly sample 10 texts in the corpus
sample_words <- arranged_corpus_words %>% 
  filter(title %in% sample(factor(title),10))

length(unique(sample_words$title))


#Term Frequency Distribution in Corpus
library(ggplot2)

ggplot(sample_words, aes(n/total, fill = title)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~title, ncol = 2, scales = "free_y")


#Examine Zipf's law for Sample Corpus
library(dplyr)
freq_by_rank <- corpus_words %>% 
  group_by(title) %>% 
  dplyr::mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank

#plot Zipf's law for Corpus
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = title)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()


#see the exponent of the power law is for the middle secion
#of the rank range
rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)
lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

#plot the fitting exponent of Zipf's law
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = title)) + 
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()


#get tf_idf 
corpus_words <- corpus_words %>%
  bind_tf_idf(word, title, n)
corpus_words


#look at high tf-idf terms in J.A's work
corpus_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

#visualization of these high tf-idf terms
plots <- 
  corpus_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(title) %>% 
  top_n(15) %>% 
  #ungroup %>%
  do(plots = 
  ggplot(data=.) +
  aes(word, tf_idf, fill = title) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~title, ncol = 2, scales = "free") +
  coord_flip())

#save it
# library(gridExtra)
# n <- length(plots$plots)
# nCol <- floor(sqrt(n))
# do.call("grid.arrange", c(plots$plots, ncol=3, nrow=3))
# 
# dev.off()
# grid.arrange(grobs=plots$plots, ncol=2)
# ggsave(file = "text-specific-high-tf-idf-terms.pdf",
#        arrangeGrob(grobs = plots, ncol=2))
# 
# pdf("text-specific-high-tf-idf-terms-test.pdf")
# cowplot::plot_grid(plotlist = plots$plots, ncol = 3)
pdf("text-specific-high-tf-idf-terms.pdf")
plots$plots
dev.off()


#Add styles label to each text
library(readxl)
metaStyle <- read_excel("analysis/Analysis.xlsx", 
                        sheet = "Metadata (Analysis)")
metaStyle.df <- as.data.frame(cbind(metaStyle[2],metaStyle$`Style,Primary`))


colnames(metaStyle.df) <- c("title", "style")
tidy_style_corpus <- left_join(tidy_corpus, metaStyle.df, 
                           by = "title")
style_corpus_words <- tidy_style_corpus %>%
  count(style, word, sort = TRUE) %>%
  ungroup()

total_style_words <- style_corpus_words %>% 
  group_by(style) %>% 
  summarize(total = sum(n))

style_corpus_words <- 
  left_join(style_corpus_words, total_style_words)
  
#get tf_idf 
style_corpus_words <- style_corpus_words %>%
  bind_tf_idf(word, style, n)




# #how many times each word was used in each text
# library(data.table)
# waters_words <- tidy_corpus %>% 
#   filter(author %like% "Waters") %>%
#   mutate(author = "Waters")
# 
# vuust_words <-  tidy_corpus %>% 
#   filter(author %like% "Vuust") %>%
#   mutate(author = "Vuust")
# 
# morgenstein_words <- tidy_corpus %>% 
#   filter(author %like% "Morgenstein") %>%
#   mutate(author = "Morgenstein")
# 
# corpus_author_words <- bind_rows(waters_words,
#                                  vuust_words,
#                                  morgenstein_words) %>%
#   count(author,word, sort = TRUE) %>%
#   ungroup()

#Highest tf-idf words in each style
plot_style_words <- style_corpus_words %>%
  bind_tf_idf(word, style, n) %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

png("style-15-highest-tf-idf-terms.png",
    width=12,height=8, units='in', res=300)

plot_style_words %>% 
  group_by(style) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = style)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~style, ncol = 2, scales = "free") +
  coord_flip()

dev.off()

#Explore interesting tf-idf terms:
library(stringr)

tbl_style_df <- left_join(tbl_df, metaStyle.df, 
                          by = "title")

tbl_style_df %>% 
  filter(str_detect(text, "gtr")) %>% 
  select(text)


tbl_style_dft %>% 
  filter(str_detect(text, "K1")) %>% 
  select(text)

tbl_style_dft %>% 
  filter(str_detect(text, "AK")) %>% 
  select(text)

#remove these less meaningful words
#into stopwords
mystopwords <- data_frame(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                                   "fig", "file", "cg", "cb", "cm"))
style_corpus_words
<- anti_join(style_corpus_words, mystopwords, by = "word")

#Highest tf-idf words in classic physics texts
#Of different authors
plot_style_words <- style_corpus_words %>%
  bind_tf_idf(word, style, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(style) %>% 
  top_n(15, tf_idf) %>%
  ungroup

ggplot(plot_style_words, aes(word, tf_idf, fill = style)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~style, ncol = 2, scales = "free") +
  coord_flip()
