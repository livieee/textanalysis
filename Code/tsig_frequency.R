#this code is created on July 16, 2018
#for time signature frequency analysis

#import Short ID Sample Corpus
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

#check table information
tbl[3,1]
#for each text, store author, title information
tbl$author <- authors1
tbl$title <- title
tbl

#take the text of the book in a tidy format(tokenized)
#keep track of which text each word comes from
library(stringr)


# In order to turn it into a tidy text dataset
#we first need to put it into a data frame
library(dplyr)
library(quanteda)
library(magrittr)
library(qdap)
library(tidytext)
tbl_df <- data_frame(line = 1:165, text = tbl$text)
tbl_df$title <- title
tbl_df$author <- authors1
tbl_df


# tbl_df$tsig <- tbl_df$text %>%
#   iconv(., "ASCII", "UTF-8", sub="") %>%
#   #enc2utf8(tbl_df$text) %>%
#   #{paste(., collapse = " ")} %>%
#   tolower(.) %>%
#   str_match_all(., "([0-9]{1,2}[/][0-9]{1,2}[\\s\\,\\-\\|]{0,}){1,}")
#   

#Tokenize into single word 
tidy_general <- tbl_df %>%
  unnest_tokens(word, text) %>%
  filter(!str_detect(word, "\\d")) 
tidy_general



#Updated Extract Pattern With Time Signature
tsig_pattern <-"([0-9]{1,2}[/][0-9]{1,2}[\\s\\,\\-\\|]{0,}){1,}"

time.sig <- 
  str_match_all(
    unlist(tbl_df$text), 
    tsig_pattern)
time.sig.list <- list()
for (i in 1:165){
  time.sig.list[[i]] <- time.sig[[i]][,1]
}
x<- time.sig.list[[165]]

time.sig.df <- data_frame(line = 1:165, tsig = time.sig.list)
time.sig.df <- time.sig.df %>%
  group_by(line) %>%
  unnest(time.sig.list)

tbl_info <- tbl_df[,c(1,3,4)]
tidy_tsig <- full_join(tbl_info,time.sig.df) %>%
  rename(word = time.sig.list) %>%
  na.omit()

  

# #Extract Pattern With Time Signature
#This code has some bug
# tsig_pattern <-"([0-9]{1,2}[/][0-9]{1,2}[\\s\\,\\-\\|]{0,}){1,}"
# #Make a tidy form of tsig
# library(tidytext)
# tidy_tsig <- tbl_df %>%
#   unnest_tokens(word, text, 
#                 token = stringr::str_match_all, pattern = tsig_pattern) 


#Extract Pattern With Numbers
number_pattern <- "([\\(]?[1-9]+[.]?[1-9]?[\\s\\,\\-\\:\\)\\+\\=]{1,}){2,}"
tidy_number <- tbl_df %>%
  unnest_tokens(word, text, 
                token = stringr::str_match_all, pattern = number_pattern)

number_counts <- tidy_number %>%
  count(word, sort = TRUE) %>%
  ungroup()
  



#combine general tokens and tsig tokens
tidy_corpus <- full_join(tidy_general, tidy_tsig)


#remove stop words
data(stop_words)
tidy_corpus <- tidy_corpus %>%
  anti_join(stop_words)


text_words <- tidy_corpus %>%
  count(title, word, sort = TRUE) %>%
  ungroup()


text_counts <- tidy_corpus %>%
  count(title,word, sort = TRUE) %>%
  group_by(title) %>%
  mutate(freq = n/sum(n)) %>%
  rename(count = n) %>%
  arrange(title)

#list of Tsig and freq in each text
text_time_sig <-
  text_counts %>%
  filter(word %in% tidy_tsig$word)

#Write result to existing workbook
library(xlsx)
library(openxlsx)

wb <- loadWorkbook("TSig.xlsx")

# Add some sheets to the workbook
addWorksheet(wb, "Text-specific TSig2")

# Write the data to the sheets
writeData(wb, sheet = "Text-specific TSig2", x = text_time_sig)
# Export the file
saveWorkbook(wb, "TSig.xlsx", overwrite = T)



#Stats of overall Tsig in each text
total_words <- text_counts %>% 
  group_by(title) %>% 
  summarize(totalwords = sum(count))

total_time_sig <- text_time_sig %>% 
  group_by(title) %>% 
  summarize(totalTsig = sum(count))

text_time_sig_overall <- left_join(total_time_sig, total_words) %>%
  mutate(freq = totalTsig/totalwords)

#write result
wb <- loadWorkbook("TSig.xlsx")

# Add some sheets to the workbook
addWorksheet(wb, "Text-specific TSig Stat2")

# Write the data to the sheets
writeData(wb, sheet = "Text-specific TSig Stat2", x = text_time_sig_overall)
# Export the file
saveWorkbook(wb, "TSig.xlsx", overwrite = T)


#corpus timesig
corpus_words <- tidy_corpus %>%
  count(word, sort = TRUE) %>%
  ungroup()


corpus_counts <- tidy_corpus %>%
  count(word, sort = TRUE) %>%
  mutate(freq = n/sum(n)) %>%
  rename(count = n)

#list of Tsig and freq in each text
corpus_time_sig <-
  corpus_counts %>%
  filter(word %in% tidy_tsig$word)

#Write result to existing workbook
library(xlsx)
library(openxlsx)

wb <- loadWorkbook("TSig.xlsx")

# Add some sheets to the workbook
addWorksheet(wb, "Corpus TSig2")

# Write the data to the sheets
writeData(wb, sheet = "Corpus TSig2", x = corpus_time_sig)
# Export the file
saveWorkbook(wb, "TSig.xlsx", overwrite = T)



#Stats of overall Tsig in each text
total_words <- corpus_counts %>% 
  summarize(totalwords = sum(count))

total_time_sig <- corpus_time_sig %>% 
  summarize(totalTsig = sum(count))

corpus_time_sig_overall <- cbind(total_time_sig, total_words) %>%
  mutate(freq = totalTsig/totalwords)

#write result
wb <- loadWorkbook("TSig.xlsx")

# Add some sheets to the workbook
addWorksheet(wb, "Corpus TSig Stat2")

# Write the data to the sheets
writeData(wb, sheet = "Corpus TSig Stat2", x = corpus_time_sig_overall)
# Export the file
saveWorkbook(wb, "TSig.xlsx", overwrite = T)




