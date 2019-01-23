#This code is to pre-process the Sample Corpus by using tidy approach
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


#You can extract more metadata from "Metadata" Spreadsheet
#For example:add styles label to each text
library(readxl)
metaStyle <- read_excel("~/Documents/Final Report/Reference Files/Sample Corpus Metadata.xlsx", 
                        sheet = "Metadata (Analysis)")
metaStyle.df <- as.data.frame(cbind(metaStyle[2],metaStyle$`Style,Primary`))


colnames(metaStyle.df) <- c("title", "style")


#In order to turn it into a tidy text dataset
#we first need to put it into a data frame
library(dplyr)
tbl_df <- data_frame(line = 1:165, text = tbl$text)


#Store metadata into tbl_df
tbl_df$title <- title
tbl_df$author <- authors_year
tbl_df

#Add extra metadata column if you want
tbl_df <- left_join(tbl_df, metaStyle.df, 
                    by = "title")

#pre-preocess
#keep apostrophe
tbl_df$text <- tbl_df$text %>%
  iconv(., "ASCII", "UTF-8", sub="") %>%
  tolower(.) %>%
  gsub("[^[:alnum:][:space:]']", " ", .)

#tokenize the text 
#you can customize the tokenization function: unnest_tokens
library(tidytext)
tidy_corpus <- tbl_df %>%
  unnest_tokens(word, text, 
                token = stringr::str_split, pattern = "\\s+") 


#remove stop words
#note: here you can import any custom stop_words
data(stop_words)
tidy_corpus <- tidy_corpus %>%
  anti_join(stop_words)
