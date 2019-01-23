#Check overlap Captialized words with people's name from NER
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

# In order to turn it into a tidy text dataset
#we first need to put it into a data frame
library(dplyr)
tbl_df <- data_frame(line = 1:165, text = tbl$text)
tbl_df$title <- title
tbl_df$author <- authors1
tbl_df

#Work our corpus as a tidy dataset
#restructure it in the one-token-per-row format
library(tidytext)
tidy_corpus <- tbl_df %>%
  unnest_tokens(word, text, 
                to_lower = F) %>%
  filter(!str_detect(word, "\\d"))

data(stop_words)
#do not remove stopwords
tidy_corpus <- tidy_corpus %>%
  anti_join(stop_words) 


tidy_startwithCapital_corpus <- tidy_corpus %>%
  filter(grepl("^[A-Z]", word))

#cap words only from one text
cap_words <- tidy_startwithCapital_corpus %>%
  filter (title == "Wilson_1980_Ragtime_its_roots")

#extract people's name from NER
library(rJava)
text.v <- scan("Wilson_1980_Ragtime_its_roots.txt", what="character", sep="\n")
text.v <- enc2utf8(text.v)
text.v <- paste(text.v, collapse = " ")

#make word annotation
library(NLP);
library(openNLP);
library(RWeka)
library(openNLPmodels.en);
library(magrittr)
text.v <- as.String(text.v)
word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()
text_annotations <- annotate(text.v, list(sent_ann, word_ann))

class(text_annotations)
head(text_annotations)
text_doc <- AnnotatedPlainTextDocument(text.v, text_annotations)


#annotate people and place
person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")

pipeline <- list(sent_ann,
                 word_ann,
                 person_ann)
text_annotations <- annotate(text.v, pipeline)
text_doc <- AnnotatedPlainTextDocument(text.v, text_annotations)

# Extract entities from an AnnotatedPlainTextDocument
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

person <- entities(text_doc, kind = "person")
#common items in both people and cap words
#Note: people contains many name as a full name format
overlap <- intersect(cap_words$word,person )
write.csv(overlap, "overlap.csv")

library(magrittr)
library(ggplot2)
cap_words <- cap_words%>%
  count(title, word)
overlap <-
  person[grepl(paste(cap_words$word, collapse = "|"), person)]
overlap.table <- sort(table(overlap), decreasing = T)
overlap.trans.table <- as.data.frame(transform(overlap.table))
write.csv(overlap.trans.table, "overlap.csv")

library(ggplot2)
ggplot(data =overlap.trans.table[1:15, 1:2], 
       aes(x = reorder(overlap, -Freq), y = Freq)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Freq), hjust = 0.5, vjust = 1,
            colour = "white", fontface = "bold")+
  labs(x = "Top 15 People's Item \n", y= "Count \n", title ="Wilson_1980_Ragtime_its_roots" ) +
  theme(axis.text.x=element_text(angle=45, hjust=1.2))

