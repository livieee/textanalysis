#Cleaner verson of text
#Apostrephe preserved
#Intro-word hyphen preserved
input.dir <- "Short_ID_Sample Corpus_txt"
files.v <- dir(input.dir, ".*txt")
source("code/corpusFunctions.R")
title = gsub("\\.*.txt$","", files.v)
library(tm)
library(quanteda)
library(magrittr)
library(qdap)
#Get the raw frequency table for each text in Corpus
make.file.table.v.l <- function(files.v, input.dir){
  #set up an empty container
  text.raws.l <- list()
  #loop over the files
  for (i in 1:length(files.v)) {
    # read the file from directory
    text.v <- scan(paste(input.dir, files.v[i], sep="/"), what="character", sep="\n")
    #convert to single string
    text.v <- enc2utf8(text.v)
    text.v <- paste(text.v, collapse = " ")
    #lowercase and split on non-word characters
    text.lower.v <- char_tolower(text.v)
    #remove hyphens not connecting words
    text.words.v <- gsub(" - ", " ", text.lower.v,perl = TRUE)
    #keep intro-word hyphen and apostrophe, alphabeta, digital
    text.words.v <-strip(text.words.v, char.keep=c("-","'"), digit.remove = F,apostrophe.remove=F,lower.case=T)
    text.words <-tokens(text.words.v, 
                        remove_punct =TRUE,
                        remove_symbols=TRUE) %>% as.character()
    #Make a raw frequency table
    text.freqs.t <- table(text.words)
    text.raws.l[[files.v[i]]] <- text.freqs.t
  }
  return(text.raws.l)
}
my.corpus.raw.l <- make.file.table.v.l(files.v, input.dir)

#Number of hapax in each text
text.hapax.v <- sapply(my.corpus.raw.l, function(x) sum(x == 1))

library(rowr)
#Get the Hapax item
text.hapax.item.l <- list()
result <- NULL
for (i in 1:length(my.corpus.raw.l)){
  text.hapax.item.v <- names(which(my.corpus.raw.l[[i]][] == "1"))
  text.hapax.item.l[[i]] <- text.hapax.item.v
  result <- cbind.fill(result, text.hapax.item.l[[i]], fill=NA)
}
result$init <- NULL
colnames(result) <- title
length(text.hapax.item.l[[165]])

#Write result to existing workbook
library(xlsx)
library(openxlsx)

wb <- loadWorkbook("analysis/Analysis.xlsx")

# Add some sheets to the workbook
addWorksheet(wb, "text-specific hapax2")

# Write the data to the sheets
writeData(wb, sheet = "text-specific hapax2", x = result)
# Export the file
saveWorkbook(wb, "analysis/Analysis.xlsx", overwrite = T)


#TRIAL
str <- "i love-you - 77-years old.but she doesn't, ‘hip hop'. 'And' 'Criss Cross'"
str1 <- removeWords(str, stopwords("english"))
str1 <- gsub(" - ", " ", str,perl = TRUE)
#str1 <- gsub("[^[:alnum:]['-]", " ", str)
str2 <-strip(str1, char.keep=c("-","'"), digit.remove = F,apostrophe.remove=F,lower.case=T)
str2 <- removeWords(str2, stopwords("english"))
words <- unlist(strsplit(str2, "//s+"))
words <- tokens(str2, remove_punct =TRUE,remove_symbols=TRUE) %>% as.character()

words <-  removeWords(words, stopwords("english"))
table(words)

