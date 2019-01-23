input.dir <- "Short_ID_Sample Corpus_txt"
files.v <- dir(input.dir, ".*txt")
source("code/corpusFunctions.R")
title <- gsub("\\.*.txt$","", files.v)

#Use custom stop list
library(tm)
library(quanteda)
library(magrittr)
library(qdap)

# make.file.table.v.l <- function(files.v, input.dir){
#   #set up an empty container
#   text.raws.l <- list()
#   #loop over the files
#   for (i in 1:length(files.v)) {
#     # read the file from directory
#     text.v <- scan(paste(input.dir, files.v[i], sep="/"), what="character", sep="\n")
#     #convert to single string
#     text.v <- enc2utf8(text.v)
#     text.v <- paste(text.v, collapse = " ")
#     #lowercase and split on non-word characters
#     text.lower.v <- tolower(text.v)
#     #remove stop list
#     text.lower.v <- gsub("[^[:alnum:][:space:]']", " ", text.lower.v)
#     text.words.l <- strsplit(text.lower.v, "\\s+")
#     text.words.v <- unlist(text.words.l)
#     text.words.v <- removeWords(text.words.v, stopwords("english"))
#     #remove the blanks
#     text.words.v <- text.words.v[which(text.words.v!="")]
#     #Make a raw frequency table
#     text.freqs.t <- table(text.words.v)
#     text.raws.l[[files.v[i]]] <- text.freqs.t
#   }
#   return(text.raws.l)
# }

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
    text.lower.v <- removeWords(text.lower.v, stopwords("english"))
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

#Get the raw frequency table for each text in Corpus
my.corpus.raw.l <- make.file.table.v.l(files.v, input.dir)

#sort the table in descending order
sorted.text.freqs.t <- sort(my.corpus.raw.l[[99]] , decreasing=TRUE)
sorted.text.freqs.t[1:15]

#to get rate of decrease plot
plot(sorted.text.freqs.t[1:15], type = "b",
     main = as.character(title[99]),
     xlab = "Top 15 Words", ylab= "Word Count", xaxt = "n")
axis(1,1:15, labels=names(sorted.text.freqs.t[1:15]))

sorted.text.freqs.df <- as.data.frame(sorted.text.freqs.t)
names(sorted.text.freqs.df)

#same graph
library(ggplot2)
ggplot(data =sorted.text.freqs.df[1:15,1:2], 
            aes(x = reorder(text.words, -Freq), y = Freq)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Freq), hjust = 0.5, vjust = 1,
            colour = "white", fontface = "bold")+
  labs(x = "Top 15 words \n", y= "Count \n", title = as.character(title[99])) +
  theme(axis.text.x=element_text(angle=45, hjust=1.2))


#Generate most 25 frequent words for each text
library(rowr)
#Get the Hapax item
text.frequent.item.l <- list()
result <- NULL
for (i in 1:length(my.corpus.raw.l)){
  sorted.text.freqs.t <- sort(my.corpus.raw.l[[i]] , decreasing=TRUE)
  text.frequent.item.v <- names(sorted.text.freqs.t[1:25])
  text.frequent.item.l[[i]] <-text.frequent.item.v
  result <- cbind.fill(result, text.frequent.item.l[[i]], fill=NA)
}
result$init <- NULL
colnames(result) <- title

#Write result to existing workbook
library(xlsx)
library(openxlsx)

wb <- loadWorkbook("analysis/Analysis.xlsx")

# Add some sheets to the workbook
addWorksheet(wb, "25-frequent-words")

# Write the data to the sheets
writeData(wb, sheet = "25-frequent-words", x = result)
# Export the file
saveWorkbook(wb, "analysis/Analysis.xlsx", overwrite = T)



