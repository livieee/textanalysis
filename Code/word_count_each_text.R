#Generate word count for eath text in Short_ID_Sample Corpus_txt
#After pre-processing

input.dir <- "Short_ID_Sample Corpus_txt"
files.v <- dir(input.dir, ".*txt")
title <- gsub("\\.*.txt$","", files.v)

#A function to calculate word count for each text after
#Pre-processing
make.file.count.v.l <- function(files.v, input.dir){
  #set up an empty container
  text.word.count.l <- list()
  #loop over the files
  for (i in 1:length(files.v)) {
    # read the file from directory
    text.v <- scan(paste(input.dir, files.v[i], sep="/"), what="character", sep="\n")
    #convert to single string
    text.v <- enc2utf8(text.v)
    text.v <- paste(text.v, collapse = " ")
    #lowercase and split on non-word characters
    text.lower.v <- tolower(text.v)
    #remove stop list
    #text.lower.v <- removeWords(text.lower.v, stopwords("english"))
    text.lower.v <- gsub("[^[:alnum:][:space:]']", " ", text.lower.v)
    text.words.l <- strsplit(text.lower.v, "\\s+")
    text.words.v <- unlist(text.words.l)
    #remove the blanks
    text.words.v <- text.words.v[which(text.words.v!="")]
    #Make a raw frequency table
    word.count <- length(text.words.v)
    text.word.count.l[[files.v[i]]] <- word.count
  }
  return(text.word.count.l)
}

my.corpus.count.l <- make.file.count.v.l(files.v, input.dir)
my.corpus.count.tdf <- t(as.data.frame(my.corpus.count.l))
write.csv(my.corpus.count.tdf,"word-count.csv")

