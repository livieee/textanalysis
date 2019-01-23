# Function to print a vector of file names in user
# friendly format
show.files <- function(file.name.v){
  for(i in 1:length(file.name.v)){
    cat(i, file.name.v[i], "\n", sep=" ")
  }
}


#Use custom stop list
library(tm)
#stopwords <- read.csv("Stoplist/Jockers-stoplist.csv",
#                      header = FALSE)
#stopwords <- as.character(stopwords$V1)


#8.2 A Word List Making
# Function takes a vector of file names and a directory path and
# returns a list in which each item in the list is an ordered
# vector of words from one of the files in the vector of file names
make.file.word.v.l <- function(files.v, input.dir){
  #set up an empty container
  text.word.vector.l <- list ()
  #loop over the files
  for (i in 1:length(files.v)) {
    # read the file from directory
    text.v <- scan(paste(input.dir, files.v[i], sep="/"), what="character", sep="\n")
    #convert to single string
    text.v <- enc2utf8(text.v)
    text.v <- paste(text.v, collapse = " ")
    #lowercase and split on non-word characters
    text.lower.v <- tolower(text.v)
    text.words.v <- strsplit(text.lower.v, "\\W")
    text.words.v <- unlist(text.words.v)
    #remove the blanks
    text.words.v <- text.words.v[which(text.words.v!="")]
    #use the index id from the files.v vector as the "name" in the list
    text.word.vector.l[[files.v[i]]] <- text.words.v
  }
  return(text.word.vector.l)
}

#A function to make raw frequency table for each text
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
    text.lower.v <- tolower(text.v)
    #remove stop list
    #text.lower.v <- removeWords(text.lower.v, stopwords("english"))
    text.lower.v <- gsub("[^[:alnum:][:space:]']", " ", text.lower.v)
    text.words.l <- strsplit(text.lower.v, "\\s+")
    text.words.v <- unlist(text.words.l)
    #remove the blanks
    text.words.v <- text.words.v[which(text.words.v!="")]
    #Make a raw frequency table
    text.freqs.t <- table(text.words.v)
    text.raws.l[[files.v[i]]] <- text.freqs.t
  }
  return(text.raws.l)
}

#Function to create table list for each file
getTEIWordTableList <- function(text.v){
  text.v <- paste(text.v, collapse = " ")
  #lowercase and split on non-word characters
  text.lower.v <- tolower(text.v)
  #text.lower.v <- removeWords(text.lower.v, stopwords("english"))
  text.words.v <- strsplit(text.lower.v, "\\W")
  text.words.v <- unlist(text.words.v)
  text.freqs.t <- table(text.words.v[which(text.words.v!="")])
  text.freqs.rel.t <- 100*(text.freqs.t/sum(text.freqs.t))
  return(text.freqs.rel.t)
}

#Function to remove blanks
removeBlanks <- function(x){
  x[which(x!="")]
}

#Function to get table list for each text segement
getTEIWordSegmentTableList <- function(text.v, chunk.size=10){
  text.v <- paste(text.v, collapse = " ")
  text.lower.v <- tolower(text.v)
  text.words.v <- strsplit(text.lower.v, "\\W")
  text.words.v <- unlist(text.words.v)
  max.length <- length(text.words.v)/chunk.size
  x <- seq_along(text.words.v)
  chunks.l <- split(text.words.v, ceiling(x/max.length))
  chunks.l <- lapply(chunks.l, removeBlanks)
  freq.chunks.l <- lapply(chunks.l, table)
  rel.freq.chunk.l <- lapply(freq.chunks.l, prop.table)
  return(rel.freq.chunk.l)
}

#Function to get table list for each text segement
#Changed to word-count-based segmentation, deal with contraction
getTEIWordSegmentTableListBetter <- function(text.v, chunk.size=10){
  words <- paste(text.v, collapse = " ")
  words.lower <- tolower(words)
  words.lower <- gsub("[^[:alnum:][:space:]']", " ", words.lower)
  words.l <- strsplit(words.lower, "\\s+")
  word.v <- unlist(words.l)
  x <- seq_along(word.v)
  max.length <- length(word.v)/chunk.size
  chunks.l <- split(word.v, ceiling(x/max.length))
  chunks.l <- lapply(chunks.l, removeBlanks)
  freq.chunks.l <- lapply(chunks.l, table)
  rel.freq.chunk.l <- lapply(freq.chunks.l, prop.table)
  return(rel.freq.chunk.l)
}



#A Function to handle the chunking to do pecentage-based by default
#And it can change to word-count-based segmentation
makeFlexTextChunks <- function(text.v, chunk.size=1000, percentage=TRUE){
  words <- paste(text.v, collapse = " ")
  words.lower <- tolower(words)
  words.lower <- gsub("[^[:alnum:][:space:]']", " ", words.lower)
  words.l <- strsplit(words.lower, "\\s+")
  word.v <- unlist(words.l)
  x <- seq_along(word.v)
  if(percentage){
    max.length <- length(word.v)/chunk.size
    chunks.l <- split(word.v, ceiling(x/max.length))
  } else {
    chunks.l <- split(word.v, ceiling(x/chunk.size))
    #deal with small chunks at the end
    if(length(chunks.l[[length(chunks.l)]]) <=
       length(chunks.l[[length(chunks.l)]])/2){
      chunks.l[[length(chunks.l)-1]] <-
        c(chunks.l[[length(chunks.l)-1]],
          chunks.l[[length(chunks.l)]])
      chunks.l[[length(chunks.l)]] <- NULL
    }
  }
  chunks.l <- lapply(chunks.l, paste, collapse=" ")
  chunks.df <- do.call(rbind, chunks.l)
  return(chunks.df)
}


#Function to make text chunks from tagged files (only get Nouns)
makeFlexTextChunksFromTagged <- function(tagged.text,
                                         chunk.size=500, percentage=TRUE){
  tagged.words <- splitText(tagged.text)
  tagged.words.keep <- c(selectTaggedWords(tagged.words,"/NN$"))
  words <- removeTags(tagged.words.keep)
  words.lower <- tolower(words)
  word.v <- gsub("[^[:alnum:][:space:]']", "", words.lower)
  x <- seq_along(word.v)
  if(percentage){
    max.length <- length(word.v)/chunk.size
    chunks.l <- split(word.v, ceiling(x/max.length))
  } else {
    chunks.l <- split(word.v, ceiling(x/chunk.size))
    if(length(chunks.l[[length(chunks.l)]]) <=
       length(chunks.l[[length(chunks.l)]])/2){
      chunks.l[[length(chunks.l)-1]] <-
        c(chunks.l[[length(chunks.l)-1]], chunks.l[[length(chunks.l)]])
      chunks.l[[length(chunks.l)]] <- NULL
    }
  }
  chunks.l <- lapply(chunks.l, paste, collapse=" ")
  chunks.df <- do.call(rbind, chunks.l)
  return(chunks.df)
}

#Function to make text chunks from tagged files (only get Adjective words)
makeAdjFlexTextChunksFromTagged <- function(tagged.text,
                                         chunk.size=500, percentage=TRUE){
  tagged.words <- splitText(tagged.text)
  tagged.words.keep <- c(selectTaggedWords(tagged.words,"/JJ$|/JJR$|/JJS$"))
  words <- removeTags(tagged.words.keep)
  words.lower <- tolower(words)
  word.v <- gsub("[^[:alnum:][:space:]']", "", words.lower)
  x <- seq_along(word.v)
  if(percentage){
    max.length <- length(word.v)/chunk.size
    chunks.l <- split(word.v, ceiling(x/max.length))
  } else {
    chunks.l <- split(word.v, ceiling(x/chunk.size))
    if(length(chunks.l[[length(chunks.l)]]) <=
       length(chunks.l[[length(chunks.l)]])/2){
      chunks.l[[length(chunks.l)-1]] <-
        c(chunks.l[[length(chunks.l)-1]], chunks.l[[length(chunks.l)]])
      chunks.l[[length(chunks.l)]] <- NULL
    }
  }
  chunks.l <- lapply(chunks.l, paste, collapse=" ")
  chunks.df <- do.call(rbind, chunks.l)
  return(chunks.df)
}


#Function to make text chunks from tagged files (only get Verbs)
makeVerbFlexTextChunksFromTagged <- function(tagged.text,
                                            chunk.size=500, percentage=TRUE){
  tagged.words <- splitText(tagged.text)
  tagged.words.keep <- c(selectTaggedWords(tagged.words,
                                           "/VB$|/VBD$|/VBG$|/VBN$|/VBP$/VBZ$"))
  words <- removeTags(tagged.words.keep)
  words.lower <- tolower(words)
  word.v <- gsub("[^[:alnum:][:space:]']", "", words.lower)
  x <- seq_along(word.v)
  if(percentage){
    max.length <- length(word.v)/chunk.size
    chunks.l <- split(word.v, ceiling(x/max.length))
  } else {
    chunks.l <- split(word.v, ceiling(x/chunk.size))
    if(length(chunks.l[[length(chunks.l)]]) <=
       length(chunks.l[[length(chunks.l)]])/2){
      chunks.l[[length(chunks.l)-1]] <-
        c(chunks.l[[length(chunks.l)-1]], chunks.l[[length(chunks.l)]])
      chunks.l[[length(chunks.l)]] <- NULL
    }
  }
  chunks.l <- lapply(chunks.l, paste, collapse=" ")
  chunks.df <- do.call(rbind, chunks.l)
  return(chunks.df)
}

