#This file is to generating untagged wordclouds for Hesslink_data
#Only the words version (do not include digits)
#It was using the tm_stopwords
#1, 15, 30 Topics 
hesselink_data <- readRDS("~/Documents/Hesselink copy2/R object for Hesselink Dataset/hesselink_data.rds")
user.comment.v <- as.character(hesselink_data$User.Comment)
length(user.comment.v) #87 Comments in total
user.name <- as.character(hesselink_data$User)


chunk.size <- 20 # number of words per chunk
library(quanteda)
library(qdap)

#A Function to handle the chunking to do pecentage-based by default
#And it can change to word-count-based segmentation
makeFlexTextChunks <- function(text.v, chunk.size=20, percentage=TRUE){
  words <- paste(text.v, collapse = " ")
  words.lower <- tolower(words)
  words.v <-strip(words.lower,
                  digit.remove = T, apostrophe.remove=F,lower.case=T)
  word.v <- tokens(words.v, what = "word", remove_symbols = TRUE,
                   remove_url = TRUE,
                   remove_punct = TRUE)
  word.v <- word.v[which(word.v!="")]
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

topic.m <- NULL
for(i in 1:length(user.comment.v)){
  comment <- user.comment.v[i]
  comment <- enc2utf8(comment)
  chunk.m <- makeFlexTextChunks(comment, chunk.size,
                                percentage=FALSE)
  #Delete file extensions
  username <- user.name[i]
  segments.m <- cbind(paste(username,
                            segment=1:nrow(chunk.m), sep="#"), chunk.m)
  topic.m <- rbind(topic.m, segments.m)
}
documents <- as.data.frame(topic.m, stringsAsFactors=F)
colnames(documents) <- c("id", "comment")

library(mallet)

#Step3: Simple topic modeling with a standard stop list
#First step to generate topic model: invoke mallet.import
mallet.instances <- mallet.import(documents$id,
                                  documents$comment,
                                  "tm_stopwords.csv",
                                  FALSE,
                                  token.regexp="[\\p{L}']+")

#Create only one topic
topic.model <- MalletLDA(num.topics=1)
#Fill in trainer object with textual data
topic.model$loadDocuments(mallet.instances)
#Access a list of the entire vocabulary of the corpus
vocabulary <- topic.model$getVocabulary()
length(vocabulary)
head(vocabulary)
vocabulary[1:50]

word.freqs <- mallet.word.freqs(topic.model)
head(word.freqs)
#Control the optimization interval and burn-in
topic.model$setAlphaOptimization(40, 80)
#Set the number of iterations in training
topic.model$train(400)

#Step 3: Unpack the model into a matrix
#That has each topic with their word type
topic.words.m <- mallet.topic.words(topic.model,
                                    smoothed=TRUE,
                                    normalized=TRUE)
dim(topic.words.m)
#1*910
rowSums(topic.words.m)
#Add the vocabulary column for each word type
colnames(topic.words.m) <- vocabulary

library(slam)
library(wordcloud)
#Create world cloud for each topic and save each
topic.top.words <- mallet.top.words(topic.model,
                                    topic.words.m[1,], 100)
#Get first three words in a topic
#topic.top.words$words[1:3]
set.seed(142)
png(paste("Topic_Modeling_Untagged/tm_stopwords_1_topic/topic1-",
          paste(topic.top.words$words[1],
                topic.top.words$words[2],
                topic.top.words$words[3],sep = "-"), ".png", sep = ""),
    width=12,height=8, units='in', res=300)
wordcloud(topic.top.words$words,
          topic.top.words$weights,
          c(4,.8), rot.per=0.2, random.order=F, 
          colors = rainbow(20))
dev.off()


#Create 15 topics
topic.model <- MalletLDA(num.topics=15)
#Fill in trainer object with textual data
topic.model$loadDocuments(mallet.instances)
#Access a list of the entire vocabulary of the corpus
vocabulary <- topic.model$getVocabulary()

#Access some basic info about frequency of words in the corpus
#And in the various documents of the corpus
word.freqs <- mallet.word.freqs(topic.model)
head(word.freqs)
#Control the optimization interval and burn-in
topic.model$setAlphaOptimization(40, 80)
#Set the number of iterations in training
topic.model$train(400)

#Step 3: Unpack the model into a matrix
#That has each topic with their word type
topic.words.m <- mallet.topic.words(topic.model,
                                    smoothed=TRUE,
                                    normalized=TRUE)
dim(topic.words.m)
#15 * 910
rowSums(topic.words.m)
#Add the vocabulary column for each word type
colnames(topic.words.m) <- vocabulary

#Generate 15 word clouds
for (i in 1: 15){
  #each topic is a row in topic.words.m
  topic.top.words <- mallet.top.words(topic.model,
                                      topic.words.m[i,], 80)
  #Produce a word cloud of top words in a topic
  set.seed(142)
  png(paste("Topic_Modeling_Untagged/tm_stopwords_15_topic/topic",
            i, "-",
            paste(topic.top.words$words[1],
                  topic.top.words$words[2],
                  topic.top.words$words[3],sep = "-"), ".png", sep = ""),
      width=12,height=8, units='in', res=300)
  wordcloud(topic.top.words$words,
            topic.top.words$weights,
            c(4,.8), rot.per=0.2, random.order=F, colors = rainbow(20))
  dev.off()
}


#Create 30 topics
topic.model <- MalletLDA(num.topics=30)
#Fill in trainer object with textual data
topic.model$loadDocuments(mallet.instances)
#Access a list of the entire vocabulary of the corpus
vocabulary <- topic.model$getVocabulary()

#Access some basic info about frequency of words in the corpus
#And in the various documents of the corpus
word.freqs <- mallet.word.freqs(topic.model)
head(word.freqs)
#Control the optimization interval and burn-in
topic.model$setAlphaOptimization(40, 80)
#Set the number of iterations in training
topic.model$train(400)

#Step 3: Unpack the model into a matrix
#That has each topic with their word type
topic.words.m <- mallet.topic.words(topic.model,
                                    smoothed=TRUE,
                                    normalized=TRUE)
dim(topic.words.m)
#30 * 910
rowSums(topic.words.m)
#Add the vocabulary column for each word type
colnames(topic.words.m) <- vocabulary

#Generate 15 word clouds
for (i in 1: 30){
  #each topic is a row in topic.words.m
  topic.top.words <- mallet.top.words(topic.model,
                                      topic.words.m[i,], 80)
  #Produce a word cloud of top words in a topic
  set.seed(142)
  png(paste("Topic_Modeling_Untagged/tm_stopwords_30_topic/topic",
            i, "-",
            paste(topic.top.words$words[1],
                  topic.top.words$words[2],
                  topic.top.words$words[3],sep = "-"), ".png", sep = ""),
      width=12,height=8, units='in', res=300)
  wordcloud(topic.top.words$words,
            topic.top.words$weights,
            c(4,.3), rot.per=0, random.order=F, colors = rainbow(20))
  dev.off()
}



