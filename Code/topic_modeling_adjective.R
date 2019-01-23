#This is for topic modeling by using Jocker's stopilist
#Get Adjective Word Clouds
setwd("~/Documents/Sample Corpus")
input.dir <- "Short_ID_Sample Corpus_txt"
files.v <- dir(path=input.dir, pattern=".*txt")
source("code/corpusFunctions.R")


#Pre-processing with a POS Tagger
#Code for tag our files
#Don't need to run here
library(openNLP)
library(NLP)
for(i in 1:length(files.v)){
  text.v <- scan(paste(input.dir, files.v[i], sep="/"), what="character", sep="\n")
  text.v <- enc2utf8(text.v)
  words <- as.String(paste(text.v, collapse = " "))
  # Need sentence and word tokens first
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- annotate(words, list(sent_token_annotator,
                             word_token_annotator))
  # now pos tags
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  a3 <- annotate(words, pos_tag_annotator, a2)
  a3w <- subset(a3, type == "word")
  tags <- sapply(a3w$features, `[[`, "POS")
  tagged_text <- paste(sprintf("%s/%s", words[a3w], tags),
                       collapse=" ")
  write(tagged_text, paste("taggedShortIDCorpus/",
                           files.v[i], sep=""))
}


#Modified text chunking for tagged corpus
#Chunk size = 500
inputDir <- "taggedShortIDCorpus"
files.v <- dir(path=inputDir, pattern=".*txt")
source("code/corpusFunctions.R")
chunk.size <- 500


#Function to split the tagged file into a vector
#Each value is a single word/POS pair
splitText <- function(text) {
  unlist(strsplit(text," "))
}

#Function to walk through word/POS pair and pick certain POS markers
selectTaggedWords <- function(tagged.words, target.tag) {
  tagged.words[grep(target.tag, tagged.words)]
}

#Function to strip off POS markers
removeTags <- function(word.pos) {
  sub("/[A-Z]{2,3}", "", word.pos)
}


#Build topic data frame from each file
#Only keep adjective words
topic.m <- NULL
for(i in 1:length(files.v)){
  tagged.text <- scan(file.path(inputDir, files.v[i]),
                      what="character", sep="\n")
  chunk.m <- makeAdjFlexTextChunksFromTagged(tagged.text,
                                          chunk.size, percentage=FALSE)
  textname <- gsub("\\..*","", files.v[i])
  segments.m <- cbind(paste(textname,
                            segment=1:nrow(chunk.m), sep="#"),
                      chunk.m)
  topic.m <- rbind(topic.m, segments.m)
}


#Run the topic modeling again
documents <- as.data.frame(topic.m, stringsAsFactors=F)
colnames(documents) <- c("id", "text")
library(mallet)
mallet.instances <- mallet.import(documents$id,
                                  documents$text,
                                  "Stoplist/Jockers_stoplist.csv",
                                  FALSE,
                                  token.regexp="[\\p{L}']+")
#Set topic number: first generate one wordcloud
topic.model <- MalletLDA(num.topics=1)
topic.model$loadDocuments(mallet.instances)
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
topic.model$train(400)
topic.words.m <- mallet.topic.words(topic.model,
                                    smoothed=TRUE,
                                    normalized=TRUE)
colnames(topic.words.m) <- vocabulary


#Step 4: Topic Visualization
library(slam)
library(wordcloud)

#Create world cloud for that one topic
topic.top.words <- mallet.top.words(topic.model,
                                    topic.words.m[1,], 80)

png(paste("analysis/tagged_wordcloud/Adj_Jockers_stoplist_1_wordcloud/topic1-",
          paste(topic.top.words$words[1],
                topic.top.words$words[2],
                topic.top.words$words[3],sep = "-"), ".png", sep = ""),
    width=12,height=8, units='in', res=300)

wordcloud(topic.top.words$words,
          topic.top.words$weights,
          c(4,.8), rot.per=0, random.order=F, colors = rainbow(20))

dev.off()

#Create 25 topics
topic.model <- MalletLDA(num.topics=25)
topic.model$loadDocuments(mallet.instances)
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
head(word.freqs)
topic.model$setAlphaOptimization(40, 80)
#Set the number of iterations in training
topic.model$train(400)

#Step 3: Unpack the model into a matrix
#That has each topic with their word type
topic.words.m <- mallet.topic.words(topic.model,
                                    smoothed=TRUE,
                                    normalized=TRUE)
dim(topic.words.m)
#25 * 14838
rowSums(topic.words.m)
colnames(topic.words.m) <- vocabulary

#Generate 25 word clouds
for (i in 1: 25){
  #each topic is a row in topic.words.m
  topic.top.words <- mallet.top.words(topic.model,
                                      topic.words.m[i,], 80)
  #Produce a word cloud of top words in a topic
  png(paste("analysis/tagged_wordcloud/Adj_Jockers_stoplist_25_wordclouds/topic",
            i, "-",
            paste(topic.top.words$words[1],
                  topic.top.words$words[2],
                  topic.top.words$words[3],sep = "-"), ".png", sep = ""),
      width=12,height=8, units='in', res=300)
  wordcloud(topic.top.words$words,
            topic.top.words$weights,
            c(4,.8), rot.per=0, random.order=F, colors = rainbow(20))
  dev.off()
}


#Create 50 topics
topic.model <- MalletLDA(num.topics=50)
topic.model$loadDocuments(mallet.instances)
#Access a list of the entire vocabulary of the corpus
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
#Control the optimization interval and burn-in
topic.model$setAlphaOptimization(40, 80)
topic.model$train(400)

#Step 3: Unpack the model into a matrix
#That has each topic with their word type
topic.words.m <- mallet.topic.words(topic.model,
                                    smoothed=TRUE,
                                    normalized=TRUE)
dim(topic.words.m)
#50 * 14838
rowSums(topic.words.m)
colnames(topic.words.m) <- vocabulary

#Generate 50 word clouds
for (i in 1: 50){
  #each topic is a row in topic.words.m
  topic.top.words <- mallet.top.words(topic.model,
                                      topic.words.m[i,], 80)
  #Produce a word cloud of top words in a topic
  png(paste("analysis/tagged_wordcloud/Adj_Jockers_stoplist_50_wordclouds/topic",
            i, "-",
            paste(topic.top.words$words[1],
                  topic.top.words$words[2],
                  topic.top.words$words[3],sep = "-"), ".png", sep = ""),
      width=12,height=8, units='in', res=300)
  wordcloud(topic.top.words$words,
            topic.top.words$weights,
            c(4,.8), rot.per=0, random.order=F, colors = rainbow(30))
  dev.off()
}


#Create 100 topics
topic.model <- MalletLDA(num.topics=100)
topic.model$loadDocuments(mallet.instances)
#Access a list of the entire vocabulary of the corpus
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
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
#100 * 14838
rowSums(topic.words.m)
#Add the vocabulary column for each word type
colnames(topic.words.m) <- vocabulary

#Generate 100 word clouds
for (i in 1: 100){
  #each topic is a row in topic.words.m
  topic.top.words <- mallet.top.words(topic.model,
                                      topic.words.m[i,], 80)
  #Produce a word cloud of top words in a topic
  png(paste("analysis/tagged_wordcloud/Adj_Jockers_stoplist_100_wordclouds/topic",
            i, "-",
            paste(topic.top.words$words[1],
                  topic.top.words$words[2],
                  topic.top.words$words[3],sep = "-"), ".png", sep = ""),
      width=12,height=8, units='in', res=300)
  wordcloud(topic.top.words$words,
            topic.top.words$weights,
            c(4,.8), rot.per=0, random.order=F, colors = rainbow(30))
  dev.off()
}
