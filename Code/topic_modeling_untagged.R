#This is for topic modeling by using Jocker's stopilist
setwd("~/Documents/Sample Corpus")
input.dir <- "Short_ID_Sample Corpus_txt"
files.v <- dir(path=input.dir, pattern=".*txt")
source("code/corpusFunctions.R")

##create and set a chunk.size variable
chunk.size <- 1000 # number of words per chunk

#Step1: Segment the data
#Build the topic data frame
topic.m <- NULL
for(i in 1:length(files.v)){
  text.v <- scan(paste(input.dir, files.v[i], sep="/"), what="character", sep="\n")
  text.v <- enc2utf8(text.v)
  chunk.m <- makeFlexTextChunks(text.v, chunk.size,
                                percentage=FALSE)
  #Delete file extensions
  textname <- gsub("\\.*.txt$","", files.v[i])
  segments.m <- cbind(paste(textname,
                            segment=1:nrow(chunk.m), sep="#"), chunk.m)
  topic.m <- rbind(topic.m, segments.m)
}

documents <- as.data.frame(topic.m, stringsAsFactors=F)
colnames(documents) <- c("id", "text")

#Step2:Load R Mallet Package
library(mallet)

#Step3: Simple topic modeling with a standard stop list
#First step to generate topic model: invoke mallet.import
mallet.instances <- mallet.import(documents$id,
                                  documents$text,
                                  "Stoplist/Jockers_stoplist.csv",
                                  FALSE,
                                  token.regexp="[\\p{L}']+")

#Second step: create a topic model trainer object
#Set the number of topics = 1
topic.model <- MalletLDA(num.topics=1)
class(topic.model)
#Fill in trainer object with textual data
topic.model$loadDocuments(mallet.instances)
#Access a list of the entire vocabulary of the corpus
vocabulary <- topic.model$getVocabulary()
class(vocabulary)
length(vocabulary)
head(vocabulary)
vocabulary[1:50]

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
rowSums(topic.words.m)
#Add the vocabulary column for each word type
colnames(topic.words.m) <- vocabulary
topic.words.m[1, 1:3]

#Step 4: Topic Visualization
library(slam)
library(wordcloud)

#Create world cloud for each topic and save each
topic.top.words <- mallet.top.words(topic.model,
                                    topic.words.m[1,], 80)
#Get first three words in a topic
#topic.top.words$words[1:3]

one.cloud <- wordcloud(topic.top.words$words,
          topic.top.words$weights,
          c(4,.8), rot.per=0, random.order=F)

png(paste("analysis/untagged_wordcloud/Jockers_stoplist_one_wordcloud/topic1-",
paste(topic.top.words$words[1],
      topic.top.words$words[2],
      topic.top.words$words[3],sep = "-"), ".png", sep = ""),
    width=12,height=8, units='in', res=300)
dev.off()

#Create 25 topics
topic.model <- MalletLDA(num.topics=25)
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
#25 * 53745
rowSums(topic.words.m)
#Add the vocabulary column for each word type
colnames(topic.words.m) <- vocabulary

#Generate 25 word clouds
for (i in 1: 25){
  #each topic is a row in topic.words.m
  topic.top.words <- mallet.top.words(topic.model,
                                      topic.words.m[i,], 80)
  #Produce a word cloud of top words in a topic
  png(paste("analysis/untagged_wordcloud/Jockers_stoplist_25_wordclouds/topic",
            i, "-",
            paste(topic.top.words$words[1],
                  topic.top.words$words[2],
                  topic.top.words$words[3],sep = "-"), ".png", sep = ""),
      width=12,height=8, units='in', res=300)
  wordcloud(topic.top.words$words,
            topic.top.words$weights,
            c(4,.8), rot.per=0, random.order=F)
  dev.off()
}


#Create 50 topics
topic.model <- MalletLDA(num.topics=50)
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
#50 * 53745
rowSums(topic.words.m)
#Add the vocabulary column for each word type
colnames(topic.words.m) <- vocabulary

#Generate 50 word clouds
for (i in 1: 50){
  #each topic is a row in topic.words.m
  topic.top.words <- mallet.top.words(topic.model,
                                      topic.words.m[i,], 80)
  #Produce a word cloud of top words in a topic
  png(paste("analysis/untagged_wordcloud/Jockers_stoplist_50_wordclouds/topic",
            i, "-",
            paste(topic.top.words$words[1],
                  topic.top.words$words[2],
                  topic.top.words$words[3],sep = "-"), ".png", sep = ""),
      width=12,height=8, units='in', res=300)
  wordcloud(topic.top.words$words,
            topic.top.words$weights,
            c(4,.8), rot.per=0, random.order=F)
  dev.off()
}


#Create 100 topics
topic.model <- MalletLDA(num.topics=100)
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
#100 * 53745
rowSums(topic.words.m)
#Add the vocabulary column for each word type
colnames(topic.words.m) <- vocabulary

#Generate 100 word clouds
for (i in 1: 100){
  #each topic is a row in topic.words.m
  topic.top.words <- mallet.top.words(topic.model,
                                      topic.words.m[i,], 80)
  #Produce a word cloud of top words in a topic
  png(paste("analysis/untagged_wordcloud/Jockers_stoplist_100_wordclouds/topic",
            i, "-",
            paste(topic.top.words$words[1],
                  topic.top.words$words[2],
                  topic.top.words$words[3],sep = "-"), ".png", sep = ""),
      width=12,height=8, units='in', res=300)
  wordcloud(topic.top.words$words,
            topic.top.words$weights,
            c(4,.8), rot.per=0, random.order=F)
  dev.off()
}