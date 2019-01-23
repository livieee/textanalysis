setwd("~/Documents/Sample Corpus")
input.dir <- "0.1_Sample Corpus_txt"
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
                                  "Stoplist/stoplist.csv",
                                  FALSE,
                                  token.regexp="[\\p{L}']+")

#Second step: create a topic model trainer object
#Set the number of topics
topic.model <- MalletLDA(num.topics=165)
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
topic.words.m[1:3, 1:3]

#Step 4: Topic Visualization
library(slam)
library(wordcloud)

#Create world cloud for each topic and save each
for (i in 1: 165){
  #each topic is a row in topic.words.m
  topic.top.words <- mallet.top.words(topic.model,
                                      topic.words.m[i,], 100)
  #Produce a word cloud of top words in a topic
  png(paste("~/Documents/Sample Corpus/results/chapter13-untagged-wordcloud/topic",i, ".png", sep = ""), 
       width=12,height=8, units='in', res=300)
  wordcloud(topic.top.words$words,
            topic.top.words$weights,
            c(4,.8), rot.per=0, random.order=F)
  dev.off()
}


##Search keywords information from matrix
#Words that contain rhythm and tempo
#grep("^.*rhythm.*$", my.corpus.l[[60]])
#keywords <- c("rhythm", "rhythms", "meter", "meters", "metre", "metres",
#              "tempo", "tempos", "tempi")
keycol <-grep("^.*rhythm.*$|^.tempo.*$", colnames(topic.words.m))
topic.words.m[, keycol]

#See which topic rows has the highest concentration of these key terms
#Note here Java returns the result from 0
imp.row <- which(rowSums(topic.words.m[, keycol]) ==
                   max(rowSums(topic.words.m[, keycol])))
##Topic 14
#Examine the top n words from topic xx(the row number)
topic.top.words <- mallet.top.words(topic.model,
                                    topic.words.m[imp.row,], 60)
png(paste("~/Documents/Sample Corpus/results/untagged-wordcloud-rhythm-tempo", sep = ""), 
    width=12,height=8, units='in', res=300)
wordcloud(topic.top.words$words,
          topic.top.words$weights,
          c(4,.8), rot.per=0, random.order=F)
dev.off()


#Search Keywords
#rhythm, tempo, rap
#keycol <-grep("^.*rhythm.*$|^.tempo.*$|^rap$", colnames(topic.words.m))
keycol<- c("rhythm","temporality","rap")
topic.words.m[,keycol]
#See which topic rows has the highest concentration of these key terms
#Note here Java returns the result from 0
imp.row <- which(rowSums(topic.words.m[, keycol]) ==
                   max(rowSums(topic.words.m[, keycol])))
topic.top.words <- mallet.top.words(topic.model,
                                    topic.words.m[imp.row,], 60)
png(paste("~/Documents/Sample Corpus/results/untagged-wordcloud-rhythm-tempo-rap", sep = ""), 
    width=12,height=8, units='in', res=300)
wordcloud(topic.top.words$words,
          topic.top.words$weights,
          c(4,.8), rot.per=0, random.order=F)
dev.off()

#Search Keywords
#rhythm, tempo,rock
keywords<- c("rhythm","temporality","edm")
#keycol <-grep("^.*rhythm.*$|^.tempo.*$|^jazz$", colnames(topic.words.m))
topic.words.m[,keywords]
#See which topic rows has the highest concentration of these key terms
#Note here Java returns the result from 0
imp.row <- which(rowSums(topic.words.m[, keywords]) ==
                   max(rowSums(topic.words.m[, keywords])))
topic.top.words <- mallet.top.words(topic.model,
                                    topic.words.m[imp.row,], 60)
png(paste("~/Documents/Sample Corpus/results/untagged-wordcloud-rhythm-tempo-edm", sep = ""), 
    width=12,height=8, units='in', res=300)
wordcloud(topic.top.words$words,
          topic.top.words$weights,
          c(4,.8), rot.per=0, random.order=F)
dev.off()


#Step 5: Check topic coherence and topic probailitiy
#Insepect the probability of each topic appearing in each document
#Row is segment, column is each topic
doc.topics.m <- mallet.doc.topics(topic.model,
                                  smoothed=T,
                                  normalized=T)
#Get text segement name
file.ids.v <- documents[,1]
head(file.ids.v)
#Make a two column matrix: book name + chunk id
file.id.l <- strsplit(file.ids.v, "#")
file.chunk.id.l <- lapply(file.id.l, rbind)
file.chunk.id.m <- do.call(rbind, file.chunk.id.l)
head(file.chunk.id.m)

#Use cbind to bind the character data values in the first column to the topical values
doc.topics.df <- as.data.frame(doc.topics.m)
doc.topics.df <- cbind(file.chunk.id.m[,1], doc.topics.df)
#Mean value for each topic across the segments for each documents
doc.topic.means.df <- aggregate(doc.topics.df[, 2:ncol(doc.topics.df)],
                                list(doc.topics.df[,1]),
                                mean)
dim(doc.topic.means.df)
head(doc.topic.means.df)
#plot topic 128 means in all the documents 
barplot(doc.topic.means.df[, "V128"], names.arg=c(1:165))
filename <- as.character(doc.topic.means.df[124, "Group.1"])
