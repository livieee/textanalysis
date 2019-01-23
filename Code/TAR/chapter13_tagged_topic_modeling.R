setwd("~/Documents/Sample Corpus")
input.dir <- "0.1_Sample Corpus_txt"
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
  write(tagged_text, paste("taggedCorpus/",
                           files.v[i], sep=""))
}

#Modified text chunking for tagged corpus
#Remove non-noun words prior to segmentation
#Chunk size = 500
inputDir <- "taggedCorpus"
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
topic.m <- NULL
for(i in 1:length(files.v)){
  tagged.text <- scan(file.path(inputDir, files.v[i]),
                      what="character", sep="\n")
  chunk.m <- makeFlexTextChunksFromTagged(tagged.text,
                                          chunk.size, percentage=FALSE)
  textname <- gsub("\\..*","", files.v[i])
  segments.m <- cbind(paste(textname,
                            segment=1:nrow(chunk.m), sep="#"),
                      chunk.m)
  topic.m <- rbind(topic.m, segments.m)
}


#With the chunk matrix now prepared
#Run the topic modeling again
documents <- as.data.frame(topic.m, stringsAsFactors=F)
colnames(documents) <- c("id", "text")
library(mallet)
mallet.instances <- mallet.import(documents$id,
                                  documents$text,
                                  "Stoplist/stoplist.csv",
                                  FALSE,
                                  token.regexp="[\\p{L}']+")
topic.model <- MalletLDA(num.topics=165)
topic.model$loadDocuments(mallet.instances)
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
topic.model$train(400)
topic.words.m <- mallet.topic.words(topic.model,
                                    smoothed=TRUE,
                                    normalized=TRUE)
colnames(topic.words.m) <- vocabulary


#Produce a word cloud for each topic data
for (i in 1: 165){
  #each topic is a row in topic.words.m
  topic.top.words <- mallet.top.words(topic.model,
                                      topic.words.m[i,], 100)
  png(paste("~/Documents/Sample Corpus/results/chapter13-tagged-wordcloud/topic",i, ".png", sep = ""), 
      width=12,height=8, units='in', res=300)
  #Produce a word cloud of top words in a topic
  wordcloud(topic.top.words$words,
            topic.top.words$weights,
            c(4,.8), rot.per=0, random.order=F)
  dev.off()
}

##Search keywords information from matrix
#Words that contain rhythm and tempo
keywords <-grep("^.*rhythm.*$|^.tempo.*$", colnames(topic.words.m))
topic.words.m[, keywords]

#See which topic rows has the highest concentration of these key terms
#Note here Java returns the result from 0
imp.row <- which(rowSums(topic.words.m[, keywords]) ==
                   max(rowSums(topic.words.m[, keywords])))
##Topic 14
#Examine the top n words from topic xx(the row number)
topic.top.words <- mallet.top.words(topic.model,
                                    topic.words.m[imp.row,], 60)
png(paste("~/Documents/Sample Corpus/results/chapter13-tagged-wordcloud-rhythm-tempo", sep = ""), 
    width=12,height=8, units='in', res=300)
wordcloud(topic.top.words$words,
          topic.top.words$weights,
          c(4,.8), rot.per=0, random.order=F)
dev.off()


#Search Keywords
#rhythm, tempo, rap
keywords<- c("rhythm","temporality","rap")
topic.words.m[,keywords]
imp.row <- which(rowSums(topic.words.m[, keywords]) ==
                   max(rowSums(topic.words.m[, keywords])))
topic.top.words <- mallet.top.words(topic.model,
                                    topic.words.m[imp.row,], 60)
png(paste("~/Documents/Sample Corpus/results/chapter13-tagged-wordcloud-rhythm-tempo-rap", sep = ""), 
    width=12,height=8, units='in', res=300)
wordcloud(topic.top.words$words,
          topic.top.words$weights,
          c(4,.8), rot.per=0, random.order=F)
dev.off()


#Search Keywords
#rhythm, temporality,rock
keywords<- c("rhythm","temporality","edm")
#keycol <-grep("^.*rhythm.*$|^.tempo.*$|^jazz$", colnames(topic.words.m))
topic.words.m[,keywords]
#See which topic rows has the highest concentration of these key terms
#Note here Java returns the result from 0
imp.row <- which(rowSums(topic.words.m[, keywords]) ==
                   max(rowSums(topic.words.m[, keywords])))
topic.top.words <- mallet.top.words(topic.model,
                                    topic.words.m[imp.row,], 60)
png(paste("~/Documents/Sample Corpus/results/chapter13-tagged-wordcloud-rhythm-tempo-jazz", sep = ""), 
    width=12,height=8, units='in', res=300)
wordcloud(topic.top.words$words,
          topic.top.words$weights,
          c(4,.8), rot.per=0, random.order=F)
dev.off()


#Check topic coherence and topic probability
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
#plot topic 130 means in all the documents 
barplot(doc.topic.means.df[, "V130"], names.arg=c(1:165))
filename <- as.character(doc.topic.means.df[31, "Group.1"])

