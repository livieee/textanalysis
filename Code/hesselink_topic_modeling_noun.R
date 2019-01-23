#This file is to generating tagged wordclouds for Hesslink_data
#Only Nouns
#Only the words version (do not include digits)
#It was using the tm_stopwords
#1, 15, 30 Topics 
hesselink_data <- readRDS("~/Documents/Hesselink copy2/R object for Hesselink Dataset/hesselink_data.rds")
user.comment.v <- as.character(hesselink_data$User.Comment)
length(user.comment.v) #87 Comments in total
user.name <- as.character(hesselink_data$User)


#Pre-processing with a POS Tagger
#Code for tag our files
#Don't need to run here
library(openNLP)
library(NLP)
library(rJava)
for(i in 1:length(user.comment.v)){
  comment <- user.comment.v[i]
  comment <- enc2utf8(comment)
  words <- as.String(paste(comment, collapse = " "))
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
  write(tagged_text, paste("taggedHesselink/comment",
                           i,".txt", sep=""))
}


inputDir <- "taggedHesselink"
files.v <- dir(path=inputDir, pattern=".*txt")
chunk.size <- 15
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

library(magrittr)
library(quanteda)
#Function to make text chunks from tagged files (only get Nouns)
makeFlexTextChunksFromTagged <- function(tagged.text,
                                         chunk.size=15, percentage=TRUE){
  tagged.words <- splitText(tagged.text)
  tagged.words.keep <- c(selectTaggedWords(tagged.words,"/NN$"))
  words <- removeTags(tagged.words.keep)
  words.lower <- tolower(words)
  word.v <- gsub("[^[:alpha:][:space:]']", "", words.lower)
  if(identical(words, character(0))){
    return(NULL)
  }
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



#Build topic data frame from each file
#Only keep adjective words
topic.m <- NULL
for(i in 1:length(files.v)){
  tagged.text <- scan(paste("taggedHesselink/comment",i,
                            ".txt",sep=""),
                      what="character", sep="\n")
  chunk.m <- makeFlexTextChunksFromTagged(tagged.text,
                                             chunk.size, percentage=FALSE)
  title <- paste("taggedHesselink/comment",i,
                 ".txt",sep="")
  textname <- gsub("\\..*","", title)
  if(!is.null(chunk.m)){
  segments.m <- cbind(paste(textname,
                            segment=1:nrow(chunk.m), sep="#"),
                      chunk.m)
  }
  
  topic.m <- rbind(topic.m, segments.m)
}

#Run the topic modeling again

documents <- as.data.frame(topic.m, stringsAsFactors=F)
colnames(documents) <- c("id", "text")
library(mallet)
mallet.instances <- mallet.import(documents$id,
                                  documents$text,
                                  "tm_stopwords.csv",
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
dim(topic.words.m)
#1*257
colnames(topic.words.m) <- vocabulary


library(slam)
library(wordcloud)
#Create world cloud for each topic and save each
topic.top.words <- mallet.top.words(topic.model,
                                    topic.words.m[1,], 100)
#Get first three words in a topic
#topic.top.words$words[1:3]
set.seed(142)
png(paste("Topic_Modeling_tagged/Noun_tm_stopwords_1_topic/topic1-",
          paste(topic.top.words$words[1],
                topic.top.words$words[2],
                topic.top.words$words[3],sep = "-"), ".png", sep = ""),
    width=12,height=8, units='in', res=300)
wordcloud(topic.top.words$words,
          topic.top.words$weights,
          c(4,.8), rot.per=0.2, random.order=F, colors = rainbow(30))
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
#15 * 257
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
  png(paste("Topic_Modeling_tagged/Noun_tm_stopwords_15_topic/topic",
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
#30 * 921
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
  png(paste("Topic_Modeling_tagged/Noun_tm_stopwords_30_topic/topic",
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




#Topic coherence evaluation 
#For 30 copics
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
doc.topics.df <- as.data.frame(doc.topics.m, stringsAsFactors = F)
doc.topics.df <- cbind(file.chunk.id.m[,1], doc.topics.df)
#Mean value for each topic across the segments for each documents
doc.topic.means.df <- aggregate(doc.topics.df[, 2:ncol(doc.topics.df)],
                                list(doc.topics.df[,1]),
                                mean)

#165*166
#Row is for each text and column is for each topic
#plot topic 130 means in all the documents
doc.topic.means.df$Group.1 <- as.character(doc.topic.means.df$Group.1)

#Get three most frequent words for each topic
result.l <- list()
for (i in 1: 30){
  #each topic is a row in topic.words.m
  topic.top.words <- mallet.top.words(topic.model,
                                      topic.words.m[i,], 80)
  top.three.words <- paste(topic.top.words$words[1],
                           topic.top.words$words[2],
                           topic.top.words$words[3],sep = ", ")
  top.three.docs <- doc.topic.means.df$Group.1[
    order(doc.topic.means.df[i+1], decreasing = T)[1:3]]
  result.l[[i]] <- c(paste("topic",i,sep=""),
                     top.three.words, top.three.docs)
  
}
topic.info.df <- do.call(cbind, result.l) 
topic.info.tdf <- t(topic.info.df)

topic.tdf.colnames<- c("topic","most frequent nouns",
                       "File ID 1",
                       "File ID 2",
                       "File ID 3")
results <- rbind (topic.tdf.colnames,topic.info.tdf)
colnames(results)


#Write to current working workbook
library(xlsx)
library(openxlsx)
wb <- loadWorkbook("analysis/Analysis.xlsx")

# Add some sheets to the workbook
addWorksheet(wb, "165-tagged-topic info")

# Write the data to the sheets
writeData(wb, sheet = "165-tagged-topic info", x = results)
# Export the file
saveWorkbook(wb, "analysis/Analysis.xlsx", overwrite = T)




