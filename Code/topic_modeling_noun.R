#Modified text chunking for tagged corpus
#Remove non-noun words prior to segmentation
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
#For nouns topic words
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
                                  "Stoplist/Jockers_stoplist.csv",
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
#80 words for each topic
for (i in 1: 165){
  #each topic is a row in topic.words.m
  topic.top.words <- mallet.top.words(topic.model,
                                      topic.words.m[i,], 80)
  #Produce a word cloud of top words in a topic
  set.seed(142)
  png(paste("analysis/wordcloud_tagged/Noun_Jockers_stoplist_165_wordclouds/topic",
            i, "-",
            paste(topic.top.words$words[1],
                  topic.top.words$words[2],
                  topic.top.words$words[3],sep = "-"), ".png", sep = ""),
      width=12,height=8, units='in', res=300)
  wordcloud(topic.top.words$words,
            topic.top.words$weights,
            c(4,.8), rot.per=0.2, random.order=F, colors = rainbow(30))
  dev.off()
}


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
for (i in 1: 165){
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

