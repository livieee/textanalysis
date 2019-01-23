setwd("~/Documents/Sample Corpus/0.1_Sample Corpus_txt")
text.v <- scan("merged_text.txt", what="character", sep="\n")
text.v <- enc2utf8(text.v)
length(text.v)
text.v <- c("Begin", text.v)
text.position.v <- grep("^==============End Of.*$", text.v)
length(text.position.v) ##Should equal to number of documents:165
first.position.v <- 1
text.position.v <- c(first.position.v, text.position.v)
chapter.freqs.l <- list()
chapter.raws.l <- list()
for(i in 1:length(text.position.v)){
  if(i != length(text.position.v)){
    chapter.title <- text.v[text.position.v[i+1]]
    start <- text.position.v[i]+1
    end <- text.position.v[i+1]-1
    content.v <- text.v[start:end]
    chapter.words.v <- tolower(paste(content.v, collapse=" "))
    chapter.words.l <- strsplit(chapter.words.v, "\\W")
    chapter.word.v <- unlist(chapter.words.l)
    chapter.word.v <- chapter.word.v[which(chapter.word.v!="")]
    chapter.freqs.t <- table(chapter.word.v)
    chapter.raws.l[[chapter.title]] <- chapter.freqs.t
    chapter.freqs.t.rel <- 100*(chapter.freqs.t/sum(chapter.freqs.t))
    chapter.freqs.l[[chapter.title]] <- chapter.freqs.t.rel
  }
}

length(chapter.raws.l)
names(chapter.raws.l[1])
chapter.raws.l[[1]]


#6.2 Mean Word Frequency (text-by-text basis)
#Total number of word tokens in each text
sum(chapter.raws.l[[1]])
## [1] 8546
#The number of unique word type in each text
length(chapter.raws.l[[1]])
## [1] 1618
#Mean Word Frequency
sum(chapter.raws.l[[1]])/length(chapter.raws.l[[1]])
#Simpler way by using buiilt-in function mean
mean(chapter.raws.l[[1]])

#Mean word frequency for each texts
lapply(chapter.raws.l,mean)
#Wrap the results into row matrix
mean.word.use.m <- do.call(rbind, lapply(chapter.raws.l,mean))
dim(mean.word.use.m)
rownames(mean.word.use.m)
#Visualize the mean word usage pattern across the Sample Corpus
plot(mean.word.use.m, type="h")

#Normalize values by scaling
scale(mean.word.use.m)
#Visualize
plot(scale(mean.word.use.m), type="h")


#6.4 Ranking the values
order(mean.word.use.m, decreasing=TRUE)
mean.word.use.m[order(mean.word.use.m, decreasing=TRUE),]


#6.5 Type-token ratio
length(chapter.raws.l[[1]])/sum(chapter.raws.l[[1]])*100
#Calculate TTR for all the chapters
ttr.l <- lapply(chapter.raws.l, function(x){length(x)/sum(x)*100})
#Store the results into a row matrix
ttr.m <- do.call(rbind, ttr.l)
#Order the results:
ttr.m[order(ttr.m, decreasing=TRUE), ]
#Plot them
plot(ttr.m, type="h")


#Practice 6.1 Test correlation of chapter length and TTR
ttr.v <- as.vector(ttr.m)
chapter.lengths.m <- do.call(rbind, lapply(chapter.raws.l, sum))
chap.len.v <- as.vector(chapter.lengths.m)
cor(ttr.v, chap.len.v)

#Practice 6.2 Test correltaoin of chapter length and mean word frequency
mean.word.use.v <- as.vector(mean.word.use.m)
cor(mean.word.use.v, chap.len.v)


#Use randomization to test the likelihood that the correlation coefficient observed with TTR could have been the result of chance.
cor(ttr.v, chap.len.v)
## -0.7971883
my.cors.v <- NULL
for (i in 1:10000){
  my.cors.v <- c(my.cors.v, cor(sample(ttr.v), chap.len.v))
}
min(my.cors.v)
## -0.2448267
max(my.cors.v)
## 0.303992
range(my.cors.v)
##-0.2448267  0.3039920
mean(my.cors.v)
##-0.0005206393
sd(my.cors.v)
##0.07878482


