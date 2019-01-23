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
#Length of each text
chapter.lengths.m <- do.call(rbind, lapply(chapter.raws.l,sum))

#Number of hapax in each chapter
chapter.hapax.v <- sapply(chapter.raws.l, function(x) sum(x == 1))
#Number of hapax in each chapter/ number words in each chapter
chapter.lengths.m <- do.call(rbind, lapply(chapter.raws.l,sum))
chapter.hapax.v[1] / chapter.lengths.m[1]
chapter.hapax.v[2]/chapter.lengths.m[2]
#Percentage of hapax in each chapter
hapax.percentage <- chapter.hapax.v / chapter.lengths.m
#Visualzie the chapter-by-chapter hapax richness
barplot(hapax.percentage, beside=T,col="grey",
        names.arg = seq(1:length(chapter.raws.l)))


#Get the Hapax item
chapter.hapax.words.l <- list()
for (i in 1:length(chapter.raws.l)){
  chapter.hapax.words.v <- which(chapter.raws.l[[i]][] == "1")
  chapter.hapax.words.l[[i]] <- chapter.hapax.words.v
  #result <- as.data.frame(chapter.hapax.words.l)
  #hapax <- as.data.frame(chapter.hapax.words.l[[i]])
}
hapax.df <- do.call(rbind, lapply(chapter.hapax.words.l, as.data.frame)) 
colnames(hapax.df)<- "relative position"
write.csv(hapax.df, "~/Documents/Sample Corpus/result/chapter7-merged-hapax.csv")


#Practice 7.1 calculate the correlation between text length and the number of hapax
cor(chapter.hapax.v, chapter.lengths.m)
#Sample Answer
lengths.v.hapax <- cbind(chapter.hapax.v, chapter.lengths.m)
cor(lengths.v.hapax [,1], lengths.v.hapax [,2])

#Practice 7.2 rank the values in hapax. percentage
ranks <- order(hapax.percentage, decreasing = TRUE)
names(chapter.raws.l)[54]
hapax.percentage[54]


