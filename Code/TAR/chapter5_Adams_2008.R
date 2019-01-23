setwd("~/Documents/Sample Corpus/0.1_Sample Corpus_txt")
text.v <- scan("Adams-2008-Aspects_of_the_music-text_relationship_in_rap.txt", what="character", sep="\n")
body.v <- gsub('^.*November 2007\\s*|\\s*References 1.*$', '', text.v)
#identify each section title
title.container.v <-c("Introduction", "A Brief Developmental History of Rap", "An Analytical Approach to Rap", 
                      "Incorporation of Drumbeat Rhythms into the Lyrics", "Incorporation of Motivic Elements into the Lyrics",
                      "Conclusion")
text.v <- c(body.v, "END")
last.container.v <- "END"
title.container.v <- c(title.container.v, last.container.v)
chapter.raws.l <- list()
chapter.freqs.l <- list()
for(i in 1:length(title.container.v)){
  if(i != length(title.container.v)){
    chapter.title <- title.container.v[i]
    pattern <- paste("^.*",title.container.v[i], "\\s*|\\s*",title.container.v[i+1], ".*$")
    content.v <- gsub(pattern, '', body.v)
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
rap.l <- lapply(chapter.freqs.l, '[', 'rap')
rap.m <- do.call(rbind, rap.l)
rap.v <- as.vector(rap.m[,1])
#Similar for music
music.l <- lapply(chapter.freqs.l, '[', 'music')
music.m <- do.call(rbind, music.l)
music.v <- as.vector(music.m[,1])

rap.music.m <- cbind(rap.v, music.v)
colnames(rap.music.m) <- c("rap", "music")

#Calculate correltaion for rap and music
rap.music.m[which(is.na(rap.music.m))] <- 0
cor(rap.music.m)
#Run correlation for two vectors we want to correlate
mycor <- cor(rap.music.m[,"rap"], rap.music.m[,"music"])
mycor 
length(rap.music.m)

#Convert the matrix object into a data frame
cor.data.df <- as.data.frame(rap.music.m)
#Check the correlation coefficient, should be the same as before
cor(cor.data.df)

#iterate 10000 times to test random data correlation
mycors.v <- NULL
for (i in 1: 10000){
  mycors.v <- c(mycors.v, cor(sample(cor.data.df$rap), cor.data.df$music))
}

#Use stats
min(mycors.v)
##-0.9675119
max(mycors.v)
##0.9718796
range(mycors.v)
## -0.9675119  0.9718796
mean(mycors.v)
##-0.002696092
sd(mycors.v)
##0.4455714

#Generate a histogram
h <- hist(mycors.v, breaks = 100, col="grey", xlab="Correlation Coefficient",
          main = "Histogram of Random Correlation Coefficients\n with Normal Curve",
          plot = T)
xfit <-seq(min(mycors.v), max(mycors.v), length = 1000)
yfit <- dnorm(xfit, mean = mean(mycors.v), sd=sd(mycors.v))
yfit <- yfit*diff(h$mids[1:2]) * length(mycors.v)
lines(xfit, yfit, col="black", lwd=2)

#Add two more columns of I and my
my.l <- lapply(chapter.freqs.l, "[", "my")
my.m <- do.call(rbind, my.l)
my.v <- as.vector(my.m[,1])
i.l <- lapply(chapter.freqs.l, "[", "i")
i.m <- do.call(rbind, i.l)
i.v <- as.vector(i.m[,1])
rap.music.my.i.m <- cbind(rap.v, music.v, my.v, i.v)
rap.music.my.i.m[which(is.na(rap.music.my.i.m))] <- 0
cor(rap.music.my.i.m)

#Calculate coefficient for i and my
text.l <- lapply(chapter.freqs.l, "[", "text")
text.m <- do.call(rbind, text.l)
text.v <- as.vector(text.m[,1])
music.text.v <- cbind(music.v, text.v)
music.text.v[which(is.na(music.text.v))] <- 0
music.text.v.data.df <- as.data.frame(music.text.v)
cor(music.text.v.data.df$music, music.text.v.data.df$text)
#0.88
music.text.my.cors.v <- NULL
for (i in 1:10000){
  music.text.my.cors.v <-c(music.text.my.cors.v, cor(sample(music.text.v.data.df$music), music.text.v.data.df$text))
}
min(music.text.my.cors.v)
## [1] -0.8940106
max(music.text.my.cors.v)
## [1] 0.9219084
range(music.text.my.cors.v)
## [1] -0.8940106  0.9219084
mean(music.text.my.cors.v)
## [1] -0.003958473
sd(music.text.my.cors.v)
## [1] 0.4459936
