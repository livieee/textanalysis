setwd("~/Documents/Sample Corpus/0.1_Sample Corpus_txt")
#install qdapRegex
if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/qdapRegex")

text.v <- scan("Adams-2008-Aspects_of_the_music-text_relationship_in_rap.txt", what="character", sep="\n")
length(text.v)
#body.v <- gsub('^.*November 2007\\s*|\\s*References 1.*$', '', text.v)
#make every words to lowercase
text.lower.v <-tolower(text.v)
#only extract words, \\W means we don't need punctuation
text.words.l <-strsplit(text.lower.v, "\\W")
#unlist the list into vector, the resulting contains empty string
text.word.v <- unlist(text.words.l)
#identify not blank items
not.blanks.v <- which(text.word.v!="")
not.blanks.v
not.blanks.v[1:10]
#Overwrite previous text word vector to non-blank version
text.word.v <- text.word.v[not.blanks.v]

#create a sequence o numbers from 1 to n
seq(1:10)
#sequence 1 to n, store into narrative time vector
n.time.v <- seq(1:length(text.word.v))

#get position of a word occurrance and store into vector
rap.v <- which(text.word.v == "rap")
#n NA values
r.count.v <- rep(NA,length(n.time.v))
#reset the NA values to 1 in those places where the word was found.
r.count.v[rap.v] <- 1
#crank out a plot showing distribution of rap across the text
plot(r.count.v, main="Dispersion Plot of `rap' in Music-Relationship in Rap",
     xlab="Text Time", ylab="rap", type="h", ylim=c(0,1), yaxt='n')

#Show occurrences of "music"
music.v <- which(text.word.v == "music")  
#change 'r' to 'm' to keep rap and music in separate variables
m.count.v <- rep(NA, length(n.time.v))
#mark occurances of word with 1
m.count.v[music.v] <- 1
plot(m.count.v, main="Dispersion Plot of `music' in Music-Relationship in Rap",
     xlab="Text Time", ylab="music", type="h", ylim=c(0,1), yaxt='n')

#Show occurrences of "rhythm"
rhythm.v <- which(text.word.v == "rhythm")  
#change 'r' to 'm' to keep rap and rhythm in separate variables
r2.count.v <- rep(NA, length(n.time.v))
#mark occurances of word with 1
r2.count.v[rhythm.v] <- 1
plot(r2.count.v, main="Dispersion Plot of `rhythm' in Music-Relationship in Rap",
     xlab="Text Time", ylab="rhythm", type="h", ylim=c(0,1), yaxt='n')

#Reload text
text.v <- scan("Adams-2008-Aspects_of_the_music-text_relationship_in_rap.txt", what="character", sep="\n")
body.v <- gsub('^.*November 2007\\s*|\\s*References 1.*$', '', text.v)
#identify each section title
title.container.v <-c("Introduction", "A Brief Developmental History of Rap", "An Analytical Approach to Rap", 
                      "Incorporation of Drumbeat Rhythms into the Lyrics", "Incorporation of Motivic Elements into the Lyrics",
                      "Conclusion")
text.v <- c(body.v, "END")
last.container.v <- "END"
title.container.v <- c(title.container.v, last.container.v)



# chap.positions.v <- grep("^CHAPTER \\d", novel.lines.v)
#Check chapter lines result
# novel.lines.v[chap.positions.v]
#
# #1.See the lines of each chapter
# chap.positions.v
# #2.Add a new item "END" to the end of the novel.lines object
# novel.lines.v <- c(novel.lines.v, "END")
# last.position.v <- length(novel.lines.v)
# chap.positions.v <- c(chap.positions.v, last.position.v)
# chap.positions.v

#print chapter lines one by one
for (i in 1:length(title.container.v)){
  print(title.container.v[i])
}

for (i in 1:length(title.container.v)){
  print(paste("Section ", i, " begins at ",
              title.container.v[i], sep = ""))
}
#content.v <- gsub('^.*Introduction\\s*|\\s*Brief Developmental History of Rap.*$', '', body.v)
#iterates over each chapter lines
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
chapter.raws.l["Introduction"] 
chapter.raws.l["A Brief Developmental History of Rap"]
#get the first item in the chapter list
chapter.freqs.l[[1]] 
#return rap's relative freq in Introduction
chapter.freqs.l[[2]]["rap"]   

#grab rap values from the each item in the list
rap.l <- lapply(chapter.freqs.l, '[', 'rap')
#capture rap frequency of every chapters into matrix
rap.m <- do.call(rbind, rap.l)
#Get the only column of rap out of another vector
rap.v <- rap.m[,1]

#Similar for music
music.l <- lapply(chapter.freqs.l, '[', 'music')
music.m <- do.call(rbind, music.l)
music.v <- music.m[,1]

#Grab the word "rhythm" from entire list
rhythm.l <- lapply(chapter.freqs.l, '[', 'rhythm')
rhythm.m <- do.call(rbind, rhythm.l)
rhythm.v <- rhythm.m[,1]

#combine three columns into a single matrix
rap.music.rhythm.m <- cbind(rap.v, music.v, rhythm.v)
#plot barplot side by side
barplot(rap.music.rhythm.m , beside=T, col="grey")

#Sample Answer
#extract value for each word
whales.l <- lapply(chapter.freqs.l, '[', 'whale')
whales.m <- do.call(rbind, whales.l)
whales.v <- as.vector(whales.m[,1])

ahabs.l <- lapply(chapter.freqs.l, '[', 'ahab')
ahabs.m <- do.call(rbind, ahabs.l)
ahabs.v <- as.vector(ahabs.m[,1])

rhythm.l <- lapply(chapter.freqs.l, '[', 'rhythm')
rhythm.m <- do.call(rbind, rhythm.l)
rhythm.v <- as.vector(rhythm.m[,1])

whales.ahabs.rhythm.m <- cbind(whales.v, ahabs.v,rhythm.v)
barplot(whales.ahabs.rhythm.m, beside=T, col="grey")

#Practice 4.2: plot the raw occurences of whale and ahab per chapter
whales.raw.l <- lapply(chapter.raws.l, '[', 'whale')
whales.raw.m <- do.call(rbind, whales.raw.l)
whales.raw.v <- as.vector(whales.raw.m[,1])

ahabs.raw.l <- lapply(chapter.raws.l, '[', 'ahab')
ahabs.raw.m <- do.call(rbind, ahabs.raw.l)
ahabs.raw.v <- as.vector(ahabs.raw.m[,1])

rhythm.raw.l <- lapply(chapter.raws.l, '[', 'rhythm')
rhythm.raw.m <- do.call(rbind, rhythm.raw.l)
rhythm.raw.v <- as.vector(rhythm.raw.m[,1])

whales.ahabs.rhythm.raw.m <- cbind(whales.raw.v, ahabs.raw.v,rhythm.raw.v)
barplot(whales.ahabs.rhythm.raw.m, beside=T, col="grey")

