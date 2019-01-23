setwd("~/Documents/Sample Corpus/Short_ID_Sample Corpus_txt")
text.v <- scan("Adams_2008_Aspects_of_the.txt", what="character", sep="\n")
text.v
text.v[1]
length(text.v)

#make every words to lowercase
text.lower.v <-tolower(text.v)

#only extract words, \\W means we don't need punctuation
text.words.l <-strsplit(text.lower.v, "\\W")

#check their data type by using class
class(text.lower.v)
class(text.words.l)

#check more detailed structure
str(text.words.l)

#unlist the list into vector, the resulting contains empty string
text.word.v <- unlist(text.words.l)

#identify not blank items
not.blanks.v <- which(text.word.v!="")
not.blanks.v
not.blanks.v[1:10]

#Overwrite previous text word vector to non-blank version
text.word.v <- text.word.v[not.blanks.v]
text.word.v

#Look at text
text.word.v[4:6]
mypositions.v <- c(4,5,6)
text.word.v[mypositions.v]
which(text.word.v=="music")
text.word.v[which(text.word.v=="music")]
length(text.word.v[which(text.word.v=="music")])
length(text.word.v)
# Put a count of the occurrences of music into music.hits.v
music.hits.v <- length(text.word.v[which(text.word.v=="music")])
# Put a count of total words into total.words.v
total.words.v <- length(text.word.v)
# now divide
music.hits.v/total.words.v

#number of unique words
length(unique(text.word.v))

#get a contigency table of word type and their freq
text.freqs.t <- table(text.word.v)

#view the first ten 
text.freqs.t[1:10]

#sort from most to least frequent
sorted.text.freqs.t <- sort(text.freqs.t , decreasing=TRUE)
sorted.text.freqs.t[1:10]


plot(sorted.text.freqs.t[1:10])
#to get more informative plot

plot(sorted.text.freqs.t[1:10], type = "b", xlab = "Top Ten Words", ylab= "Word Count", xaxt = "n")
axis(1,1:10, labels=names(sorted.text.freqs.t[1:10]))

