setwd("~/Documents/Sample Corpus/0.1_Sample Corpus_txt")
text.v <- scan("Adams-2008-Aspects_of_the_music-text_relationship_in_rap.txt", what="character", sep="\n")
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

#Chapter 3 
#3.1 Accessing Word Data
#Compare the usage of he vs. she and him vs. her:
sorted.text.freqs.t["he"]
## he
## 21
sorted.text.freqs.t["she"]
## she
## 4
sorted.text.freqs.t["him"]
## him
## NA
sorted.text.freqs.t["her"]
## her
## 2

#list first value in frequency table
sorted.text.freqs.t[1]
#list frequency of the word "the"
sorted.text.freqs.t["the"]

#how much more frequent "he" than "she"
sorted.text.freqs.t["he"]/sorted.text.freqs.t["she"]

#lengh of word list = sum of raw counts in sorted table
length(text.word.v)
sum(sorted.text.freqs.t)
sum(text.freqs.t)

#Sorted relative frequcny table (*100 means in every 100 words)
sorted.text.rel.freqs.t <- 100*(sorted.text.freqs.t/sum(sorted.text.freqs.t))

#occurences of "the" per every 100 words
sorted.text.rel.freqs.t["the"]

#plot top ten frequent words by percentage freq
plot(sorted.text.rel.freqs.t[1:10], type = "b", xlab = "Top Ten Words", main = "Aspects of the music-text relationship in rap", ylab= "Percentage of Full Text", xaxt = "n")
axis(1,1:10, labels=names(sorted.text.freqs.t[1:10]))

#Practice 3.1 Top Ten Words in flow of rap music
text.v <- scan("Adams-2009-On_the_metrical_techniques_of_flow_in_rap_music.txt", what="character", sep="\n")

#view text2
text.v

#check the length (lines)
length(text.v)

#make every word lower case
text.lower.v <-tolower(text.v)

#make a word list (eliminate punctuation)
flow.words.l <-strsplit(text.lower.v, "\\W")

#unlist the text, make a real word list
flow.word.v <- unlist(flow.words.l)

#eliminate the blanks
not.blanks.v <- which(flow.word.v!="")
flow.word.v <- flow.word.v[not.blanks.v]

#make a sorted contigency table:word with its counts
flow.freqs.t <- table(flow.word.v)
sorted.flow.freqs.t <- sort(flow.freqs.t , decreasing=TRUE)

#make a sorted relative.freq table
sorted.flow.rel.freqs.t <- 100*(sorted.flow.freqs.t/sum(sorted.flow.freqs.t))

#plot top ten frequent words by percentage freq
plot(sorted.flow.rel.freqs.t[1:10], type = "b", main = "Flow in Rap Music", xlab = "Top Ten Words", ylab= "Percentage of Full Text", xaxt = "n")
axis(1,1:10, labels=names(sorted.flow.freqs.t[1:10]))


#3.2: combine names of two list together and see how many unique types
combined.v <- c(names(sorted.text.rel.freqs.t[1:10]), names(sorted.flow.rel.freqs.t[1:10]))
length(unique(combined.v))

#3.3: find words two lists shared
names(sorted.flow.rel.freqs.t[which (names(sorted.flow.rel.freqs.t[1:10]) %in% names(sorted.text.rel.freqs.t[1:10]))])
names(sorted.text.rel.freqs.t[which (names(sorted.text.rel.freqs.t[1:10]) %in% names(sorted.flow.rel.freqs.t[1:10]))])

#3.4:Words in top ten of flow that are not in top ten of aspect
#presentflow: index flow words present in aspect
presentflow <- which(names(sorted.flow.rel.freqs.t[1:10])
                      %in% names(sorted.text.rel.freqs.t[1:10]))
names(sorted.flow.rel.freqs.t[1:10])[-presentflow]

#Words in aspect but not in flow
#presenttext: text words present in flow
presenttext <- which(names(sorted.text.rel.freqs.t[1:10])
                     %in% names(sorted.flow.rel.freqs.t[1:10]))
names(sorted.text.rel.freqs.t[1:10])[-presenttext]

