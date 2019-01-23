setwd("~/Documents/SampleCorpus")
input.dir <- "0.1_Sample Corpus_txt"
#Get all the file names under this directory
files.v <- dir(input.dir, "\\.txt$")
#8.1
# Function to print a vector of file names in user
# friendly format
show.files <- function(file.name.v){
  for(i in 1:length(file.name.v)){
    cat(i, file.name.v[i], "\n", sep=" ")
  }
}
#Send the files.v object to the function
show.files(files.v)

#8.2 A Word List Making
# Function takes a vector of file names and a directory path and
# returns a list in which each item in the list is an ordered
# vector of words from one of the files in the vector of file names
make.file.word.v.l <- function(files.v, input.dir){
  #set up an empty container
  text.word.vector.l <- list ()
  #loop over the files
  for (i in 1:length(files.v)) {
    # read the file from directory
    text.v <- scan(paste(input.dir, files.v[i], sep="/"), what="character", sep="\n")
    #convert to single string
    text.v <- enc2utf8(text.v)
    text.v <- paste(text.v, collapse = " ")
    #lowercase and split on non-word characters
    text.lower.v <- tolower(text.v)
    text.words.v <- strsplit(text.lower.v, "\\W")
    text.words.v <- unlist(text.words.v)
    #remove the blanks
    text.words.v <- text.words.v[which(text.words.v!="")]
    #use the index id from the files.v vector as the "name" in the list
    text.word.vector.l[[files.v[i]]] <- text.words.v
  }
  return(text.word.vector.l)
}
my.corpus.l <- make.file.word.v.l(files.v, input.dir)
#investigate content
class(my.corpus.l)
str(my.corpus.l)

#8.4 Finding Words and Their Neighbors
#Return the position of a word "rhythm"
positions.v <- which(my.corpus.l[[1]][]=="rhythm")
#Get a count of the number of times a word "rhythm" occurs
length(positions.v)
#Get the words come before and after the first instance of gutenberg
first.instance <- positions.v[1]
my.corpus.l[[1]][133]     #Check if it's the same word
#See the words just before and after the 3rd word
my.corpus.l[[1]][132:134]
#Same by use variable
my.corpus.l[[1]][(first.instance-1): (first.instance+1)]
#Pretty printed version
cat(my.corpus.l[[1]][(first.instance-1): (first.instance+1)])

#Return the position of a word "rhythm" in second text
positions2.v <- which(my.corpus.l[[2]][]=="rhythm")
#Get a count of the number of times a word "rhythm" occurs
length(positions2.v)
#Get the words come before and after the first instance of gutenberg
first.instance2 <- positions2.v[1]
my.corpus.l[[2]][20]     #Check if it's the same word
#See the words just before and after the 3rd word
my.corpus.l[[2]][19:21]
#Same by use variable
my.corpus.l[[2]][(first.instance2-1): (first.instance2+1)]
#Pretty printed version
cat(my.corpus.l[[2]][(first.instance2-1): (first.instance2+1)])


#Practice 8.1 Produce a five-word KWIC list for all occurrences of the word "rhythm" in both novels
#Get positions of "rhythm" for both novels
context <- 5
rhythm.positions.aspect <- which(my.corpus.l[[1]][]=="rhythm")
rhythm.positions.metrical <- which(my.corpus.l[[2]][]=="rhythm")

#8.2 KWIC with Cleaner Output
#Five-word KWIC list for Adams-2008
for (i in 1:length(rhythm.positions.aspect)){
  start <- rhythm.positions.aspect[i]-context
  end <- rhythm.positions.aspect[i]+context
  before <- my.corpus.l[[1]][start:(start+context-1)]
  after <- my.corpus.l[[1]][(start+context+1):end]
  keyword <- my.corpus.l[[1]][start+context]
  cat("-----------------------",i,"-----------------------", "\n")
  cat(before, "[", keyword, "]", after,"\n")
}

#Five-word KWIC list for Adams-2009
for (i in 1:length(rhythm.positions.metrical)){
  start <- rhythm.positions.metrical[i]-context
  end <- rhythm.positions.metrical[i]+context
  before <- my.corpus.l[[2]][start:(start+context-1)]
  after <- my.corpus.l[[2]][(start+context+1):end]
  keyword <- my.corpus.l[[2]][start+context]
  cat("-----------------------",i,"-----------------------", "\n")
  cat(before, "[", keyword, "]", after,"\n")
}

