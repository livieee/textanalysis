source("code/doitKwicBest.R")
input.dir <- "0.1_Sample Corpus_txt"
output.dir <- "results/"
files.v <- dir(input.dir, ".*txt")
#Get the word list of Sample Courpus and check the length
my.corpus.l <- make.file.word.v.l(files.v, input.dir)
length(my.corpus.l)

#Build KWIC list
doitKwicBest(my.corpus.l)

#Get the index of filename
match("Butler-2005-Hearing_Kaleidoscopes-Embedded_grouping_dissonance_in_electronic_dance_music.txt", files.v)
files.v[15]
