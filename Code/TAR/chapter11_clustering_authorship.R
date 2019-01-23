source("code/corpusFunctions.R")
##Create path for directory and get each file's name
input.dir <- "0.1_Sample Corpus_txt"
files.v <- dir(path=input.dir, pattern=".*txt")

##Load each file and create word table for each file
#Get the word list of Sample Courpus
text.freqs.l <- list() #a list object to hold the results
for (i in 1:length(files.v)){
  text.v <- scan(paste(input.dir, files.v[i], sep="/"), what="character", sep="\n")
  #convert to single string
  text.v <- enc2utf8(text.v)
  worddata <-getTEIWordTableList(text.v)
  text.freqs.l[[files.v[i]]] <- worddata
}
class(text.freqs.l)
names(text.freqs.l)
str(text.freqs.l)


## Convert list object into data matrix
freqs.l <- mapply(data.frame,
                  ID=seq_along(text.freqs.l),
                  text.freqs.l, SIMPLIFY=FALSE,
                  MoreArgs=list(stringsAsFactors=FALSE))
class(freqs.l[[1]])
class(text.freqs.l[[1]])
head(freqs.l)
##First ten rows of the data frame contained inside first list item
freqs.l[[1]][1:10,]
freqs.l[[1]][100:110,]


#Transform into matrix object, a single three-column data frame
freqs.df <- do.call(rbind, freqs.l)
dim(freqs.df)
class(freqs.df)
head(freqs.df)


#Transform: row is each text, and columns for each word type
result <- xtabs(Freq ~ ID+Var1, data=freqs.df)
dim(result)
##165 * 56553
colnames(result)
colnames(result)[100:110]
result[1, 100:110]

#Transform result into a matrix object, here 2 is for column, 1 is for row
final.m <- apply(result, 2, as.numeric)
class(final.m)

#Prepare data for clustering
#Set a threshold
smaller.m <- final.m[, apply(final.m, 2, mean)>=.25]
#Compare the size of the original matrix to the new, smaller one
dim(final.m)
dim(smaller.m)

titles <- names(text.freqs.l)
#format:author-name-until-digital
authors <- gsub("(\\.*\\d+)(-|_).*$", "\\1", titles)
#authors <-gsub("^([^-]*-[^-]*)-.*$", "\\1", titles)
#authors1 <- gsub("^([^_]*_[^_]*)_.*$", "\\1", authors)

#Create a distance object (distances between two books matrix)
dm <- dist(smaller.m)
#Perform a cluster analysis on the distance object
cluster <- hclust(dm)
#Get the book file names to use as labels
cluster$labels <- authors
#Plot the results as a dendrogram for inspection
plot(cluster)

##Output the dendrogram with appropriate size
pdf("~/Documents/Sample Corpus/result/chapter11-clustering-0.25.pdf", width=40, height=15)
plot(cluster)
dev.off()

#Change threshold
smaller.m <- final.m[, apply(final.m, 2, mean)>=2.25]
#Compare the size of the original matrix to the new, smaller one
dim(final.m)
dim(smaller.m)

titles <- names(text.freqs.l)
authors <- gsub("(\\.*\\d+)(-|_).*$", "\\1", titles)

#Create a distance object (distances between two books matrix)
dm <- dist(smaller.m)
#Perform a cluster analysis on the distance object
cluster <- hclust(dm)
#Get the book file names to use as labels
cluster$labels <- authors
#Plot the results as a dendrogram for inspection
plot(cluster)

##Output the dendrogram with appropriate size
pdf("~/Documents/Sample Corpus/result/chapter11-clustering-2.25.pdf", width=40, height=15)
plot(cluster)
dev.off()

