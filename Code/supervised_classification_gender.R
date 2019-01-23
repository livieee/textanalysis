setwd("~/Documents/Sample Corpus")
source("code/corpusFunctions.R")
##Create path for directory and get each file's name
input.dir <- "Short_ID_Sample Corpus_txt"
files.v <- dir(path=input.dir, pattern=".*txt")


text.freqs.l <- list() #a list object to hold the results
for (i in 1:length(files.v)){
  text.v <- scan(paste(input.dir, files.v[i], sep="/"), what="character", sep="\n")
  #convert to single string
  text.v <- enc2utf8(text.v)
  chunk.data.l <- getTEIWordSegmentTableList(text.v, 10)
  text.freqs.l[[files.v[i]]] <- chunk.data.l
}

#Check the structure of text.freqs.l
length(text.freqs.l)
length(text.freqs.l[[1]])
str(text.freqs.l[[37]])
str(text.freqs.l$`Adams-2009-On_the_metrical_techniques_of_flow_in_rap_music.txt`)
##See first chunk of the Adams-2008
text.freqs.l$Adams_2008_Aspects_of_the.txt

##Converting an R list into Matrix
my.mapply <- function(x){
  my.list <- mapply(data.frame, ID=seq_along(x),
                    x, SIMPLIFY=FALSE,
                    MoreArgs=list(stringsAsFactors=FALSE))
  my.df <- do.call(rbind, my.list)
  return(my.df)
}
freqs.l <- lapply(text.freqs.l, my.mapply)
freqs.df <- do.call(rbind,freqs.l)
dim(freqs.df)
#832132 * 3
head(freqs.df)


#Create unique file name+chhunk ID
#Note: this part has problem since author-year has replicated
#booknames.v <- gsub("\\..*", "", rownames(freqs.df))
#bookids.v <- gsub("(\\.*\\d+)(-|_).*$", "\\1", booknames.v)
#book.chunk.ids <- paste(bookids.v, freqs.df$ID, sep="#")
#freqs.df$ID <- book.chunk.ids
#head(freqs.df)
bookids.v <- gsub("\\.txt.*$", "", rownames(freqs.df))
book.chunk.ids <- paste(bookids.v, freqs.df$ID, sep="#")
freqs.df$ID <- book.chunk.ids
head(freqs.df)


#Cross Tabulation
#Create single row of data for each segment
result.t <- xtabs(Freq ~ ID+Var1, data=freqs.df)
final.df <- as.data.frame.matrix(result.t)
final.df[50:100, c("of", "the")]
dim(final.df)
#1650 * 56552
final.df[1,1:10]
rownames(final.df)
dim(final.df)
#1650 * 56552


#12.7 Mapping the data to metadata
##Create an text column from row name has #
#So we have a dataframe to that stores file name + chunk ID(1-10)
metacols.m <- do.call(rbind, strsplit(rownames(final.df), "#"))
head(metacols.m)
colnames(metacols.m) <- c("sampletext", "samplechunk")
head(metacols.m)
class(metacols.m)
metacols.df <- as.data.frame(metacols.m)
head(metacols.df)

#See all of the unique values in the sampletext column
unique(metacols.m[,"sampletext"])

#Import Metadata(Analysis) Spreadsheet
library(readxl)
metaGender <- read_excel("analysis/Analysis.xlsx", 
                         sheet = "Metadata(modified for gender)")
metaGender.df <- as.data.frame(cbind(metaGender[2],metaGender[6]))
colnames(metaGender.df) <- c("sampletext", "samplegender")
metacols.gender.df <- merge(metacols.df, metaGender.df, 
                            by = "sampletext",
                            all.x = TRUE)
dim(metacols.gender.df)
#1630 * 3
head(metacols.gender.df)


#Label for genders
gender.v <- metacols.gender.df[, 3]
unique(gender.v)
#Final data frame
gender.df <- cbind(gender.v, metacols.m, final.df)
dim(gender.df)
#1650 * 56555



#Reduce the feature set
freq.means.v <- colMeans(gender.df[,4:ncol(gender.df)])
#Find columns that has mean greater or equal to 0.005
keepers.v <- which(freq.means.v >=.005)
length(keepers.v)
keepers.v
names(keepers.v)
smaller.df <- gender.df[, names(keepers.v)] #does not include gender, metalcols
dim(smaller.df)
smaller.df <- gender.df[, c(names(gender.df)[1:3],
                                names(keepers.v))]
dim(smaller.df)

#Classification
#Identify the rows belonging to the anonymous author 
#We don't have for now
anon.v <- which(is.na(smaller.df$gender.v))
#Identify the rows that don't have anonymous rows and metadata
train <- smaller.df[-anon.v, 4:ncol(smaller.df)]
#Add a column that the classifier will use to organize the data (data that already known)
class.f <- smaller.df[-anon.v,"gender.v"]

#Pick a classifier and run the classification- SVM
library(e1071)
## Loading required package: class
#Generate a model by using train data and class data
model.svm <- svm(train, class.f)
#To test the accuracy of model, pred.svm contain a vector of text lables and the machine's guesses
pred.svm <- predict(model.svm, train)
pred.svm.df <- as.data.frame(pred.svm)


##Write result to existing workbook
#Problem, when writing to excel directly
#We cannot see the text name, so it's better to write csv
library(xlsx)
library(openxlsx)
wb <- loadWorkbook("analysis/Analysis.xlsx")
# Add some sheets to the workbook
addWorksheet(wb, "gender-model-predict")
# Write the data to the sheets
writeData(wb, sheet = "gender-model-predict", x = pred.svm)
# Export the file
saveWorkbook(wb, "analysis/Analysis.xlsx", overwrite = T)

write.csv(pred.svm.df, "predict-labels-model-gender-0.005-18-words.csv")
table <- table(pred.svm, class.f)
#Write the summary into csv
write.csv(table(pred.svm, class.f), 
          "analysis/supervised_classification_gender/gender-svm-summary-0.005-18-words.csv")

#Classify the anonymous text
#First to isolate the text data
testdata <- smaller.df[anon.v,4:ncol(smaller.df)]
#send the test data to the model for prediction
final.result <- predict (model.svm, testdata)
as.data.frame(final.result)
write.csv(as.data.frame(final.result), 
          "analysis/supervised_classification_gender/gender-predict-testdata-0.005-18-words.csv")


#Increase the number of features by decrease the frequency mean
keepers.v <- which(freq.means.v >=.0001)
length(keepers.v)
names(keepers.v)
smaller.df <- gender.df[, c(names(gender.df)[1:3],
                                names(keepers.v))]
anon.v <- which(is.na(smaller.df$gender.v))
train <- smaller.df[-anon.v,4:ncol(smaller.df)]
class.f <- smaller.df[-anon.v,"gender.v"]
model.svm <- svm(train, class.f)
pred.svm <- predict(model.svm, train)
pred.svm.df <- as.data.frame(pred.svm)
write.csv(pred.svm.df, "analysis/supervised_classification_gender/predict-labels-model-gender-0.0001-1244-words.csv")
table(pred.svm, class.f)
write.csv(table(pred.svm, class.f), 
          "analysis/supervised_classification_gender/gender-svm-summary-0.00005-2252-words.csv")

#Grab testdata(data without labels)
testdata <- smaller.df[anon.v,4:ncol(smaller.df)]
final.result <- predict (model.svm, testdata)
final.result.df <- as.data.frame(final.result)
write.csv(final.result.df, 
"analysis/supervised_classification_gender/gender-predict-testdata-0.00005-2252-words.csv")



#choose features that appears at least once in every author's work
keepers.v <- which(freq.means.v >=.0005)
smaller.df <- gender.df[, c(names(gender.df)[1:3],
                                names(keepers.v))]
author.sums <- aggregate(smaller.df[,
                                    4:ncol(smaller.df)],
                         list(smaller.df[,1]), sum)
reduced.author.sums <- author.sums[,
                                   colSums(author.sums==0) == 0]
keepers.v <- colnames(
  reduced.author.sums)[2:ncol(reduced.author.sums)]
smaller.df # show the new data frame
new.smaller.df <- smaller.df[,
                             c("gender.v","sampletext","samplechunk", keepers.v)]
anon.v <- which(is.na(smaller.df$gender.v))
train <- new.smaller.df[-anon.v,4:ncol(new.smaller.df)]
class.f <- new.smaller.df[-anon.v,"gender.v"]
model.svm <- svm(train, class.f)
pred.svm <- predict(model.svm, train)
write.csv(table(pred.svm, class.f), 
"analysis/supervised_classification_gender/gender-svm-summary-at-least-once-words.csv")

testdata <- new.smaller.df[anon.v,4:ncol(new.smaller.df)]
final.result <- predict (model.svm, testdata)
as.data.frame(final.result)


