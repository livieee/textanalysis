library(quanteda)
library(magrittr)

text.v <- scan("merged.txt", what="character", sep="\n")
text.v <- enc2utf8(text.v)
text.v <- paste(text.v, collapse = " ")
#lowercase and split on non-word characters
text.lower.v <- tolower(text.v)
text.lower.v <- gsub("[^[:alnum:][:space:]']", " ", text.lower.v)
text.words.l <- strsplit(text.lower.v, "\\s+")
text.words.v <- unlist(text.words.l)
text.words.v <- text.words.v[which(text.words.v!="")]

length(text.words.v)
#Make a raw frequency table
text.freqs.t <- table(text.words.v)
text.freqs.t[1]

#Hapax item in the merged.txt
hapax <- names(which(text.freqs.t == "1"))
head(hapax)
corpus.result <- as.data.frame(hapax)
colnames(corpus.result) <- "hapax across corpus"
length(hapax)


library(xlsx)
library(openxlsx)
wb <- loadWorkbook("analysis/Analysis.xlsx")
# Add some sheets to the workbook
addWorksheet(wb, "sample-corpus hapax")

# Write the data to the sheets
writeData(wb, sheet = "sample-corpus hapax", x = corpus.result)
# Export the file
saveWorkbook(wb, "analysis/Analysis.xlsx", overwrite = T)


