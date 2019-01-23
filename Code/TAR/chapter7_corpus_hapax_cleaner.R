library(quanteda)
library(magrittr)

text.v <- scan("merged.txt", what="character", sep="\n")
text.v <- enc2utf8(text.v)
text.v <- paste(text.v, collapse = " ")
#lowercase and split on non-word characters

text.lower.v <- char_tolower(text.v)
#remove hyphens not connecting words
text.words.v <- gsub(" - ", " ", text.lower.v,perl = TRUE)
#keep intro-word hyphen and apostrophe, alphabeta, digital
text.words.v <-strip(text.words.v, char.keep=c("-","'"),
                     digit.remove = F,
                     apostrophe.remove=F,
                     lower.case=T)

#word tokenization, remove puncutation and symbols
#Don't have to remove the blanks
text.words <-tokens(text.words.v, 
                    remove_punct =TRUE,
                    remove_symbols=TRUE) %>% as.character()



length(text.words)
#Make a raw frequency table
text.freqs.t <- table(text.words)
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
addWorksheet(wb, "sample-corpus hapax2")

# Write the data to the sheets
writeData(wb, sheet = "sample-corpus hapax2", x = corpus.result)
# Export the file
saveWorkbook(wb, "analysis/Analysis.xlsx", overwrite = T)


