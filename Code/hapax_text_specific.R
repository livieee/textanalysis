input.dir <- "Short_ID_Sample Corpus_txt"
files.v <- dir(input.dir, ".*txt")
source("code/corpusFunctions.R")
title <- gsub("\\.*.txt$","", files.v)

#Get the raw frequency table for each text in Corpus
my.corpus.raw.l <- make.file.table.v.l(files.v, input.dir)

#Number of hapax in each text
text.hapax.v <- sapply(my.corpus.raw.l, function(x) sum(x == 1))

library(rowr)
#Get the Hapax item
text.hapax.item.l <- list()
result <- NULL
for (i in 1:length(my.corpus.raw.l)){
  text.hapax.item.v <- names(which(my.corpus.raw.l[[i]][] == "1"))
  text.hapax.item.l[[i]] <- text.hapax.item.v
  result <- cbind.fill(result, text.hapax.item.l[[i]], fill=NA)
}
result$init <- NULL
colnames(result) <- title

#Write result to existing workbook
library(xlsx)
library(openxlsx)

wb <- loadWorkbook("analysis/Analysis.xlsx")

# Add some sheets to the workbook
addWorksheet(wb, "text-specific hapax")

# Write the data to the sheets
writeData(wb, sheet = "text-specific hapax", x = result)
# Export the file
saveWorkbook(wb, "analysis/Analysis.xlsx", overwrite = T)
