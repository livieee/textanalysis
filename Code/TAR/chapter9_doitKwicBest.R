# Function to print a vector of file names in user
# friendly format
show.files <- function(file.name.v){
  for(i in 1:length(file.name.v)){
    cat(i, file.name.v[i], "\n", sep=" ")
  }
}

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

##It asks the user to search which word in which article within how many context
##And print the result one by one in readable format
##Finally asks the user if they want to save the result in spreadsheet
doitKwicBest <- function(named.text.word.vector.l){
  show.files(names(named.text.word.vector.l))
  # ask the user for three bits of information
  file.id <- as.numeric(
    readline("Which file would you like to examine? Enter a number: \n"))
  context <- as.numeric(
    readline("How much context do you want to see? Enter a number: \n"))
  keyword <- tolower((readline("Enter a keyword: \n")))
  hits.v <- which(named.text.word.vector.l[[file.id]] == keyword)
  if(length(hits.v)>0){
    result <- NULL
    for(h in 1:length(hits.v)){
      start <- hits.v[h]-context
      if(start < 1){
        start <- 1
      }
      end <- hits.v[h]+context
      cat("\n-----------------------", h, "-------------------------\n")
      cat(named.text.word.vector.l[[file.id]][start:(hits.v[h]-1)], sep=" ")
      cat(" [", named.text.word.vector.l[[file.id]][hits.v[h]],"] ", sep="")
      cat(named.text.word.vector.l[[file.id]][(hits.v[h]+1):end], sep=" ")
      myrow <- cbind(hits.v[h],
                     paste(named.text.word.vector.l[[file.id]][start:(hits.v[h]-1)],
                           collapse=" "),
                     paste(named.text.word.vector.l[[file.id]][hits.v[h]],
                           collapse=" "),
                     paste(named.text.word.vector.l[[file.id]][(hits.v[h]+1):end],
                           collapse=" "))
      result <- rbind(result,myrow)
    }
    colnames(result) <- c("position", "left", "keyword", "right")
    toprint <- as.numeric((
      readline("Would you like to save this result to a file:
enter 1=yes or 0=no \n")))
    if(toprint==1){
      write.csv(result,
                paste("results/", keyword,"_In_",
                      context, names(named.text.word.vector.l)[file.id], ".csv", sep=""))
    }
  } else {
    cat("YOUR KEYWORD WAS NOT FOUND\n")
  }
}