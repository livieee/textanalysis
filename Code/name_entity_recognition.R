library(rJava)
text.v <- scan("Squinobal_2009_West_African_music.txt", what="character", sep="\n")
text.v <- enc2utf8(text.v)
text.v <- paste(text.v, collapse = " ")

install.packages("openNLPmodels.en",
                 repos = "http://datacube.wu.ac.at/",
                 type = "source")

#make word annotation
library(NLP);
library(openNLP);
library(RWeka)
library(openNLPmodels.en);
library(magrittr)
text.v <- as.String(text.v)
word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()
text_annotations <- annotate(text.v, list(sent_ann, word_ann))

class(text_annotations)
head(text_annotations)
text_doc <- AnnotatedPlainTextDocument(text.v, text_annotations)



#annotate people and place
person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")

pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)
text_annotations <- annotate(text.v, pipeline)
text_doc <- AnnotatedPlainTextDocument(text.v, text_annotations)

# Extract entities from an AnnotatedPlainTextDocument
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

person <- entities(text_doc, kind = "person")
write.csv(person, "person.csv")
entities(text_doc, kind = "location")
write.csv(entities(text_doc, kind = "location"), "location.csv")
entities(text_doc, kind = "organization")
write.csv(entities(text_doc, kind = "organization"), "organization.csv")
