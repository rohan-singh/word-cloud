install.packages('ctv')
install.views('NaturalLanguageProcessing')

worldCloud <- function()
{
  library('ctv')
  #path: (./corpus/target)
  cname <- file.path(".","corpus","target")
  
  library (tm)
  docs <- Corpus(DirSource(cname))
  
  library (SnowballC)
  #replacing '/' and '@' with a whitespace 
  for (j in seq(docs))
  {
    docs[[j]] <- gsub("/"," ",docs[[j]])
    docs[[j]] <- gsub("@"," ",docs[[j]])
  }
  
  docs <- tm_map(docs,tolower)
  docs <- tm_map(docs, PlainTextDocument)
  docs <- tm_map(docs,removeWords, stopwords("english"))
  docs <- tm_map(docs,removeNumbers)
  docs <- tm_map(docs,removePunctuation)
  docs <- tm_map(docs,stripWhitespace)
  dtm <- DocumentTermMatrix(docs)
  
  library(wordcloud)
  m <- as.matrix(dtm)
  v <- sort(colSums(m),decreasing=TRUE)
  head(v,14)
  words <- names(v)
  d <- data.frame(word=words, freq=v)
  wordcloud(d$word,d$freq,min.freq=50)
  return
}
