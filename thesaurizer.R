library(rvest)
library(dplyr)
library(ggplot2)

findSynonym <- function(word){
  # Basic web scraper which accesses thesaurus.com to find synonyms
  thesaurusLink = paste('https://www.collinsdictionary.com/us/dictionary/english-thesaurus/', word, sep = '')
  synoPage = read_html(thesaurusLink)
  
  synonyms <- synoPage %>%
    html_nodes('.type-syn .orth') %>%
    
    html_text() %>%
    unlist()
  return(synonyms)
}

thesaurizeSentence <- function(sentence){
  # Get rid of punctuation and separate by spaces
  sentence <- gsub('[[:punct:]]', '', sentence)
  sentence = strsplit(sentence, '\\s+')
  sentence = sentence[[1]]
  # For every word find its synonym. If there isn't a page, just use the same word.
  # Added in a banned words list because the page for I has a bunch of nonsense
  bannedwords = list('I')
  newSentence = character(length=length(sentence))
  c = 1
  for(word in sentence){
    synonym = findSynonym(word)
    Sys.sleep(round(runif(1,min=4,max=16)))
    if(length(synonym)<1){
      synonym = word
    }else{
      synonym = sample(synonym)[1]
    }
    newSentence[c] = synonym
    c = c+1
  }
  newSentence = paste(newSentence, collapse = ' ')
  return(newSentence)
}


sentence1 = 'Bye buddy I hope you find your dad'
synoSentence = thesaurizeSentence(sentence1)
message(synoSentence)
