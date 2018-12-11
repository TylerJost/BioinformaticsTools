# Thesaurizer's goal is to find the most ridiculous string of synonyms for a sentence

rm(list = setdiff(ls(), lsf.str()))
library(rvest)
library(dplyr)
library(ggplot2)

findSynonym <- function(word){
  # Basic web scraper which accesses thesaurus.com to find synonyms
  thesaurusLink = paste('https://www.thesaurus.com/browse/', word, sep = '')
  synoPage = read_html(thesaurusLink)
  
  synonyms <- synoPage %>%
    html_nodes('.e1qva7ye2+ .e1991neq0 .etbu2a31') %>%
    
    html_text() %>%
    unlist()
  return(synonyms)
}

thesaurizeSentence <- function(sentence){
  # Get rid of punctuation and separate by spaces
  sentence <- gsub('[[:punct:]]', '', sentence)
  sentence = strsplit(sentence, '\\s+')
  # For every word find its synonym. If there isn't a page, just use the same word.
  # Added in a banned words list because the page for I has a bunch of nonsense
  bannedwords = list('I')
  for(word in 1:length(sentence[[1]])){
    syno <- tryCatch({findSynonym(sentence[[1]][word])}, error = function(error_message){return(0)})
    if(sentence[[1]][word] %in% bannedwords){
      sentence[[1]][word] = sentence[[1]][word]
    }
    else if(is.character(syno)){
      # Randomly select word from list of words
      sentence[[1]][word] = syno[as.integer(runif(1, min = 1, max = length(syno)))]
    }
    else{
      sentence[[1]][word] = sentence[[1]][word]
    }
  }
  
  sentence = paste(sentence[[1]], collapse = ' ')
  return(sentence)
}


sentence1 = 'Bye buddy I hope you find your dad'
synoSentence = thesaurizeSentence(sentence1)
message(synoSentence)
