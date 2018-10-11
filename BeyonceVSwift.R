# Lyric analysis
rm(list = setdiff(ls(), lsf.str()))
library(rvest)
library(dplyr)
library(tidytext)
library(ggplot2)

lyricsWebScrape <- function(artist){
  metrolink = paste('http://www.metrolyrics.com/',artist,'-lyrics.html', sep = "")
  songList <- read_html(metrolink)
  
  songs <- songList %>%
    html_nodes('#popular .hasvidtable') %>%
    html_text() %>%
    unlist()
  
  # Take each song and make it into the corresponding correct HTML Link
  lyricsList = character(length(songs))
  for(songNo in length(songs):1){
    song = songs[songNo]
    song = gsub('\n', '',song)
    songs[songNo] = gsub(' Lyrics', '',song)
    song = gsub('[[:punct:]]', '', song)
    song = paste('http://www.metrolyrics.com/', sapply(strsplit(song, ' '), paste, collapse = '-'),'-',artist,'.html', sep = '')
    
    lyrics <- 0
    lyrics <- tryCatch(
      read_html(song), error = function(e){c('0','0')}
    )
    
    # If we were able to access the link, download the lyrics
    if(is.list(lyrics)){
      lyricLines <-lyrics %>%
        html_nodes('.verse') %>%
        html_text() %>%
        unlist()
      lyricLines <- gsub('\n',' ', paste(lyricLines,collapse = " "))
      lyricsList[songNo] <- gsub('[[:punct:]]','', lyricLines)
    }else{
      print(sprintf('Unable to get song %s', songs[songNo]))
    }
  }
  ldf = data.frame(lyricsList)
  
  ldf$songs = as.list(songs)
 
  
  # Get rid of songs we couldn't find the lyrics to
  ldf = ldf[-c(which(ldf$lyricsList == '')),]
  ldf <- ldf[c('songs', 'lyricsList')]
   return(ldf)
}

BeyonceLyrics <- lyricsWebScrape('beyonce')
SwiftLyrics <- lyricsWebScrape('taylor-swift')

BeyonceLyrics$lyricsList = as.character(BeyonceLyrics$lyricsList)
SwiftLyrics$lyricsList = as.character(SwiftLyrics$lyricsList)

# Filter words in stop words, words less than 3 characters, and distinct words
tidyBey <- BeyonceLyrics %>%
  unnest_tokens(lyrics,lyricsList) %>%
  anti_join(stop_words, by = c('lyrics'='word')) %>%
  filter(nchar(lyrics)>3)

tidyTay <- SwiftLyrics %>%
  unnest_tokens(lyrics,lyricsList) %>%
  anti_join(stop_words, by = c('lyrics'='word')) %>%
  filter(nchar(lyrics)>3)

# Get the top 10 word occurrences
graphics.off()
Top10Bey = count(tidyBey, lyrics, sort=TRUE)[1:10,]
Top10Bey$lyrics = factor(Top10Bey$lyrics, levels = Top10Bey$lyrics)
ggplot(Top10Bey) + geom_col(aes(lyrics,n))+labs(x='Lyric', y='Frequency', title = 'Top 10 Most Common Words by Beyoncé')

Top10Tay = count(tidyTay, lyrics, sort=TRUE)[1:10,]
Top10Tay$lyrics = factor(Top10Tay$lyrics, levels = Top10Tay$lyrics)
ggplot(Top10Tay) + geom_col(aes(lyrics,n))+labs(x='Lyric', y='Frequency', title = 'Top 10 Most Common Words by Taylor Swift')


# Get the sentiment analysis
BeySent <- tidyBey %>%
  inner_join(get_sentiments('afinn'), by = c('lyrics'='word'))
BeySent$songs = as.character(BeySent$songs)
# Find sum of scores corresponding to each song
BeySent = aggregate(score~songs, data=BeySent, FUN = sum)
BeySent$artist <-rep('Beyonce') 
# Repeat for Taylor Swift
TaySent <- tidyTay %>%
  inner_join(get_sentiments('afinn'), by = c('lyrics'='word'))
TaySent$songs = as.character(TaySent$songs)
TaySent = aggregate(score~songs, data=TaySent, FUN = sum)
TaySent$artist <-rep('Taylor Swift')

sentAnalysis = bind_rows(BeySent, TaySent)

ggplot(sentAnalysis, aes(score,fill=artist))+geom_histogram(alpha = 0.4, binwidth=5, color='black', position = 'identity')+
  scale_fill_manual(values=c("gold1", "darkcyan"))+
  labs(x="Sentiment Score per Song", y="Count", title="Sentiment A nalysis of Beyoncé vs. Taylor Swift")