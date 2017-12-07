library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)

x <- load_tweets_db(as.data.frame = TRUE, table_name = "programminglanguages")

score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
                    sentence <- gsub('[[:punct:]]', "", sentence)
                    sentence <- gsub('[[:cntrl:]]', "", sentence)
                    sentence <- gsub('\\d+', "", sentence)
                    sentence <- tolower(sentence)
                    word.list <- str_split(sentence, '\\s+')
                    words <- unlist(word.list)
                    pos.matches <- match(words, pos.words)
                    neg.matches <- match(words, neg.words)
                    pos.matches <- !is.na(pos.matches)
                    neg.matches <- !is.na(neg.matches)
                    score <- sum(pos.matches) - sum(neg.matches)
                    return(score)
}, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

search <- function(searchterm)
{
  pos <- scan('positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
  neg <- scan('negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
  pos.words <- c(pos, 'upgrade')
  neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')

  scores <- score.sentiment(data$text, pos.words, neg.words, .progress='text')
  print(scores)
}
