library(twitteR)
library(RMySQL)
library(stringi)
library(tm)
library(ggplot2)
library(wordcloud)

print(Sys.time())
con <- dbConnect(MySQL(), user="root", password="password", dbname="twitter", host="localhost")
register_db_backend(con)

score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
                    sentence <- gsub("\\\\n", " ", sentence)
                    sentence <- gsub("[^0-9A-Za-z///' ]", "", sentence)
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

  pos <- scan('positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
  neg <- scan('negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
  pos.words <- c(pos, 'upgrade')
  neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')

  #scores <- score.sentiment(text, pos.words, neg.words)
  #print(paste0(label, ": ", sum(scores$score)))

search <- c("c", "cpp", "cplusplus", "python", "java", "python", "csharp", "javascript", "dotnet", "php", "rscript", "rlang", "rstats", "matlab", "ruby", "golang", "perl", "visualbasic", "scratch", "swift")

data <- data.frame()

for (lan in search) {
  grab <- load_tweets_db(as.data.frame = TRUE, table_name = lan)
  grab[,20] <- lan
  grab <- grab[!grepl("RT ", grab$text),]
  amount <- 1000
  if(amount > dim(grab)[1]) {
    amount = dim(grab)[1]
  }
  print(paste0("Adding ", amount, " to data"))
  data <- rbind(data, grab[1:amount,])
  print(paste0(lan, " : ", dim(grab)[1]))
}

  scores = score.sentiment(data$text, pos, neg, .progress='text')
  scores$very.pos = as.numeric(scores$score >= 2)
  scores$very.neg = as.numeric(scores$score <= -2)

  # how many very positives and very negatives
  numpos = sum(scores$very.pos)
  numneg = sum(scores$very.neg)

  # global score
  global_score = round( 100 * numpos / (numpos + numneg) )

  # colors
  cols = c("#7CAE00", "#00BFC4", "#F8766D", "#C77CFF")
  names(cols) = c("beer", "coffee", "soda", "wine")

  # boxplot
  #ggplot(scores, aes(x=data, y=score, group=V20)) +
  #  geom_boxplot(aes(fill=drink)) +
  #  scale_fill_manual(values=cols) +
  #  geom_jitter(colour="gray40",
  #              position=position_jitter(width=0.2), alpha=0.3) +
  print(count(scores$very.pos))
  print(count(scores$very.neg))

readline(prompt="Enter...")

print(dim(data))

data$text <- stri_encode(data$text, "", "UTF-8") # re-mark encodings


data$text <- gsub("\\\\n", " ", data$text)
data$text <- gsub("[^0-9A-Za-z///' ]", "", data$text)

corpus <- Corpus(VectorSource(data$text), readerControl=list(reader=readPlain))
corpus <- tm_map(corpus, content_transformer(tolower))
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
corpus <- tm_map(corpus, content_transformer(removeURL))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, c(stopwords('english'), "rt", search))

corpusCopy <- corpus

corpus <- tm_map(corpus, stemDocument)
writeLines(strwrap(corpus[[190]]$content, 60))

for(i in 1:5) {
  cat(paste("[[", i, "]] ", sep=""))
  writeLines(corpus[[i]]$content)
}

print("Adding Dictionary")
stemCompletion2 <- function(x, dictionary) {
  print(paste0("Starting ulist : ", class(x)))
  x <- unlist(strsplit(as.character(x), " "))
  print(paste0("Starting not empty : ", class(x)))
  x <- x[x != ""]
  print(paste0("Starting stem Completion: ", class(x)))
  x <- stemCompletion(x, dictionary=dictionary)
  print(paste0("Starting Paste : ", class(x)))
  x <- paste(x, sep="", collapse=" ")
  print(paste0("Starting Plain Text Doc : ", class(x)))
  PlainTextDocument(stripWhitespace(x))
}
#corpus <- tm_map(corpus, stemCompletion, dictionary=corpusCopy)
ptm <- proc.time()
#corpus <- tm_map(corpus, stemCompletion2, dictionary=corpusCopy)
print(proc.time() - ptm)

# miningCases <- tm_map(corpusCopy, grep, pattern = "\\<mining")
# print(sum(unlist(miningCases)))

print("Converting to Term Document Matrix")
tdm <- TermDocumentMatrix(corpus, control = list(wordLengths=c(1, Inf)))
print(tdm)

# freq.terms <- findFreqTerms(tdm, lowfreq=15)
# term.freq <- rowSums(as.matrix(tdm))
# term.freq <- subset(term.freq, term.freq >= 15)

# df <- data.frame(term=names(term.freq), freq=term.freq)
# ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") + xlab("Terms") + ylab("Count") + coord_flip()

readline(prompt="Enter...")

m <- as.matrix(tdm)
print(dim(m))
word.freq <- sort(rowSums(m), decreasing=T)
pal <- brewer.pal(9, "BuGn")[-(1:4)]
png(filename="cloud_without.png")
wordcloud(words=names(word.freq), freq=word.freq, min.freq=3, random.order=F, colors=pal)
dev.off()

readline(prompt="Enter...")

tdm2 <- removeSparseTerms(tdm, sparse=0.95)
m2 <- as.matrix(tdm2)

distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="ward")
plot(fit)
rect.hclust(fit, k=6)

readline(prompt="Enter...")

m3 <- t(m2)
set.seed(122)
k <- 6
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits=3)
for(i in 1:k) {
  cat(paste("cluster ", i, ":   ", set = " "))
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:5], "\n")
}


library(fpc)
library(cluster)

d <- dist(t(m3), method="euclidian")   
kfit <- kmeans(d, k)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

# mat4 <- weightTfIdf(tdm)
# mat4 <- as.matrix(mat4)
# 
# norm_eucl <- function(m)
#     m/apply(m,1,function(x) sum(x^2)^.5)
# mat_norm <- norm_eucl(mat4)
# 
# set.seed(5)
# k <- 3
# kmeansResult <- kmeans(mat_norm, k)
# 
# kmeansResult$cluster[1:5]
# 
# count(kmeansResult$cluster)
# 
# result <- data.frame('actual'=data$V20, 'predicted'=kmeansResult$cluster)
# result <- result[order(result[,1]),]
# 
# result$counter <- 1
# result.agg <- aggregate(counter~actual+predicted, data=result, FUN='sum')
# 
# result.agg
# 
# ggplot(data=result.agg, aes(x=actual, y=predicted, size=counter)) + geom_point()


# for (i in 1:nrow(data)) {
#   process <- data[i,]$text 
#   # print(process)
#   process <- gsub("\\\\n", " ", process)
#   process <- gsub("[^0-9A-Za-z///' ]", "", process)
#   data[i,]$text <- process
#   #print(process)
# }


