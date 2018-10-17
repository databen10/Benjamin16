library(twitteR)
library(purrr)
library(dplyr)
require('ROAuth')
require('RCurl')
library(plyr)
library(stringr)
library(syuzhet)
library(sentiment)
library(wordcloud)
library(tm)
library(wordcloud)
library(ggplot2)
library(SnowballC)
install.packages(c("tm", "wordcloud","SnowballC"))
##SENTIMENT FUNCTION
##NEW
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
##NEW
pos.words = scan('C:/Users/wise/Desktop/positive-words.txt',what='character',comment.char=';')
neg.words = scan('C:/Users/wise/Desktop/negative-words.txt',what='character',comment.char=';')
Buhari <- score.sentiment(text5$text,pos.words,neg.words,.progress='text')
rscorescore <- score.sentiment(tweet2_df$text,pos.words,neg.words,.progress='t')
hist(Buhari$score)
hist(rscorescore$score)
consumerKey <- "iN2qMzjuw32eYw6dYA03dITHe"
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerSecret <- "2lPfaBT5RjA3cQHAiI1BQj3juer321Y0PgAbkxN6o8c5dIz6kN"
accessToken <- "897279190524981248-gy91expj0rTeNBCJsUBumjaduP7StDr"
accessTokenSecret <- "l35WtHJJTR1y4IHPyjLRHEOVnapjkv92UjG3RR3LBhCvo"
Cred <- OAuthFactory$new(consumerKey="iN2qMzjuw32eYw6dYA03dITHe",
                             consumerSecret="2lPfaBT5RjA3cQHAiI1BQj3juer321Y0PgAbkxN6o8c5dIz6kN",
                             requestURL="https://api.twitter.com/oauth/request_token",
                             accessURL= "https://api.twitter.com/access_token",
                             authURL="https://api.twitter.com/oauth/authorize")
Cred$handshake()
setup_twitter_oauth("iN2qMzjuw32eYw6dYA03dITHe","2lPfaBT5RjA3cQHAiI1BQj3juer321Y0PgAbkxN6o8c5dIz6kN","897279190524981248-gy91expj0rTeNBCJsUBumjaduP7StDr","l35WtHJJTR1y4IHPyjLRHEOVnapjkv92UjG3RR3LBhCvo")
tweet1 <- userTimeline("@chelseafc",n=100)
tweet2 <- userTimeline("@manutd",n=100)
tweet_df <- tbl_ df(map_df(tweet1,as.data.frame))
tweet2_df <- tbl_df(map_df(tweet2,as.data.frame))
library(sentiment)
buhari<-searchTwitter("Eleka2018", n=200)
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
Cred$handshake(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl'))  
ConsumerKey:F7zwM2YRfFUjuffCsaPBYbi9E
ConsumerSecret:BBLuqPuRzdRlvudmAkCzxKvggv4SV18qeItR9FoMl3YmeGgjbr
AccessToken:897279190524981248-xLVUr02X5lUK05q4yn2QWL4O3fryhx2
AccessTokenSecret:srZ4VvgSn4aX8kRRH4hhbLIt0NL79UmoeDYuvsyH2WxJA
download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='cacert.pem')
Cred$handshake(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl'))
save ("Cred, file = 'twitter authentication.RData")
Trump_search = searchTwitter("trump", n =2000000, since = "2015-10-01", until = "2016-10-31", lang = "en")
rt <- search_tweets("DonaldTrump", n = 100, retryonratelimit = TRUE, retweets = FALSE, lang = "en")
search_tweets(DonaldTrump, n = 100, type = "recent", include_rts = FALSE, parse = TRUE, usr = TRUE, token = NULL,
              retryonratelimit = , verbose = TRUE, lang = "en")
djt <- get_timeline("realDonaldTrump", n = 50)
lookup_statuses(realDonaldTrump, token = NULL, parse = TRUE, usr = TRUE,
                clean_tweets = FALSE, as_double = FALSE)
tweets<-twListToDF(searchTwitter("#realDonaldTrump", n=100)
ctweet_test = searchTwitter("#HillaryClinton", n=100)
ttweet_test = searchTwitter("#realDonaldTrump", n=100)
length(ctweet_test)
tweets_text.df = laply(ctweet_test, function(t) t$getText())
analysis = score.sentiment(tweets.text, pos.words, neg.words)
table(analysis$score)
write.csv(tweets.text, file = "ttweet_test.csv")
#######as.numeric##########################
we <- gsub(",", "", we)   # remove comma
we <- as.numeric(we)  
#######tweets cleaning#############
some_txt1= gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",df)
####################remove html link####################
some_txt2= gsub("http[^[:blank:]]+","",some_txt1)
some_txt3= gsub("@\\wt","",some_txt2)
############Remove Punctuation#########################
some_txt4= gsub("[[:punct:]]","",some_txt3)
##########Remove Punctuations#########################
some_txt5= gsub("[^[:alnum:]]", "", some_txt4)
some_txt6 <- Corpus(VectorSource(some_txt5))
some_txt6 <- tm_map(some_txt6, removePunctuation)
some_txt6 <- tm_map(some_txt6, content_transformer(tolower))
some_txt6 <- tm_map(some_txt6, removeWords, stopwords("english"))
some_txt6 <- tm_map(some_txt6, stripWhitespace)
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL

write.csv(clintontweet.df,"tweet4.csv")
write.csv(some_txt5, "tweetc.csv")
hilary_tweet<-sapply(ctweet_test, function(x) x$getText())
pal<-brewer.pal(8,"Dark2")
wordcloud(some_txt6, min.freq = 5, max.words = Inf, width = 1000, heigth = 1000, random.order = FALSE, color = pal)
get_nrc_sentiment("i met a girl today she's so fucking hot but she's not that tall, i like her though")
mysentiment <- get_nrc_sentiment(some_txt5)
SentimentScores<-data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores<-cbind("sentiment", rownames(SentimentScores), SentimentScores)
rownames(SentimentScores)<-NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) + geom_bar(aes(fill = sentiment), stat = "identity") + theme(legend.position = "none") + xlab("Sentiment"), ylab("Score") + ggtitle("tweets on Sentiment Analysis")
mat = create_matrix(tweet_all, language = "english", removeStopwords = FALSE, removeNumbers = TRUE, stemWords = FALSE, tm::weightTfIdf)
####################R SOURCE CODE##################################
trump = readLines("./ttweet1.txt")
hillary = readLines("./ctweet1.txt")
trump_test = readLines("./ttweet_test.txt")
hillary_test = readLines("./ctweet_test.txt")
tweet = c(trump, hillary)
tweet_test = c(trump_test, hillary_test)
tweet_all = c(tweet, tweet_test)
sentiment = c(rep("trump", length(trump)), rep("hillary", length(hillary)))
sentiment_test = c(rep("trump", length(trump_test)), rep("hillary", length(hillary_test)))
sentiment_all = as.factor(c(sentiment, sentiment_test))
library(RTextTools)
library(e1071)
mat = create_matrix(tweet_all, language = "english", removeStopwords = FALSE, 
                    removeNumbers = TRUE, stemWords = FALSE, tm::weightTfIdf)
mat = as.matrix(mat)
classifier = naiveBayes(mat[1:10, ], as.factor(sentiment_all[1:10]))
predicted = predict(classifier, mat[11:20, ])
predicted
table(sentiment_test, predicted)
recall_accuracy(sentiment_test, predicted)

