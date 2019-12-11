setwd('./')

# loading libraries
library(twitteR)
library(plyr)
library(stringr)
library(tm)
library(wordcloud)
library(sentimentr)
library(ggplot2)
library(RMySQL)

# keys to acess API's twitter
consumer_key = "xxx"
consumer_secret_key = "xxx"
acess_token = "xxx"
acesss_secret_token = "xxx"

# creating conection with twitter session
setup_twitter_oauth(consumer_key, consumer_secret_key, acess_token, acesss_secret_token)

hashtag <- "#nba"

# search for twitters
tweets <- searchTwitter(hashtag, n=300, since='2019-08-08', lang='en')

# creating a dataframe
df <- do.call("rbind", lapply(tweets, as.data.frame))

# using a auxiliary variable to transform the text
textList <- df$text

# cleaning up the text

# removing @username
textList <- gsub("@\\w+", "", textList)

# converting the text to lowerCase
textList <- tolower(textList)

# remove #word
textList <- gsub("#\\w+", "", textList)

# remove links
textList <- gsub("http\\w+", "", textList)

# removing punctuation
textList <- gsub("[[:punct:]]", "", textList)

# removing \n character
textList <- gsub("\\n", "", textList)

# removing numbers
textList <- gsub("[0-9]*", "", textList)

# REMOVING STOP WORDS
corpus <- VCorpus(VectorSource(textList))

corpus <- tm_map(corpus, removeWords, stopwords('en'))

# remove synonymous of words, for example, loving -> love
corpus <- tm_map(corpus, stemDocument)

wordcloud(corpus, max.words =100, min.freq=5, scale=c(1,1), 
          random.order = FALSE, colors=palette())

textList <- sapply(corpus, as.character)

# apllying sentiment analysis
sent <- sentiment(textList)

polarityLevel <- function(x){
  if(x < 0){
    return("Negative")
  }
  else if(x == 0){
    return("Neutral")
  }
  else{
    return("Positive")
  }
}

summary(sent$sentiment)
sd(sent$sentiment)

df_sentiment <- as.data.frame(sent)
df_sentiment$element_id <- NULL
df_sentiment$sentence_id <- NULL
df_sentiment['text'] <- textList
df_sentiment$dateCreate <- df$created
df_sentiment$polarity <- unlist(lapply(sent$sentiment, polarityLevel))


df_database <- df_sentiment[, c('text', "dateCreate", "sentiment")]

# connecting with localhost
connect <- dbConnect(MySQL(), user='root', password='@lf@ch@rlie94', dbname='TWEETS', host='localhost')

# inserting the tweets in database
for(i in 1:length(df_database$text)){
  query1 <- paste("INSERT INTO TWEET(TEXTO, DATA_TWEET, score_sentimento) VALUES ('", df_database[i, 'text']) 
  query2 <- paste("', '", toString(df_database[i, 'dateCreate']))
  query3 <- paste(query2, "',")
  query3 <- paste(query3, toString(df_database[i, 'sentiment']))
  query3 <- paste(query3, ")")
  query4 <- paste(query1, query3)
 
  temp <- dbSendStatement(connect, query4)
  dbClearResult(temp)
}

# close connection
dbDisconnect(connect)

# using a histogram to see the results
graph <- ggplot(df_sentiment, aes(x=sentiment, fill= as.factor(polarity))) +
              geom_histogram(bins =  30)

graph <- graph +
          geom_vline(aes(xintercept=mean(sentiment)),
                         color="black", linetype="dashed", size=1)

graph <- graph +
          scale_fill_manual(values=c("firebrick2", "darkgrey", "deepskyblue4")) + 
            labs(title=paste("sentiment analysis graph with the subject: ", hashtag)) +
              theme(plot.title = element_text(color = "black", size = 12, face = "italic", hjust = 0.5))
            
graph

# temporal series
temporalSerieGraph <- ggplot(df_sentiment, aes(x=dateCreate, y=sentiment)) +
                      geom_line() +
                      labs(title=paste("sentiment analysis using temporal series with the subject: ", hashtag), x="Time", y="Sentiment_score") +
                      theme(plot.title = element_text(color = "black", size = 12, face = "italic", hjust = 0.5))
                
temporalSerieGraph


