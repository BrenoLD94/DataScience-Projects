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

# FUNCTIONS
CleanText <- function(text){
  # removing @username
  text <- gsub("@\\w+", "", text)
  
  # converting the text to lowerCase
  text <- tolower(text)
  
  # remove #word
  text <- gsub("#\\w+", "", text)
  
  # remove links
  text <- gsub("http\\w+", "", text)
  
  # removing punctuation
  text <- gsub("[[:punct:]]", "", text)
  
  # removing \n character
  text <- gsub("\\n", "", text)
  
  # removing numbers
  text <- gsub("[0-9]*", "", text)
  
  text <- gsub("^(RT|via)", "", text)
  
  return(text)
}

cleanTweet <- function(text, language='en'){
  corpus <- VCorpus(VectorSource(text))
  
  corpus <- tm_map(corpus, removeWords, stopwords(language))
  
  # remove synonymous of words, for example, loving -> love
  corpus <- tm_map(corpus, stemDocument)
  
  return(corpus)
}  

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

# keys to acess API's twitter
consumer_key = ""
consumer_secret_key = ""
acess_token = ""
acesss_secret_token = ""

# creating conection with twitter session
setup_twitter_oauth(consumer_key, consumer_secret_key, acess_token, acesss_secret_token)

hashtag <- "#trump"

# search for twitters
tweets <- searchTwitter(hashtag, n=100, since='2019-01-01', lang='en')

# creating a dataframe
df <- do.call("rbind", lapply(tweets, as.data.frame))

# write.csv(df, "Tweets.csv")

# using a auxiliary variable to transform the text
textList <- df$text

# cleaning up the text
textList <- CleanText(textList)

textList <- iconv(textList, to = "utf-8", sub="")

# REMOVING STOP WORDS
corpus <- cleanTweet(textList)

library(RColorBrewer)
pals <- brewer.pal(8,"Dark2")

wordcloud(corpus, 
          max.words = 50, 
          min.freq = 3, 
          scale= c(2, 1), 
          random.order = FALSE,
          random.color = FALSE,
          colors=pals)

textList <- sapply(corpus, as.character)

# understanding the data
docText <- TermDocumentMatrix(corpus)

findFreqTerms(docText, lowfreq = 11)

findAssocs(docText, 'kid', 0.60)

# applying sentiment analysis
sent <- sentiment(textList)

summary(sent$sentiment)
sd(sent$sentiment)

df_sentiment <- as.data.frame(sent)
df_sentiment$element_id <- NULL
df_sentiment$sentence_id <- NULL
df_sentiment['text'] <- textList
df_sentiment$dateCreate <- df$created
df_sentiment$polarity <- unlist(lapply(sent$sentiment, polarityLevel))

# using a histogram to see the results
graph <- ggplot(df_sentiment, aes(x=polarity, fill= as.factor(polarity))) +
              geom_histogram(stat = 'count')

graph <- graph +
          geom_vline(aes(xintercept=mean(sentiment)),
                         color="black", linetype="dashed", size=1)

graph <- graph +
          scale_fill_manual(values=c("firebrick2", "darkgrey", "deepskyblue4")) + 
            labs(title=paste("sentiment analysis graph with the subject: ", hashtag)) +
              theme(plot.title = element_text(color = "black", size = 12, face = "italic", hjust = 0.5))
            
graph
